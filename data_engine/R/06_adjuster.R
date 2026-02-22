# /data_engine/R/06_adjuster.R

de_rev_cumprod_exclusive <- function(x) {
  if (!length(x)) return(x)
  data.table::shift(rev(cumprod(rev(x))), 1L, type = "lead", fill = 1)
}

de_apply_adjustments <- function(universe_raw, events_apply, cfg) {
  dt <- data.table::as.data.table(universe_raw)
  ev <- data.table::as.data.table(events_apply)
  
  data.table::setkey(dt, symbol, refdate)
  if (nrow(ev)) data.table::setkey(ev, symbol, refdate)
  
  if (nrow(ev)) dt <- merge(dt, ev, by=c("symbol", "refdate"), all.x=TRUE)
  if (!"split_value" %in% names(dt)) dt[, split_value := 1]
  if (!"div_cash" %in% names(dt)) dt[, div_cash := 0]
  if (!"source_mask" %in% names(dt)) dt[, source_mask := "none"]
  if (!"has_manual" %in% names(dt)) dt[, has_manual := FALSE]

  dt[is.na(split_value), split_value := 1]
  dt[is.na(div_cash), div_cash := 0]
  dt[is.na(source_mask) | !nzchar(source_mask), source_mask := "none"]
  dt[is.na(has_manual), has_manual := FALSE]
  
  data.table::setorder(dt, symbol, refdate)
  
  # 1) Split Factor
  dt[, split_factor_cum := de_rev_cumprod_exclusive(split_value), by = symbol]
  dt[, `:=`(
    open_adj_split = open * split_factor_cum,
    high_adj_split = high * split_factor_cum,
    low_adj_split  = low  * split_factor_cum,
    close_adj_split= close* split_factor_cum
  )]
  
  # 2) Dividend Factor
  dt[, close_prev := data.table::shift(close_adj_split, 1L, type = "lag"), by = symbol]
  dt[, `:=`(div_factor_event = 1, issue_div = FALSE, div_cash_eff = div_cash)]
  
  # Dividend Rescue (Basis Mismatch)
  is_div <- dt$div_cash > 0
  need_scale <- is_div & is.finite(dt$close_prev) & dt$close_prev > 0 & 
                is.finite(dt$split_factor_cum) & dt$split_factor_cum > 0 & dt$split_factor_cum < 1 & 
                dt$div_cash >= dt$close_prev
  
  if (any(need_scale)) {
    idx <- which(need_scale)
    scaled <- dt$div_cash[idx] * dt$split_factor_cum[idx]
    ok <- is.finite(scaled) & scaled >= 0 & scaled < dt$close_prev[idx]
    if (any(ok)) dt[idx[ok], div_cash_eff := scaled[ok]]
  }
  
  # Apply Div
  # Track boundary dividend rows separately (first visible row in truncated window)
  dt[, row_in_symbol := seq_len(.N), by = symbol]
  dt[, issue_div_boundary := FALSE]
  
  boundary_div <- is_div & dt$row_in_symbol == 1L & is.na(dt$close_prev)
  if (any(boundary_div)) dt[boundary_div, issue_div_boundary := TRUE]
  
  bad_div <- is_div & !boundary_div & (
    is.na(dt$close_prev) | dt$close_prev <= 0 |
    is.na(dt$div_cash_eff) | dt$div_cash_eff < 0 |
    dt$div_cash_eff >= dt$close_prev
  )
  if (any(bad_div)) dt[bad_div, issue_div := TRUE]
  
  good_div <- is_div & !boundary_div & !bad_div
  if (any(good_div)) dt[good_div, div_factor_event := (close_prev - div_cash_eff) / close_prev]
    
  dt[, div_factor_cum := de_rev_cumprod_exclusive(div_factor_event), by = symbol]
  dt[, adj_factor_final := split_factor_cum * div_factor_cum]
  
  dt[, `:=`(
    open_adj_final  = open * adj_factor_final,
    high_adj_final  = high * adj_factor_final,
    low_adj_final   = low  * adj_factor_final,
    close_adj_final = close* adj_factor_final
  )]
  
  # Adjustments Timeline Audit
  tl <- dt[, .(
    symbol, refdate, split_value, div_cash, div_cash_eff,
    split_factor_cum, div_factor_event, div_factor_cum, adj_factor_final,
    source_mask, has_manual, issue_div, issue_div_boundary
  )]
  dt[, row_in_symbol := NULL]
  
  list(panel = dt, adjustments_timeline = tl)
}

de_build_panel_adjusted <- function(universe_raw, events_apply, cfg) {
  res <- de_apply_adjustments(universe_raw, events_apply, cfg)
  dt <- res$panel
  tl <- res$adjustments_timeline
  
  # State Calculation
  st <- tl[, .(
    has_split = any(split_value != 1),
    has_div = any(div_cash > 0),
    has_manual = any(has_manual),
    issue_div_any = any(issue_div)
  ), by = symbol]

  st[, adjustment_state := "no_actions"]
  st[has_div & !has_split, adjustment_state := "dividend_only"]
  st[has_split & !has_div, adjustment_state := "split_only"]
  st[has_split & has_div, adjustment_state := "split_dividend"]
  st[has_manual == TRUE, adjustment_state := "manual_override"]

  # Jump Safety Net
  jump_audit <- dt[is.finite(close_adj_final) & close_adj_final > 0, {
    v <- abs(diff(log(close_adj_final)))
    if (!length(v) || all(!is.finite(v))) {
      .(residual_max_abs_logret = 0, residual_jump_date = as.Date(NA))
    } else {
      .(residual_max_abs_logret = as.numeric(v[which.max(v)]),
        residual_jump_date = refdate[which.max(v) + 1L])
    }
  }, by = symbol]

  jump_audit[, residual_jump_flag := is.finite(residual_max_abs_logret) &
                                   residual_max_abs_logret >= cfg$adj_residual_jump_tol_log]

  st <- merge(st, jump_audit, by = "symbol", all.x = TRUE)

  # Cause-specific suspect labels (more informative than a single bucket)
  st[issue_div_any == TRUE & !residual_jump_flag, adjustment_state := "suspect_dividend_factor"]
  st[issue_div_any != TRUE & residual_jump_flag == TRUE, adjustment_state := "suspect_residual_jump"]
  st[issue_div_any == TRUE & residual_jump_flag == TRUE, adjustment_state := "suspect_both"]
  
  dt[st, on = "symbol", adjustment_state := i.adjustment_state]
  
  # Build Debug Panel
  panel_debug <- data.table::copy(dt)
  data.table::setnames(panel_debug, c("open","high","low","close"), c("open_raw","high_raw","low_raw","close_raw"))
  data.table::setnames(panel_debug, c("open_adj_final","high_adj_final","low_adj_final","close_adj_final"), c("open","high","low","close"))
  
  # Canonical Output
  panel_model <- panel_debug[, .(symbol, refdate, asset_type, open, high, low, close, turnover, qty, adjustment_state)]
  
  de_validate_contract(panel_model, "panel_adj_model")
  de_validate_contract(panel_debug, "panel_adj_debug")
  
  list(panel_adj_model = panel_model, panel_adj_debug = panel_debug, adjustments_timeline = tl, residual_jump_audit = jump_audit)
}