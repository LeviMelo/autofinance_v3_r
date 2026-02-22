# /data_engine/R/05_corp_actions.R

de_select_ca_candidates <- function(universe_raw, cfg, force_symbols = NULL) {
  de_require("data.table")
  
  # Work on a projected copy to avoid mutating/reordering upstream universe_raw
  dt <- data.table::copy(
    data.table::as.data.table(universe_raw)[, .(symbol, asset_type, refdate, turnover, close)]
  )
  
  if (!nrow(dt)) {
    force_symbols <- unique(toupper(trimws(as.character(force_symbols))))
    force_symbols <- force_symbols[!is.na(force_symbols) & nzchar(force_symbols)]
    audit_dt <- data.table::data.table(symbol = force_symbols)
    audit_dt[, `:=`(
      is_candidate = TRUE,
      is_forced = TRUE,
      has_positive_jump = FALSE,
      has_negative_gap = FALSE
    )]
    return(list(candidates = force_symbols, prefilter_audit = audit_dt))
  }
  
  end_date <- max(dt$refdate, na.rm = TRUE)
  liq_start <- end_date - ceiling(cfg$ca_prefilter_liq_window_days * 1.6)
  
  dt_liq <- dt[refdate >= liq_start & refdate <= end_date]
  total_market_days <- length(unique(dt_liq$refdate))
  if (total_market_days == 0) total_market_days <- 1L
  
  stats_liq <- dt_liq[, .(
    med_turnover = stats::median(turnover, na.rm = TRUE),
    days_ratio = .N / total_market_days
  ), by = .(symbol, asset_type)]
  
  candidates <- stats_liq[
    med_turnover >= cfg$min_turnover & days_ratio >= cfg$min_days_traded_ratio,
    unique(symbol)
  ]
  
  force_symbols <- unique(toupper(trimws(as.character(force_symbols))))
  force_symbols <- force_symbols[!is.na(force_symbols) & nzchar(force_symbols)]
  candidates <- unique(c(candidates, force_symbols))
  
  # Diagnostic prefilter audit (gap tracking)
  data.table::setorder(dt, symbol, refdate)
  dt[, close_lag := data.table::shift(close, 1L), by = symbol]
  dt[, log_ret := data.table::fifelse(close > 0 & close_lag > 0, log(close / close_lag), NA_real_)]
  dt[, ret_1d := (close / close_lag) - 1]
  
  thr_map <- data.table::data.table(
    asset_type = c("equity", "fii", "etf", "bdr"),
    thr = c(cfg$ca_prefilter_gap_equity, cfg$ca_prefilter_gap_fii, cfg$ca_prefilter_gap_etf, cfg$ca_prefilter_gap_bdr)
  )
  dt <- merge(dt, thr_map, by = "asset_type", all.x = TRUE, sort = FALSE)
  
  jump_pos <- dt[is.finite(log_ret) & log_ret >= cfg$ca_prefilter_jump_log_thr, unique(symbol)]
  jump_neg <- dt[is.finite(ret_1d) & is.finite(thr) & ret_1d <= thr, unique(symbol)]
  
  all_audit_syms <- unique(c(dt$symbol, force_symbols))
  audit_dt <- data.table::data.table(symbol = all_audit_syms)
  audit_dt[, is_candidate := symbol %in% candidates]
  audit_dt[, is_forced := symbol %in% force_symbols]
  audit_dt[, has_positive_jump := symbol %in% jump_pos]
  audit_dt[, has_negative_gap := symbol %in% jump_neg]
  
  data.table::setorder(audit_dt, symbol)
  
  list(
    candidates = unique(candidates[!is.na(candidates) & nzchar(candidates)]),
    prefilter_audit = audit_dt
  )
}

de_build_ca_registry <- function(symbols, from, to, cfg) {
  de_require("data.table")
  
  empty_ca_tbl <- function() {
    data.table::data.table(
      symbol = character(),
      yahoo_symbol = character(),
      refdate = as.Date(character()),
      action_type = character(),
      value = numeric(),
      source = character()
    )
  }
  
  required_cols <- de_contracts$corp_actions_raw
  
  normalize_ca_tbl <- function(x) {
    x <- data.table::as.data.table(x)
    de_validate_contract(x, "corp_actions_raw")
    # Drop extra columns from stale caches (e.g., accidental row_id leakage)
    x <- x[, ..required_cols]
    data.table::setcolorder(x, required_cols)
    x
  }
  
  # Sanitize symbols
  symbols <- unique(toupper(trimws(as.character(symbols))))
  symbols <- symbols[!is.na(symbols) & nzchar(symbols)]
  if (!length(symbols)) return(empty_ca_tbl())
  
  if (cfg$ca_cache_mode %in% c("batch", "by_symbol")) {
    de_require("digest")
    hash_payload <- list(syms = sort(symbols), from = as.Date(from), to = as.Date(to), mode = cfg$ca_fetch_mode)
    batch_hash <- digest::digest(hash_payload)
  }
  
  cache_dir_ca <- file.path(cfg$cache_dir, "corp_actions")
  if (!dir.exists(cache_dir_ca)) dir.create(cache_dir_ca, recursive = TRUE)
  
  # Batch cache check (validate before returning)
  if (cfg$ca_cache_mode == "batch") {
    batch_file <- file.path(cache_dir_ca, paste0("ca_batch_", batch_hash, ".rds"))
    if (file.exists(batch_file)) {
      cached <- tryCatch(readRDS(batch_file), error = function(e) e)
      if (!inherits(cached, "error")) {
        norm <- tryCatch(normalize_ca_tbl(cached), error = function(e) e)
        if (!inherits(norm, "error")) {
          de_log("DE_CA:", "Loading from validated CA batch cache.")
          return(norm)
        } else {
          de_log("DE_CA:", "Batch cache invalid/corrupt. Rebuilding.")
        }
      } else {
        de_log("DE_CA:", "Batch cache unreadable. Rebuilding.")
      }
    }
  }
  
  map_dt <- data.table::data.table(symbol = symbols, yahoo_symbol = vapply(symbols, de_yahoo_symbol, ""))
  map_dt <- map_dt[!is.na(yahoo_symbol) & nzchar(yahoo_symbol)]
  res_list <- vector("list", nrow(map_dt))
  
  for (i in seq_len(nrow(map_dt))) {
    sym <- map_dt$symbol[i]
    ysym <- map_dt$yahoo_symbol[i]
    
    if (cfg$ca_cache_mode == "by_symbol") {
      sym_hash <- digest::digest(list(sym = sym, from = as.Date(from), to = as.Date(to), mode = cfg$ca_fetch_mode))
      sym_file <- file.path(cache_dir_ca, paste0("sym_", sym, "_", sym_hash, ".rds"))
      
      if (file.exists(sym_file)) {
        cached <- tryCatch(readRDS(sym_file), error = function(e) e)
        if (!inherits(cached, "error")) {
          norm <- tryCatch(normalize_ca_tbl(cached), error = function(e) e)
          if (!inherits(norm, "error")) {
            res_list[[i]] <- norm
            next
          } else {
            de_log("DE_CA:", "Invalid symbol cache for ", sym, "; refetching.")
          }
        } else {
          de_log("DE_CA:", "Unreadable symbol cache for ", sym, "; refetching.")
        }
      }
    }
    
    # Fetch vendor data
    if (cfg$ca_fetch_mode == "chart") {
      out <- de_yahoo_fetch_chart_events_one(ysym, from, to)
    } else {
      dt_s <- de_yahoo_fetch_splits_quantmod_one(ysym, from, to)
      dt_d <- de_yahoo_fetch_dividends_quantmod_one(ysym, from, to)
      out <- data.table::rbindlist(list(dt_s, dt_d), fill = TRUE)
    }
    
    # Safe empty caching (0-row typed schema)
    if (is.null(out) || !nrow(out)) {
      out <- empty_ca_tbl()
      attr(out, "cached_empty_symbol") <- sym
      attr(out, "cached_empty_yahoo_symbol") <- ysym
    } else {
      out[, symbol := sym]
      out[, source := "yahoo"]
      out <- out[, ..required_cols]
      out <- normalize_ca_tbl(out)
    }
    
    if (cfg$ca_cache_mode == "by_symbol") saveRDS(out, sym_file)
    res_list[[i]] <- out
  }
  
  dt <- data.table::rbindlist(res_list, fill = TRUE)
  if (!nrow(dt)) dt <- empty_ca_tbl()
  dt <- normalize_ca_tbl(dt)
  
  # Save batch cache (even empty, to prevent refetch storms)
  if (cfg$ca_cache_mode == "batch") {
    batch_file <- file.path(cache_dir_ca, paste0("ca_batch_", batch_hash, ".rds"))
    saveRDS(dt, batch_file)
  }
  
  dt
}

de_fix_yahoo_splits_by_raw_gap <- function(corp_actions_raw, universe_raw, cfg) {
  sp0 <- data.table::as.data.table(corp_actions_raw)
  if (!nrow(sp0)) {
    return(list(
      corp_actions_apply = sp0,
      split_audit = data.table::data.table(),
      quarantine = data.table::data.table()
    ))
  }
  
  # Work on projected copies to avoid mutating caller objects (row_id / close_lag leakage)
  sp <- data.table::copy(sp0[, .(symbol, yahoo_symbol, refdate, action_type, value, source)])
  dt <- data.table::copy(data.table::as.data.table(universe_raw)[, .(symbol, refdate, open, close)])
  
  sp[, row_id := .I]
  to_val <- sp[action_type == "split" & source == "yahoo" & is.finite(value) & value > 0]
  if (!nrow(to_val)) return(list(corp_actions_apply = sp, split_audit = data.table::data.table(), quarantine = data.table::data.table()))
  
  data.table::setorder(dt, symbol, refdate)
  dt[, close_lag := data.table::shift(close, 1L), by = symbol]
  data.table::setkey(dt, symbol, refdate)
  
  offs <- seq.int(-cfg$split_gap_max_back_days, cfg$split_gap_max_forward_days)
  
  eval_one <- function(sym, vdate, vval) {
    best <- list(err = Inf, eff = as.Date(NA), chosen = NA_real_)
    for (o in offs) {
      d <- vdate + o
      row <- dt[.(sym, d), nomatch = 0L]
      if (nrow(row) != 1L || !is.finite(row$close_lag[[1]]) || row$close_lag[[1]] <= 0) next
      
      obs <- NA_real_
      if (cfg$split_gap_use_open && is.finite(row$open[[1]]) && row$open[[1]] > 0) {
        obs <- row$open[[1]] / row$close_lag[[1]]
      } else if (is.finite(row$close[[1]]) && row$close[[1]] > 0) {
        obs <- row$close[[1]] / row$close_lag[[1]]
      } else next
      
      for (cv in c(vval, 1/vval)) {
        if (!is.finite(cv) || cv <= 0) next
        e <- abs(log(obs) - log(cv))
        if (is.finite(e) && e < best$err) {
          best$err <- e; best$eff <- d; best$chosen <- cv
        }
      }
    }
    best
  }
  
  out <- to_val[, {
    b <- eval_one(symbol, refdate, value)
    st <- if (!is.finite(b$err) || is.na(b$eff)) "unverified" else if (b$err <= cfg$split_gap_tol_log) "kept" else "rejected"
    .(eff_refdate = b$eff, chosen_value = b$chosen, chosen_err = b$err, status = st)
  }, by = .(row_id, symbol, yahoo_symbol, vendor_refdate = refdate, action_type, value, source)]
  
  data.table::setorder(out, symbol, eff_refdate, chosen_value, chosen_err)
  out[, dup_rank := seq_len(.N), by = .(symbol, eff_refdate, chosen_value)]
  out[dup_rank > 1L & status == "kept", status := "dup"]
  
  kept <- out[status %in% c("kept", "unverified"), .(symbol, yahoo_symbol, refdate = data.table::fifelse(status=="kept", eff_refdate, vendor_refdate), action_type = "split", value = data.table::fifelse(status=="kept", chosen_value, value), source)]
  base_ca <- sp[!(action_type == "split" & source == "yahoo"), .(symbol, yahoo_symbol, refdate, action_type, value, source)]
  
  ca_apply <- data.table::rbindlist(list(base_ca, kept), fill = TRUE)
  ca_apply <- unique(ca_apply, by = c("symbol", "refdate", "action_type", "value", "source"))
  quar <- out[status %in% c("rejected", "dup")]
  
  list(corp_actions_apply = ca_apply, split_audit = out, quarantine = quar)
}

de_build_events <- function(corp_actions_apply, manual_events = NULL, cfg) {
  dt <- data.table::as.data.table(corp_actions_apply)
  
  if (cfg$enable_manual_events && !is.null(manual_events)) {
    me <- data.table::as.data.table(manual_events)
    if (!"source" %in% names(me)) me[, source := "manual"]
    dt <- data.table::rbindlist(list(dt, me), fill = TRUE)
  }
  
  if (!nrow(dt)) return(data.table::data.table(symbol=character(), refdate=as.Date(character()), split_value=numeric(), div_cash=numeric(), source_mask=character(), has_manual=logical()))
  
  dt <- unique(dt, by = c("symbol", "refdate", "action_type", "value", "source"))
  
  sp <- dt[action_type == "split", .(split_value = prod(value, na.rm=TRUE), mask_s = paste(sort(unique(source)), collapse="+")), by=.(symbol, refdate)]
  di <- dt[action_type == "dividend", .(div_cash = sum(value, na.rm=TRUE), mask_d = paste(sort(unique(source)), collapse="+")), by=.(symbol, refdate)]
  
  ev <- merge(sp, di, by=c("symbol", "refdate"), all = TRUE)
  ev[is.na(split_value), split_value := 1]
  ev[is.na(div_cash), div_cash := 0]
  ev[, source_mask := data.table::fifelse(!is.na(mask_s) & !is.na(mask_d), paste0(mask_s, "+", mask_d), data.table::fifelse(!is.na(mask_s), mask_s, mask_d))]
  ev[, has_manual := grepl("manual", source_mask)]
  ev[, c("mask_s", "mask_d") := NULL]
  
  ev <- ev[is.finite(split_value) & split_value > 0 & is.finite(div_cash) & div_cash >= 0]
  de_validate_contract(ev, "events_apply")
  ev[order(symbol, refdate)]
}