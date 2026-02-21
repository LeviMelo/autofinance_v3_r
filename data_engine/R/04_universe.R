# /data_engine/R/04_universe.R

de_select_min_cols <- function(df) {
  de_require(c("data.table", "dplyr"))
  dt <- data.table::as.data.table(df)
  pick_col <- function(alts) { cand <- intersect(alts, names(dt)); if(length(cand)) cand[1] else NA_character_ }
  
  c_sym  <- pick_col(c("symbol", "ticker"))
  c_ref  <- pick_col(c("refdate", "date"))
  c_open <- pick_col(c("open", "price.open", "preco_abertura"))
  c_high <- pick_col(c("high", "price.high", "preco_maximo"))
  c_low  <- pick_col(c("low", "price.low", "preco_minimo"))
  c_close<- pick_col(c("close", "price.close", "preco_ultimo"))
  c_vol  <- pick_col(c("financial_volume", "volume", "vol_fin"))
  c_qty  <- pick_col(c("trade_quantity", "quantity", "qty"))
  
  if (any(is.na(c(c_sym, c_ref, c_close)))) stop("Missing core B3 columns in rb3 output.")
  
  data.table::data.table(
    symbol = toupper(trimws(as.character(dt[[c_sym]]))),
    refdate = as.Date(dt[[c_ref]]),
    open = if(!is.na(c_open)) as.numeric(dt[[c_open]]) else NA_real_,
    high = if(!is.na(c_high)) as.numeric(dt[[c_high]]) else NA_real_,
    low  = if(!is.na(c_low))  as.numeric(dt[[c_low]])  else NA_real_,
    close= as.numeric(dt[[c_close]]),
    vol_fin = if(!is.na(c_vol)) as.numeric(dt[[c_vol]]) else NA_real_,
    qty_raw = if(!is.na(c_qty)) as.numeric(dt[[c_qty]]) else NA_real_
  )
}

de_unify_liquidity <- function(dt) {
  dt[, qty := qty_raw]
  dt[, turnover := data.table::fifelse(
    is.finite(vol_fin) & vol_fin > 0, vol_fin,
    data.table::fifelse(is.finite(qty) & qty > 0 & is.finite(close), qty * close, NA_real_)
  )]
  dt[, c("vol_fin", "qty_raw") := NULL]
  dt
}

de_dedupe_symbol_date <- function(dt) {
  data.table::setorder(dt, asset_type, symbol, refdate)
  dup <- dt[, .N, by = .(symbol, refdate)][N > 1L]
  if (nrow(dup)) {
    if (!"turnover" %in% names(dt)) dt[, turnover := NA_real_]
    if (!"qty" %in% names(dt)) dt[, qty := NA_real_]
    dt <- dt[order(
      symbol, refdate,
      -data.table::fifelse(is.finite(turnover), turnover, -Inf),
      -data.table::fifelse(is.finite(qty), qty, -Inf),
      -data.table::fifelse(is.finite(close), close, -Inf)
    )][, .SD[1L], by = .(symbol, refdate)]
    data.table::setorder(dt, asset_type, symbol, refdate)
  }
  dt
}

de_process_rb3_lazy_df <- function(df_lazy, include_types) {
  out_list <- list()
  for (tp in include_types) {
    lazy_tp <- switch(tp,
      equity = tryCatch(rb3::cotahist_filter_equity(df_lazy), error=function(e) NULL),
      fii    = tryCatch(rb3::cotahist_filter_fii(df_lazy), error=function(e) NULL),
      etf    = tryCatch(rb3::cotahist_filter_etf(df_lazy), error=function(e) NULL),
      bdr    = tryCatch(rb3::cotahist_filter_bdr(df_lazy), error=function(e) NULL)
    )
    if (!is.null(lazy_tp)) {
      df_tp <- dplyr::collect(lazy_tp)
      if (nrow(df_tp)) {
        dt_tp <- de_select_min_cols(df_tp)
        dt_tp <- de_unify_liquidity(dt_tp)
        dt_tp[, asset_type := tp]
        out_list[[tp]] <- dt_tp
      }
    }
  }
  if (!length(out_list)) return(NULL)
  dt <- data.table::rbindlist(out_list, fill = TRUE)
  dt[!is.na(symbol) & nzchar(symbol) & !is.na(refdate) & is.finite(close) & close > 0]
}

de_build_universe_year <- function(year, include_types, cfg) {
  de_require("dplyr")
  df_lazy <- de_rb3_fetch_yearly_lazy(year, cfg)
  de_process_rb3_lazy_df(df_lazy, include_types)
}

# NEW: Missing window implementation
de_build_universe_window <- function(start_date, end_date, cfg) {
  de_require("dplyr")
  df_lazy <- de_rb3_fetch_window_lazy(start_date, end_date, cfg)
  dt <- de_process_rb3_lazy_df(df_lazy, cfg$include_types)
  if (is.null(dt)) stop("Universe window is empty.")
  
  dt <- de_dedupe_symbol_date(dt)
  de_validate_contract(dt, "universe_raw")
  dt
}

de_build_universe <- function(years, cfg) {
  res <- lapply(sort(unique(as.integer(years))), de_build_universe_year, include_types = cfg$include_types, cfg = cfg)
  res <- res[!vapply(res, is.null, logical(1))]
  if (!length(res)) stop("Universe is empty.")
  
  dt <- data.table::rbindlist(res, fill = TRUE)
  
  # NEW: Global dedupe and validation after merging years
  dt <- de_dedupe_symbol_date(dt)
  de_validate_contract(dt, "universe_raw")
  
  dt
}