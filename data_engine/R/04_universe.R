# /data_engine/R/04_universe.R

de_select_min_cols <- function(df) {
  de_require(c("data.table", "dplyr"))
  dt <- data.table::as.data.table(df)

  pick_col <- function(alts) {
    cand <- intersect(alts, names(dt))
    if (length(cand)) cand[1] else NA_character_
  }

  # Core identity / price
  c_sym <- pick_col(c("symbol", "ticker"))
  c_ref <- pick_col(c("refdate", "date"))
  c_open <- pick_col(c("open", "price.open", "preco_abertura"))
  c_high <- pick_col(c("high", "price.high", "preco_maximo"))
  c_low <- pick_col(c("low", "price.low", "preco_minimo"))
  c_close <- pick_col(c("close", "price.close", "preco_ultimo"))

  # Price/quote extras (optional)
  c_avg <- pick_col(c("average"))
  c_bid <- pick_col(c("best_bid"))
  c_ask <- pick_col(c("best_ask"))

  # Activity fields (semantic correction)
  c_tval <- pick_col(c("traded_value", "financial_volume", "volume", "vol_fin")) # VOLTOT
  c_tunit <- pick_col(c("traded_units", "traded_contracts", "quantity", "qty")) # QUATOT
  c_ntr <- pick_col(c("n_trades", "trade_quantity", "trade_count")) # TOTNEG

  # Metadata (optional, preserved if present)
  c_isin <- pick_col(c("isin"))
  c_instr_mkt <- pick_col(c("instrument_market"))
  c_corp <- pick_col(c("corporation_name"))
  c_spec <- pick_col(c("specification_code"))
  c_bdi <- pick_col(c("bdi_code"))
  c_ccy <- pick_col(c("trading_currency"))
  c_settl <- pick_col(c("days_to_settlement"))
  c_lot <- pick_col(c("allocation_lot_size"))

  if (any(is.na(c(c_sym, c_ref, c_close)))) stop("Missing core B3 columns in rb3 output.")

  data.table::data.table(
    symbol = toupper(trimws(as.character(dt[[c_sym]]))),
    refdate = as.Date(dt[[c_ref]]),
    open = if (!is.na(c_open)) as.numeric(dt[[c_open]]) else NA_real_,
    high = if (!is.na(c_high)) as.numeric(dt[[c_high]]) else NA_real_,
    low = if (!is.na(c_low)) as.numeric(dt[[c_low]]) else NA_real_,
    close = as.numeric(dt[[c_close]]),

    # raw price/quote extras (preserve; adjust policy can evolve later)
    average_raw = if (!is.na(c_avg)) as.numeric(dt[[c_avg]]) else NA_real_,
    best_bid_raw = if (!is.na(c_bid)) as.numeric(dt[[c_bid]]) else NA_real_,
    best_ask_raw = if (!is.na(c_ask)) as.numeric(dt[[c_ask]]) else NA_real_,

    # raw activity fields
    traded_value_raw = if (!is.na(c_tval)) as.numeric(dt[[c_tval]]) else NA_real_,
    traded_units_raw = if (!is.na(c_tunit)) as.numeric(dt[[c_tunit]]) else NA_real_,
    n_trades_raw = if (!is.na(c_ntr)) as.numeric(dt[[c_ntr]]) else NA_real_,

    # metadata (keep source-ish names to reduce remap friction)
    isin = if (!is.na(c_isin)) as.character(dt[[c_isin]]) else NA_character_,
    instrument_market = if (!is.na(c_instr_mkt)) as.character(dt[[c_instr_mkt]]) else NA_character_,
    corporation_name = if (!is.na(c_corp)) as.character(dt[[c_corp]]) else NA_character_,
    specification_code = if (!is.na(c_spec)) as.character(dt[[c_spec]]) else NA_character_,
    bdi_code = if (!is.na(c_bdi)) as.character(dt[[c_bdi]]) else NA_character_,
    trading_currency = if (!is.na(c_ccy)) as.character(dt[[c_ccy]]) else NA_character_,
    days_to_settlement = if (!is.na(c_settl)) as.numeric(dt[[c_settl]]) else NA_real_,
    allocation_lot_size = if (!is.na(c_lot)) as.numeric(dt[[c_lot]]) else NA_real_
  )
}

de_unify_liquidity <- function(dt) {
  # Canonical activity fields
  dt[, traded_units := traded_units_raw]
  dt[, n_trades := n_trades_raw]

  # Monetary traded value (prefer source financial volume, fallback to units * close)
  dt[, traded_value := data.table::fifelse(
    is.finite(traded_value_raw) & traded_value_raw > 0, traded_value_raw,
    data.table::fifelse(
      is.finite(traded_units) & traded_units > 0 & is.finite(close) & close > 0,
      traded_units * close,
      NA_real_
    )
  )]

  # Legacy compatibility aliases (semantics corrected)
  dt[, turnover := traded_value]
  dt[, qty := traded_units]

  # Drop raw staging cols
  drop_cols <- intersect(c("traded_value_raw", "traded_units_raw", "n_trades_raw"), names(dt))
  if (length(drop_cols)) dt[, (drop_cols) := NULL]

  dt
}

de_dedupe_symbol_date <- function(dt) {
  data.table::setorder(dt, asset_type, symbol, refdate)
  dup <- dt[, .N, by = .(symbol, refdate)][N > 1L]
  if (nrow(dup)) {
    if (!"traded_value" %in% names(dt)) dt[, traded_value := if ("turnover" %in% names(dt)) turnover else NA_real_]
    if (!"traded_units" %in% names(dt)) dt[, traded_units := if ("qty" %in% names(dt)) qty else NA_real_]
    if (!"n_trades" %in% names(dt)) dt[, n_trades := NA_real_]

    dt <- dt[order(
      symbol, refdate,
      -data.table::fifelse(is.finite(traded_value), traded_value, -Inf),
      -data.table::fifelse(is.finite(traded_units), traded_units, -Inf),
      -data.table::fifelse(is.finite(n_trades), n_trades, -Inf),
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
      equity = tryCatch(rb3::cotahist_filter_equity(df_lazy), error = function(e) NULL),
      fii    = tryCatch(rb3::cotahist_filter_fii(df_lazy), error = function(e) NULL),
      etf    = tryCatch(rb3::cotahist_filter_etf(df_lazy), error = function(e) NULL),
      bdr    = tryCatch(rb3::cotahist_filter_bdr(df_lazy), error = function(e) NULL)
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
  if (!length(out_list)) {
    return(NULL)
  }
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
