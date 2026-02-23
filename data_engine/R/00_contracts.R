# /data_engine/R/00_contracts.R

# --- Data Engine Contracts Definition ---
de_contracts <- list(
  universe_raw     = c("symbol", "refdate", "asset_type", "open", "high", "low", "close", "traded_value", "traded_units", "n_trades", "turnover", "qty"),
  corp_actions_raw = c("symbol", "yahoo_symbol", "refdate", "action_type", "value", "source"),
  events_apply     = c("symbol", "refdate", "split_value", "div_cash", "source_mask", "has_manual"),
  panel_adj_model  = c("symbol", "refdate", "asset_type", "open", "high", "low", "close", "traded_value", "traded_units", "n_trades", "turnover", "qty", "adjustment_state"),
  panel_adj_debug  = c("symbol", "refdate", "asset_type", "open_raw", "high_raw", "low_raw", "close_raw", "open_adj_split", "high_adj_split", "low_adj_split", "close_adj_split", "open", "high", "low", "close", "traded_value", "traded_units", "n_trades", "turnover", "qty", "adjustment_state")
)

# --- Liquidity Semantics ---
# Canonical Field | Legacy Alias | B3 Label   | rb3 Label        | Description
# ----------------|--------------|------------|------------------|---------------------------
# traded_value    | turnover     | VOLTOT     | volume           | Monetary traded value
# traded_units    | qty          | QUATOT     | traded_contracts | Quantity in units
# n_trades        | (none)       | TOTNEG     | trade_quantity   | Number of trades

de_assert_cols <- function(dt, cols, name = "object") {
  if (is.null(dt)) stop(name, " is NULL.", call. = FALSE)
  if (!is.data.frame(dt)) stop(name, " must be a data.frame/data.table.", call. = FALSE)
  miss <- setdiff(cols, names(dt))
  if (length(miss)) stop(name, " contract violation. Missing cols: ", paste(miss, collapse = ", "), call. = FALSE)
  invisible(TRUE)
}

de_assert_no_dupes <- function(dt, key_cols, name = "object") {
  if (!requireNamespace("data.table", quietly = TRUE)) stop("data.table required")
  x <- data.table::as.data.table(dt)
  dup <- x[, .N, by = key_cols][N > 1L]
  if (nrow(dup)) stop(name, " has duplicated keys on: ", paste(key_cols, collapse = ", "), call. = FALSE)
  invisible(TRUE)
}

# New: Strict Type, Domain enforcement, and Enum checks
de_enforce_types_and_domains <- function(dt, contract_name) {
  # FIX: Guard against missing data.table in this isolated scope
  if (!requireNamespace("data.table", quietly = TRUE)) stop("data.table required")

  x <- data.table::as.data.table(dt)

  if ("symbol" %in% names(x)) if (!is.character(x$symbol)) stop(contract_name, "$symbol must be character")
  if ("refdate" %in% names(x)) if (!inherits(x$refdate, "Date")) stop(contract_name, "$refdate must be Date")

  ohlc <- intersect(c("open", "high", "low", "close"), names(x))
  for (col in ohlc) {
    if (any(x[[col]] <= 0, na.rm = TRUE)) stop(contract_name, " has zero/negative prices in ", col)
  }

  liq_nonneg <- intersect(c("turnover", "qty", "traded_value", "traded_units", "n_trades"), names(x))
  for (col in liq_nonneg) {
    if (any(x[[col]] < 0, na.rm = TRUE)) stop(contract_name, " has negative values in ", col)
  }

  if ("asset_type" %in% names(x)) {
    if (!all(x$asset_type %in% c("equity", "fii", "etf", "bdr"))) stop("Invalid asset_type in ", contract_name)
  }

  if (contract_name == "events_apply") {
    if (any(x$split_value <= 0, na.rm = TRUE)) stop("events_apply has invalid split_value <= 0")
    if (any(x$div_cash < 0, na.rm = TRUE)) stop("events_apply has invalid div_cash < 0")
  }
  # Corporate actions enum/domain
  if (all(c("action_type", "value") %in% names(x))) {
    if (!all(x$action_type %in% c("dividend", "split"))) {
      stop(contract_name, " has invalid action_type (allowed: dividend, split)")
    }

    bad_split <- x$action_type == "split" & (!is.finite(x$value) | is.na(x$value) | x$value <= 0)
    bad_div <- x$action_type == "dividend" & (!is.finite(x$value) | is.na(x$value) | x$value < 0)

    if (any(bad_split, na.rm = TRUE)) stop(contract_name, " has invalid split value(s) <= 0 / non-finite")
    if (any(bad_div, na.rm = TRUE)) stop(contract_name, " has invalid dividend value(s) < 0 / non-finite")
  }
  invisible(TRUE)
}

de_validate_contract <- function(dt, contract_name) {
  if (!contract_name %in% names(de_contracts)) stop("Unknown contract: ", contract_name, call. = FALSE)

  de_assert_cols(dt, de_contracts[[contract_name]], contract_name)
  de_enforce_types_and_domains(dt, contract_name)

  if (all(c("symbol", "refdate") %in% names(dt)) && contract_name != "corp_actions_raw") {
    de_assert_no_dupes(dt, c("symbol", "refdate"), contract_name)
    # NOTE: We intentionally DO NOT check or enforce row sorting here.
    # Physical row ordering is the responsibility of the builder functions.
    # This keeps the validator 100% pure and side-effect free.
  }
  invisible(TRUE)
}
