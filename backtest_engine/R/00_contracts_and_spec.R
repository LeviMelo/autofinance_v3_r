#' @title Backtest Engine Contracts and Specs
#' @description Backtest spec, strategy interface contract, validation helpers.

#' @export
bt_spec_default <- function() {
  list(
    schedule = list(freq = "months"),
    execution = list(delay_days = 1, price_field = "open"),
    costs = list(fee_rate = 0.0000, slippage_rate = 0.0000),
    accounting = list(initial_nav = 1.0, initial_cash = 1.0),
    analytics = list(),
    meta = list()
  )
}

#' @export
bt_get_spec <- function(overrides = NULL) {
  spec <- bt_spec_default()
  if (!is.null(overrides)) {
    spec <- utils::modifyList(spec, overrides)
  }
  spec
}

#' @export
bt_validate_spec <- function(spec) {
  req_names <- c("schedule", "execution", "costs", "accounting", "analytics", "meta")
  missing <- setdiff(req_names, names(spec))
  if (length(missing) > 0) stop("BacktestSpec is missing required sections: ", paste(missing, collapse = ", "))
  invisible(spec)
}

#' @export
bt_validate_run_artifact <- function(x) {
  req_names <- c("nav_series", "rebalance_log", "target_log", "fills_log", "cost_log")
  missing <- setdiff(req_names, names(x))
  if (length(missing) > 0) stop("BacktestRunArtifact missing: ", paste(missing, collapse = ", "))
  invisible(x)
}
