#' @title Backtest Analytics
#' @description Compute summary metrics and diagnostics from backtest outputs.

#' @export
bt_compute_performance <- function(nav_series, spec_analytics = list()) {
  if (nrow(nav_series) < 2) return(list(cum_ret = 0, cagr = 0, vol = 0, sharpe = 0))

  rets <- diff(nav_series$nav) / nav_series$nav[-nrow(nav_series)]

  cum_ret <- (tail(nav_series$nav, 1) / nav_series$nav[1]) - 1

  days <- as.numeric(difftime(tail(nav_series$date, 1), nav_series$date[1], units = "days"))
  years <- days / 365.25
  cagr <- if (years > 0) (1 + cum_ret)^(1 / years) - 1 else 0

  vol <- sd(rets, na.rm = TRUE) * sqrt(252)
  sharpe <- if (vol > 0) cagr / vol else 0

  list(
    cum_ret = cum_ret,
    cagr = cagr,
    vol = vol,
    sharpe = sharpe
  )
}

#' @export
bt_compute_drawdowns <- function(nav_series) {
  if (nrow(nav_series) < 1) return(list(max_dd = 0))
  roll_max <- cummax(nav_series$nav)
  dd <- (nav_series$nav / roll_max) - 1
  list(max_dd = min(dd, na.rm = TRUE))
}

#' @export
bt_compute_backtest_summary <- function(run_artifact, spec_analytics = list()) {
  perf <- bt_compute_performance(run_artifact$nav_series, spec_analytics)
  dd <- bt_compute_drawdowns(run_artifact$nav_series)

  c(perf, dd)
}
