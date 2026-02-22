#' @title Backtest Data and Clock
#' @description Manage simulation calendar, decision dates, execution dates.

#' @export
bt_make_data_context <- function(data_bundle_or_panel, aux = list()) {
  dt <- data.table::as.data.table(data_bundle_or_panel)
  data.table::setkeyv(dt, c("symbol", "refdate"))

  ctx <- list()
  ctx$dt <- dt
  ctx$calendar <- sort(unique(dt$refdate))
  ctx
}

#' @export
bt_calendar <- function(data_ctx) {
  data_ctx$calendar
}

#' @export
bt_rebalance_dates <- function(calendar, spec_schedule) {
  # dummy: last day of each month
  d <- as.Date(calendar)
  mos <- format(d, "%Y-%m")
  as.Date(unname(sort(tapply(d, mos, max))))
}

#' @export
bt_next_exec_date <- function(calendar, decision_date, spec_execution) {
  idx <- which(calendar > decision_date)
  if (length(idx) == 0) return(as.Date(NA))
  calendar[idx[1]]
}

#' @export
bt_get_exec_prices <- function(data_ctx, exec_date, field = "open", symbols = NULL) {
  sub_dt <- data_ctx$dt[refdate == exec_date]
  if (!is.null(symbols)) sub_dt <- sub_dt[symbol %in% symbols]
  res <- sub_dt[[field]]
  names(res) <- sub_dt$symbol
  res
}

#' @export
bt_get_mark_prices <- function(data_ctx, date, field = "close", symbols = NULL) {
  sub_dt <- data_ctx$dt[refdate == date]
  if (!is.null(symbols)) sub_dt <- sub_dt[symbol %in% symbols]
  res <- sub_dt[[field]]
  names(res) <- sub_dt$symbol
  res
}
