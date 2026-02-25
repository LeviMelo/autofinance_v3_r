#' @title Backtest Engine — Data Context and Clock
#' @description Calendar, scheduling, price access, and no-lookahead enforcement.

# ── Data context constructor ──────────────────────────────────────────────────

#' @export
bt_make_data_context <- function(data_bundle_or_panel) {
    if (!requireNamespace("data.table", quietly = TRUE)) {
        stop("Package 'data.table' required")
    }

    if (is.list(data_bundle_or_panel) && "panel_adj_model" %in% names(data_bundle_or_panel)) {
        dt <- data.table::as.data.table(data_bundle_or_panel$panel_adj_model)
    } else if (is.data.frame(data_bundle_or_panel)) {
        dt <- data.table::as.data.table(data_bundle_or_panel)
    } else {
        stop("Input must be a data.frame or bundle with 'panel_adj_model'")
    }

    req <- c("symbol", "refdate", "open", "close")
    miss <- setdiff(req, names(dt))
    if (length(miss) > 0) stop("Panel missing: ", paste(miss, collapse = ", "))

    if (!inherits(dt$refdate, "Date")) dt[, refdate := as.Date(refdate)]
    data.table::setkeyv(dt, c("symbol", "refdate"))

    # Ensure activity columns
    if (!"turnover" %in% names(dt) && "traded_value" %in% names(dt)) {
        dt[, turnover := traded_value]
    }
    if (!"qty" %in% names(dt) && "traded_units" %in% names(dt)) {
        dt[, qty := traded_units]
    }

    list(dt = dt)
}

# ── Calendar ──────────────────────────────────────────────────────────────────

#' @export
bt_calendar <- function(data_ctx) sort(unique(data_ctx$dt$refdate))

# ── Rebalance schedule ────────────────────────────────────────────────────────

#' @export
bt_rebalance_dates <- function(calendar, spec_clock) {
    freq <- spec_clock$freq %||% "months"

    if (length(calendar) < 2) {
        return(calendar)
    }

    if (freq == "days") {
        return(calendar[-1])
    }

    if (freq == "weeks") {
        wks <- as.numeric(format(calendar, "%W"))
        keep <- c(FALSE, diff(wks) != 0)
        return(calendar[keep])
    }

    if (freq == "months") {
        mos <- as.numeric(format(calendar, "%m"))
        keep <- c(FALSE, diff(mos) != 0)
        return(calendar[keep])
    }

    if (is.numeric(freq)) {
        idx <- seq(1, length(calendar), by = max(1L, as.integer(freq)))
        return(calendar[idx[-1]])
    }

    stop("Unsupported clock$freq: ", freq)
}

# ── Decision → Execution mapping ─────────────────────────────────────────────

#' @export
bt_map_execution_date <- function(calendar, decision_date, spec_execution) {
    lag <- spec_execution$exec_lag %||% 1L
    future <- calendar[calendar > decision_date]
    if (length(future) < lag) {
        return(as.Date(NA))
    }
    future[lag]
}

# ── Price fetching ────────────────────────────────────────────────────────────

#' @export
bt_get_exec_prices <- function(data_ctx, exec_date, price_field, symbols = NULL) {
    sub <- data_ctx$dt[refdate == exec_date]
    if (!is.null(symbols)) sub <- sub[symbol %in% symbols]

    prices <- sub[[price_field]]
    names(prices) <- sub$symbol

    if (!is.null(symbols)) {
        out <- rep(NA_real_, length(symbols))
        names(out) <- symbols
        found <- intersect(symbols, names(prices))
        out[found] <- prices[found]
        return(out)
    }
    prices
}

#' @export
bt_get_mark_prices <- function(data_ctx, mark_date, mark_field = "close",
                               symbols = NULL) {
    bt_get_exec_prices(data_ctx, mark_date, mark_field, symbols)
}

# ── No-lookahead data slice ───────────────────────────────────────────────────

#' @export
bt_as_of_slice <- function(data_ctx, as_of_date) {
    data_ctx$dt[refdate <= as_of_date]
}
