#' @title Backtest Engine — Data Context and Clock
#' @description Calendar, scheduling, price access, no-lookahead enforcement, base universe selection.

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

    # Normalize activity columns BOTH ways for downstream compatibility
    if (!"traded_value" %in% names(dt) && "turnover" %in% names(dt)) dt[, traded_value := turnover]
    if (!"turnover" %in% names(dt) && "traded_value" %in% names(dt)) dt[, turnover := traded_value]

    if (!"traded_units" %in% names(dt) && "qty" %in% names(dt)) dt[, traded_units := qty]
    if (!"qty" %in% names(dt) && "traded_units" %in% names(dt)) dt[, qty := traded_units]

    if (!"n_trades" %in% names(dt) && "ntrades" %in% names(dt)) dt[, n_trades := ntrades]

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
bt_get_mark_prices <- function(data_ctx, mark_date, mark_field = "close", symbols = NULL) {
    bt_get_exec_prices(data_ctx, mark_date, mark_field, symbols)
}

# ── No-lookahead data slice ───────────────────────────────────────────────────

#' @export
bt_as_of_slice <- function(data_ctx, as_of_date) {
    data_ctx$dt[refdate <= as_of_date]
}

# ── Universe selection (Backtester-owned base opportunity set) ────────────────

.bt_last_n_dates <- function(calendar, as_of_date, n) {
    cal <- calendar[calendar <= as_of_date]
    if (length(cal) < n) {
        return(NULL)
    }
    tail(cal, n)
}

#' @export
bt_select_universe <- function(data_ctx, as_of_date, universe_spec) {
    if (!requireNamespace("data.table", quietly = TRUE)) stop("Need data.table")

    as_of_date <- as.Date(as_of_date)
    dt <- data_ctx$dt
    cal <- bt_calendar(data_ctx)

    lb <- as.integer(universe_spec$lookback_days %||% 63L)
    dates_lb <- .bt_last_n_dates(cal, as_of_date, lb)
    if (is.null(dates_lb)) {
        return(list(
            universe = character(0),
            status = "WARMUP",
            reason = sprintf("Universe warmup: need %d days, have %d", lb, sum(cal <= as_of_date)),
            diag = list(as_of_date = as_of_date, have_days = sum(cal <= as_of_date), need_days = lb)
        ))
    }

    sub <- dt[refdate %in% dates_lb]
    # type filter if available
    if ("asset_type" %in% names(sub)) {
        types <- universe_spec$include_types %||% NULL
        if (!is.null(types)) sub <- sub[asset_type %in% types]
    }

    # Ensure traded_value exists
    if (!"traded_value" %in% names(sub)) {
        if ("turnover" %in% names(sub)) sub[, traded_value := turnover] else sub[, traded_value := NA_real_]
    }

    # Aggregate liquidity / coverage
    stats <- sub[, .(
        n_obs = .N,
        med_tv = median(traded_value, na.rm = TRUE)
    ), by = symbol]

    cov_ratio <- stats$n_obs / length(dates_lb)
    stats[, cov := cov_ratio]

    min_cov <- universe_spec$min_days_traded_ratio %||% 0.75
    min_tv <- universe_spec$min_median_traded_value %||% 0

    stats <- stats[cov >= min_cov & is.finite(med_tv) & med_tv >= min_tv]

    # Require valid last close on as_of_date (or last available in window if missing on exact day)
    last_px <- dt[refdate <= as_of_date, .(last_close = tail(close, 1)), by = symbol]
    stats <- merge(stats, last_px, by = "symbol", all.x = TRUE)
    stats <- stats[is.finite(last_close) & last_close > 0]

    # Optional: rectangular close price history
    mph <- universe_spec$min_price_history_days %||% NULL
    if (!is.null(mph)) {
        mph <- as.integer(mph)
        dates_ph <- .bt_last_n_dates(cal, as_of_date, mph)
        if (is.null(dates_ph)) {
            return(list(
                universe = character(0),
                status = "WARMUP",
                reason = sprintf("Price-history warmup: need %d days, have %d", mph, sum(cal <= as_of_date)),
                diag = list(as_of_date = as_of_date, have_days = sum(cal <= as_of_date), need_days = mph)
            ))
        }

        cand <- stats$symbol
        subp <- dt[symbol %in% cand & refdate %in% dates_ph, .(n_obs = .N, n_finite = sum(is.finite(close) & close > 0)), by = symbol]
        subp <- subp[n_obs == length(dates_ph) & n_finite == length(dates_ph)]
        stats <- stats[symbol %in% subp$symbol]
    }

    # Optional cap by liquidity rank
    maxu <- universe_spec$max_universe %||% NULL
    if (!is.null(maxu)) {
        maxu <- as.integer(maxu)
        data.table::setorder(stats, -med_tv)
        stats <- head(stats, maxu)
    }

    u <- unique(as.character(stats$symbol))
    u <- u[!is.na(u) & nzchar(u)]

    list(
        universe = u,
        status = "OK",
        reason = "ok",
        diag = list(
            as_of_date = as_of_date,
            lookback_days = lb,
            n_universe = length(u),
            min_cov = min_cov,
            min_median_traded_value = min_tv,
            min_price_history_days = mph %||% NA_integer_
        )
    )
}

# ── Tradability mask (optional; useful for models) ────────────────────────────

#' @export
bt_tradability_mask <- function(data_ctx, as_of_date, symbols) {
    if (!requireNamespace("data.table", quietly = TRUE)) stop("Need data.table")
    as_of_date <- as.Date(as_of_date)

    dt <- data_ctx$dt[refdate == as_of_date & symbol %in% symbols]
    if (nrow(dt) == 0) {
        out <- setNames(rep(FALSE, length(symbols)), symbols)
        return(out)
    }

    # Ensure fields exist
    if (!"traded_value" %in% names(dt) && "turnover" %in% names(dt)) dt[, traded_value := turnover]
    if (!"n_trades" %in% names(dt) && "ntrades" %in% names(dt)) dt[, n_trades := ntrades]
    if (!"n_trades" %in% names(dt)) dt[, n_trades := NA_real_]
    if (!"traded_value" %in% names(dt)) dt[, traded_value := NA_real_]

    tv <- dt$traded_value
    names(tv) <- dt$symbol
    nt <- dt$n_trades
    names(nt) <- dt$symbol
    cl <- dt$close
    names(cl) <- dt$symbol

    out <- setNames(rep(FALSE, length(symbols)), symbols)
    out[names(tv)] <- (is.finite(tv) & tv > 0) & (is.finite(nt) & nt > 0) & (is.finite(cl) & cl > 0)
    out
}

# ── Build strategy-visible context (no-lookahead + base universe + holdings) ─

#' @export
bt_build_strategy_context <- function(data_ctx, decision_date, base_universe,
                                      holdings_symbols = character(0), keep_holdings = TRUE) {
    decision_date <- as.Date(decision_date)

    syms <- base_universe
    if (isTRUE(keep_holdings) && length(holdings_symbols) > 0) {
        syms <- unique(c(syms, holdings_symbols))
    }

    dt_slice <- bt_as_of_slice(data_ctx, decision_date)
    dt_slice <- dt_slice[symbol %in% syms]

    list(
        dt = dt_slice,
        decision_date = decision_date,
        universe = base_universe
    )
}
