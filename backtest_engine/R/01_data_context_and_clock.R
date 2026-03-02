# backtest_engine/R/01_data_context_and_clock.R
# Backtest Engine v3 — strict calendar, strict universe, strict price access.

#' @export
bt_make_data_context <- function(data_bundle_or_panel) {
    if (!requireNamespace("data.table", quietly = TRUE)) .bt_stop("pkg_missing", "Package 'data.table' required")

    dt <- NULL
    if (is.list(data_bundle_or_panel) && "panel_adj_model" %in% names(data_bundle_or_panel)) {
        dt <- data.table::as.data.table(data_bundle_or_panel$panel_adj_model)
    } else if (is.data.frame(data_bundle_or_panel)) {
        dt <- data.table::as.data.table(data_bundle_or_panel)
    } else {
        .bt_stop("data_input", "Input must be a data.frame or list(panel_adj_model)")
    }

    req <- c("symbol", "refdate", "open", "close", "traded_value", "traded_units", "n_trades")
    miss <- setdiff(req, names(dt))
    if (length(miss) > 0) .bt_stop("data_missing_cols", paste("Missing columns:", paste(miss, collapse = ", ")))

    if (!inherits(dt$refdate, "Date")) dt[, refdate := as.Date(refdate)]
    if (any(is.na(dt$refdate))) .bt_stop("data_refdate", "refdate contains NA after coercion")

    dt[, symbol := as.character(symbol)]
    if (any(is.na(dt$symbol) | !nzchar(dt$symbol))) .bt_stop("data_symbol", "symbol contains NA/empty strings")

    # strict uniqueness
    data.table::setkeyv(dt, c("symbol", "refdate"))
    if (anyDuplicated(dt, by = c("symbol", "refdate")) > 0) .bt_stop("data_duplicates", "Duplicate (symbol, refdate) rows")

    # optional asset_type
    if (!"asset_type" %in% names(dt)) dt[, asset_type := NA_character_]

    list(dt = dt)
}

#' @export
bt_calendar <- function(data_ctx) sort(unique(data_ctx$dt$refdate))

.bt_snap_left <- function(cal, d) {
    d <- as.Date(d)
    if (is.na(d)) .bt_stop("date_invalid", "Invalid Date")
    x <- cal[cal >= d]
    if (length(x) == 0) as.Date(NA) else x[1]
}

.bt_snap_right <- function(cal, d) {
    d <- as.Date(d)
    if (is.na(d)) .bt_stop("date_invalid", "Invalid Date")
    x <- cal[cal <= d]
    if (length(x) == 0) as.Date(NA) else x[length(x)]
}

.bt_idx <- function(cal, d) {
    d <- as.Date(d)
    i <- match(d, cal)
    if (is.na(i)) .bt_stop("date_not_in_calendar", paste("Date not in calendar:", as.character(d)))
    as.integer(i)
}

#' @export
bt_shift_trading_days <- function(cal, d, k) {
    d <- as.Date(d)
    if (is.na(d)) .bt_stop("date_invalid", "Invalid Date")
    if (!.bt_is_scalar_int(k)) .bt_stop("shift_k", "k must be integer")
    i <- match(d, cal)
    if (is.na(i)) .bt_stop("date_not_in_calendar", paste("Date not in calendar:", as.character(d)))
    j <- i + as.integer(k)
    if (j < 1L || j > length(cal)) as.Date(NA) else cal[j]
}

#' @export
bt_rebalance_dates <- function(calendar, spec_clock) {
    cal <- as.Date(calendar)
    if (length(cal) < 2) {
        return(cal)
    }

    freq <- spec_clock$freq %||% "months"

    if (identical(freq, "days")) {
        return(cal)
    } # daily decisions
    if (identical(freq, "weeks")) {
        wks <- as.integer(format(cal, "%Y")) * 100L + as.integer(format(cal, "%W"))
        keep <- c(TRUE, diff(wks) != 0)
        return(cal[keep])
    }
    if (identical(freq, "months")) {
        mos <- as.integer(format(cal, "%Y")) * 100L + as.integer(format(cal, "%m"))
        keep <- c(TRUE, diff(mos) != 0)
        return(cal[keep])
    }
    if (is.numeric(freq)) {
        n <- as.integer(freq)
        if (n < 1L) .bt_stop("clock_freq", "clock$freq integer must be >= 1")
        idx <- seq(1L, length(cal), by = n)
        return(cal[idx])
    }

    .bt_stop("clock_freq", "Unsupported clock$freq")
}

#' @export
bt_map_execution_date <- function(cal, decision_date, exec_lag) {
    if (!.bt_is_scalar_int(exec_lag) || as.integer(exec_lag) < 0L) .bt_stop("exec_lag", "exec_lag must be integer >= 0")
    d <- as.Date(decision_date)
    if (is.na(d)) .bt_stop("date_invalid", "Invalid decision_date")
    if (!d %in% cal) .bt_stop("date_not_in_calendar", paste("Decision date not in calendar:", as.character(d)))
    if (exec_lag == 0L) {
        return(d)
    }
    bt_shift_trading_days(cal, d, as.integer(exec_lag))
}

.bt_prices_on_date_strict <- function(data_ctx, date, field, symbols) {
    dt <- data_ctx$dt
    d <- as.Date(date)
    if (is.na(d)) .bt_stop("date_invalid", "Invalid date")
    if (!field %in% names(dt)) .bt_stop("price_field", paste("Missing field:", field))

    syms <- unique(as.character(symbols))
    if (any(is.na(syms) | !nzchar(syms))) {
        bad <- syms[is.na(syms) | !nzchar(syms)]
        .bt_stop("price_symbols_invalid", paste("Invalid symbols requested for price lookup:", paste(unique(bad), collapse = ", ")))
    }
    if (length(syms) == 0) {
        return(setNames(numeric(0), character(0)))
    }

    sub <- dt[refdate == d & symbol %in% syms]
    if (nrow(sub) != length(syms)) {
        miss <- setdiff(syms, sub$symbol)
        .bt_stop("price_missing", paste("Missing prices for", field, "on", as.character(d), ":", paste(miss, collapse = ", ")))
    }

    px <- as.numeric(sub[[field]])
    names(px) <- sub$symbol
    bad <- !is.finite(px) | px <= 0
    if (any(bad)) {
        .bt_stop("price_invalid", paste("Invalid prices for", field, "on", as.character(d), ":", paste(names(px)[bad], collapse = ", ")))
    }

    px[syms]
}

# Mark-to-market price lookup with optional carry of the last valid observed price.
.bt_prices_on_date_mark <- function(data_ctx, cal, date, field, symbols,
                                    missing_policy = "carry_last",
                                    max_stale_days = 5L) {
    if (!requireNamespace("data.table", quietly = TRUE)) .bt_stop("pkg_missing", "Package 'data.table' required")
    dt <- data_ctx$dt

    d <- as.Date(date)
    if (is.na(d)) .bt_stop("date_invalid", "Invalid date")
    if (!d %in% cal) .bt_stop("date_not_in_calendar", paste("Date not in calendar:", as.character(d)))
    if (!field %in% names(dt)) .bt_stop("price_field", paste("Missing field:", field))

    syms <- unique(as.character(symbols))
    if (any(is.na(syms) | !nzchar(syms))) {
        bad <- syms[is.na(syms) | !nzchar(syms)]
        .bt_stop("price_symbols_invalid", paste("Invalid symbols requested for mark lookup:", paste(unique(bad), collapse = ", ")))
    }
    if (length(syms) == 0) return(setNames(numeric(0), character(0)))

    out <- setNames(rep(NA_real_, length(syms)), syms)

    today <- dt[refdate == d & symbol %in% syms, .(symbol, px = as.numeric(get(field)))]
    if (nrow(today) > 0) {
        today <- today[is.finite(px) & px > 0]
        if (nrow(today) > 0) out[today$symbol] <- today$px
    }

    miss <- names(out)[!is.finite(out)]
    if (length(miss) == 0) return(out[syms])

    pol <- missing_policy %||% "carry_last"
    if (identical(pol, "error")) {
        .bt_stop("price_missing", paste("Missing prices for", field, "on", as.character(d), ":", paste(miss, collapse = ", ")))
    }
    if (!identical(pol, "carry_last")) {
        .bt_stop("mark_policy", paste("Unsupported mark_missing_policy:", as.character(pol)))
    }

    hist <- dt[symbol %in% miss & refdate <= d, .(symbol, refdate, px = as.numeric(get(field)))]
    if (nrow(hist) > 0) {
        hist <- hist[is.finite(px) & px > 0]
    }
    if (nrow(hist) > 0) {
        data.table::setorder(hist, symbol, refdate)
        last <- hist[, .SD[.N], by = symbol]

        if (!is.null(max_stale_days)) {
            max_stale_days <- as.integer(max_stale_days)
            idx_now <- match(d, cal)
            idx_last <- match(last$refdate, cal)
            stale <- idx_now - idx_last
            keep <- is.finite(stale) & stale >= 0 & stale <= max_stale_days
            last <- last[keep]
        }

        if (nrow(last) > 0) out[last$symbol] <- last$px
    }

    miss2 <- names(out)[!is.finite(out)]
    if (length(miss2) > 0) {
        last_seen <- dt[symbol %in% miss2 & refdate <= d & is.finite(get(field)) & get(field) > 0,
            .(last_refdate = max(refdate)),
            by = symbol
        ]
        last_map <- setNames(as.character(last_seen$last_refdate), as.character(last_seen$symbol))
        miss_info <- vapply(miss2, function(s) {
            ls <- last_map[[s]]
            if (is.null(ls) || !nzchar(ls)) {
                paste0(s, "(last=NA)")
            } else {
                paste0(s, "(last=", ls, ")")
            }
        }, character(1))
        .bt_stop(
            "price_missing_mark",
            paste(
                "Missing mark prices for", field, "on", as.character(d), ":",
                paste(miss_info, collapse = ", "),
                "| policy=carry_last",
                "| max_stale_days=", if (is.null(max_stale_days)) "NULL" else as.character(max_stale_days)
            )
        )
    }

    out[syms]
}

.bt_prices_on_date_available <- function(data_ctx, date, field, symbols) {
    dt <- data_ctx$dt
    d <- as.Date(date)
    if (is.na(d)) .bt_stop("date_invalid", "Invalid date")
    if (!field %in% names(dt)) .bt_stop("price_field", paste("Missing field:", field))

    syms <- unique(as.character(symbols))
    syms <- syms[!is.na(syms) & nzchar(syms)]
    if (length(syms) == 0) return(setNames(numeric(0), character(0)))

    sub <- dt[refdate == d & symbol %in% syms]
    if (nrow(sub) == 0) return(setNames(numeric(0), character(0)))

    px <- as.numeric(sub[[field]])
    names(px) <- as.character(sub$symbol)
    keep <- is.finite(px) & px > 0 & !is.na(names(px)) & nzchar(names(px))
    px <- px[keep]
    if (length(px) == 0) return(setNames(numeric(0), character(0)))
    px[unique(names(px))]
}

#' @export
bt_as_of_slice <- function(data_ctx, as_of_date) {
    d <- as.Date(as_of_date)
    if (is.na(d)) .bt_stop("date_invalid", "Invalid as_of_date")
    data_ctx$dt[refdate <= d]
}

# Strict base universe selection.
# Errors if insufficient calendar history for the configured rules.
#' @export
bt_select_universe <- function(data_ctx, as_of_date, universe_spec) {
    if (!requireNamespace("data.table", quietly = TRUE)) .bt_stop("pkg_missing", "Package 'data.table' required")

    d <- as.Date(as_of_date)
    if (is.na(d)) .bt_stop("date_invalid", "Invalid as_of_date")

    dt <- data_ctx$dt
    cal <- bt_calendar(data_ctx)
    if (!d %in% cal) .bt_stop("date_not_in_calendar", paste("as_of_date not in calendar:", as.character(d)))

    lb <- as.integer(universe_spec$lookback_days %||% 63L)
    mph <- universe_spec$min_price_history_days %||% NULL
    mph <- if (is.null(mph)) NULL else as.integer(mph)

    # Required calendar depth for investability check (strict)
    need <- lb
    if (!is.null(mph)) need <- max(need, mph)

    cal_upto <- cal[cal <= d]
    if (length(cal_upto) < need) {
        .bt_stop(
            "universe_insufficient_history",
            sprintf("Need %d trading days up to %s, have %d", need, as.character(d), length(cal_upto))
        )
    }

    dates_lb <- tail(cal_upto, lb)
    sub <- dt[refdate %in% dates_lb]

    # type filter if present
    if ("asset_type" %in% names(sub)) {
        types <- universe_spec$include_types %||% NULL
        if (!is.null(types)) sub <- sub[asset_type %in% types]
    }

    # strict activity
    if (!all(c("traded_value", "n_trades", "close") %in% names(sub))) .bt_stop("universe_missing_cols", "Missing traded_value/n_trades/close")

    # coverage + median traded value
    stats <- sub[, .(
        n_obs = .N,
        med_tv = stats::median(traded_value, na.rm = TRUE)
    ), by = symbol]

    cov <- stats$n_obs / length(dates_lb)
    stats[, cov := cov]

    min_cov <- universe_spec$min_days_traded_ratio %||% 0.75
    min_tv <- universe_spec$min_median_traded_value %||% 0

    stats <- stats[is.finite(med_tv) & med_tv >= min_tv & cov >= min_cov]

    # strict existence on as_of_date (so we can mark at close and keep causality stable)
    today <- dt[refdate == d, .(symbol, close, traded_value, n_trades)]
    today <- today[is.finite(close) & close > 0 & is.finite(traded_value) & traded_value > 0 & is.finite(n_trades) & n_trades > 0]
    stats <- stats[symbol %in% today$symbol]

    # strict rectangular close history if enabled
    if (!is.null(mph)) {
        dates_ph <- tail(cal_upto, mph)
        cand <- stats$symbol
        subp <- dt[symbol %in% cand & refdate %in% dates_ph, .(
            n_obs = .N,
            n_ok = sum(is.finite(close) & close > 0)
        ), by = symbol]
        ok <- subp[n_obs == length(dates_ph) & n_ok == length(dates_ph), symbol]
        stats <- stats[symbol %in% ok]
    }

    # top-K cap
    maxu <- universe_spec$max_universe %||% NULL
    if (!is.null(maxu)) {
        maxu <- as.integer(maxu)
        data.table::setorder(stats, -med_tv)
        stats <- head(stats, maxu)
    }

    u <- unique(as.character(stats$symbol))
    u <- u[!is.na(u) & nzchar(u)]
    if (length(u) < 1) .bt_stop("universe_empty", paste("Universe empty on", as.character(d), "under configured rules"))

    u
}

#' @export
bt_tradability_mask <- function(data_ctx, as_of_date, symbols) {
    if (!requireNamespace("data.table", quietly = TRUE)) .bt_stop("pkg_missing", "Package 'data.table' required")
    d <- as.Date(as_of_date)
    if (is.na(d)) .bt_stop("date_invalid", "Invalid as_of_date")

    syms <- unique(as.character(symbols))
    if (length(syms) == 0) {
        return(setNames(logical(0), character(0)))
    }

    dt <- data_ctx$dt[refdate == d & symbol %in% syms]
    out <- setNames(rep(FALSE, length(syms)), syms)
    if (nrow(dt) == 0) {
        return(out)
    }

    tv <- dt$traded_value
    names(tv) <- dt$symbol
    nt <- dt$n_trades
    names(nt) <- dt$symbol
    cl <- dt$close
    names(cl) <- dt$symbol

    ok <- (is.finite(tv) & tv > 0) & (is.finite(nt) & nt > 0) & (is.finite(cl) & cl > 0)
    out[names(ok)] <- ok
    out
}

#' @export
bt_build_strategy_context <- function(data_ctx, decision_date, base_universe,
                                      holdings_symbols = character(0), keep_holdings = TRUE) {
    d <- as.Date(decision_date)
    if (is.na(d)) .bt_stop("date_invalid", "Invalid decision_date")

    u <- unique(as.character(base_universe))
    if (length(u) < 1) .bt_stop("context_universe_empty", "base_universe empty")

    syms <- u
    if (isTRUE(keep_holdings) && length(holdings_symbols) > 0) {
        syms <- unique(c(syms, as.character(holdings_symbols)))
    }

    dt_slice <- bt_as_of_slice(data_ctx, d)
    dt_slice <- dt_slice[symbol %in% syms]

    list(
        dt = dt_slice,
        decision_date = d,
        universe = u
    )
}
