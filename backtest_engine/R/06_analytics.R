# backtest_engine/R/06_analytics.R
# Backtest Engine v3 — daily NAV analytics (strict).

.bt_perf_from_nav <- function(nav_df, rf_annual = 0) {
    if (!is.data.frame(nav_df) || nrow(nav_df) < 2) {
        return(list(
            cagr = NA_real_, vol = NA_real_, sharpe = NA_real_, max_dd = NA_real_, calmar = NA_real_,
            start = as.Date(NA), end = as.Date(NA), n_obs = 0L
        ))
    }

    nav_df <- nav_df[order(as.Date(nav_df$date)), , drop = FALSE]
    nav <- as.numeric(nav_df$nav)
    dates <- as.Date(nav_df$date)

    if (any(!is.finite(nav)) || any(nav <= 0)) .bt_stop("nav_invalid", "NAV must be finite and > 0 for analytics")

    rets <- diff(log(nav))
    rets <- rets[is.finite(rets)]
    if (length(rets) < 2) {
        return(list(
            cagr = NA_real_, vol = NA_real_, sharpe = NA_real_, max_dd = NA_real_, calmar = NA_real_,
            start = min(dates), end = max(dates), n_obs = length(nav)
        ))
    }

    # daily RF
    rf_annual <- rf_annual %||% 0
    if (!is.finite(rf_annual) || rf_annual < 0) .bt_stop("rf_invalid", "rf_annual must be >= 0")
    rf_d <- (1 + rf_annual)^(1 / 252) - 1

    mu <- mean(rets)
    sdv <- stats::sd(rets)
    vol <- sdv * sqrt(252)

    sharpe <- if (sdv > 0) ((mu - rf_d) / sdv) * sqrt(252) else NA_real_

    n_days <- as.numeric(max(dates) - min(dates))
    years <- n_days / 365.25
    if (!is.finite(years) || years <= 0) years <- NA_real_

    cagr <- if (!is.na(years)) (nav[length(nav)] / nav[1])^(1 / years) - 1 else NA_real_

    cum_max <- cummax(nav)
    dd <- nav / cum_max - 1
    max_dd <- min(dd)

    calmar <- if (is.finite(cagr) && is.finite(max_dd) && max_dd < 0) cagr / abs(max_dd) else NA_real_

    list(
        cagr = cagr,
        vol = vol,
        sharpe = sharpe,
        max_dd = max_dd,
        calmar = calmar,
        start = min(dates),
        end = max(dates),
        n_obs = length(nav)
    )
}

#' @export
bt_compute_analytics <- function(run_artifact) {
    if (!is.list(run_artifact) || !"nav" %in% names(run_artifact)) .bt_stop("artifact", "Invalid run_artifact")

    nav <- run_artifact$nav
    rf <- run_artifact$meta$bt_spec$analytics$rf_annual %||% 0

    if (!"in_score" %in% names(nav)) .bt_stop("score_flag_missing", "run_artifact$nav is missing 'in_score' column")
    in_score <- as.logical(nav$in_score)
    nav_score <- nav[!is.na(in_score) & in_score, , drop = FALSE]
    if (nrow(nav_score) < 2) {
        .bt_stop("score_window_empty", sprintf("Scored window has <2 NAV points (n=%d)", nrow(nav_score)))
    }

    perf_score <- .bt_perf_from_nav(nav_score, rf_annual = rf)
    perf_full <- .bt_perf_from_nav(nav, rf_annual = rf)

    # costs
    execs <- run_artifact$executions
    total_cost <- if (is.data.frame(execs) && nrow(execs) > 0) sum(execs$fee + execs$slippage, na.rm = TRUE) else 0
    skips <- run_artifact$execution_skips
    n_skips <- if (is.data.frame(skips)) nrow(skips) else 0L

    list(
        performance_score = perf_score,
        performance_full = perf_full,
        total_costs = total_cost,
        n_exec_fills = if (is.data.frame(execs)) nrow(execs) else 0L,
        n_exec_skips = n_skips,
        n_decisions = if (is.data.frame(run_artifact$decisions)) nrow(run_artifact$decisions) else 0L,
        window = run_artifact$meta$window
    )
}

#' @export
bt_print_summary <- function(analytics) {
    p <- analytics$performance_score
    cat(sprintf(
        "SCORE: CAGR=%.2f%% Vol=%.2f%% Sharpe=%.2f MaxDD=%.2f%% Calmar=%.2f\n",
        100 * p$cagr, 100 * p$vol, p$sharpe, 100 * p$max_dd, p$calmar
    ))
    cat(sprintf("Score Period: %s to %s | obs=%d\n", as.character(p$start), as.character(p$end), p$n_obs))
    cat(sprintf(
        "Costs: %.2f | Decisions: %d | Fills: %d | Skips: %d\n",
        analytics$total_costs, analytics$n_decisions, analytics$n_exec_fills, analytics$n_exec_skips %||% 0L
    ))
}
