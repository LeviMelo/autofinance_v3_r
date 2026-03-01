#' @title Backtest Engine — Analytics
#' @description Performance metrics from BacktestRunArtifact.

#' @export
bt_compute_performance <- function(nav_series) {
    if (!is.data.frame(nav_series) || nrow(nav_series) < 2) {
        return(list(
            cagr = NA, vol = NA, sharpe = NA, max_dd = NA,
            calmar = NA, n_days = 0
        ))
    }

    nav <- nav_series$nav
    dates <- nav_series$date

    rets <- diff(log(nav))
    rets <- rets[is.finite(rets)]
    if (length(rets) < 2) {
        return(list(cagr = NA, vol = NA, sharpe = NA, max_dd = NA, calmar = NA, n_days = 0))
    }

    n_days <- as.numeric(max(dates) - min(dates))
    years <- n_days / 365.25
    if (years <= 0) years <- 1

    cagr <- (nav[length(nav)] / nav[1])^(1 / years) - 1
    vol <- sd(rets) * sqrt(252)
    sharpe <- if (vol > 0) cagr / vol else NA

    cum_max <- cummax(nav)
    drawdowns <- nav / cum_max - 1
    max_dd <- min(drawdowns)

    calmar <- if (!is.na(max_dd) && max_dd < 0) cagr / abs(max_dd) else NA

    list(
        cagr     = round(cagr, 6),
        vol      = round(vol, 6),
        sharpe   = round(sharpe, 4),
        max_dd   = round(max_dd, 6),
        calmar   = round(calmar, 4),
        n_days   = n_days,
        n_obs    = length(nav),
        start    = min(dates),
        end      = max(dates)
    )
}

#' @export
bt_compute_analytics <- function(run_artifact) {
    perf_full <- bt_compute_performance(run_artifact$nav_series)

    # Turnover summary
    to <- unlist(run_artifact$turnover_log)
    turnover_summary <- if (length(to) > 0) {
        list(
            mean_turnover   = mean(to, na.rm = TRUE),
            median_turnover = median(to, na.rm = TRUE),
            max_turnover    = max(to, na.rm = TRUE),
            total_turnover  = sum(to, na.rm = TRUE)
        )
    } else {
        list(mean_turnover = NA, median_turnover = NA, max_turnover = NA, total_turnover = NA)
    }

    # Cost summary
    cost_total <- 0
    if (length(run_artifact$cost_log) > 0) {
        cost_total <- sum(vapply(run_artifact$cost_log, function(c) c$total_cost %||% 0, numeric(1)))
    }

    # Post-ready window (based on first_ready_exec_date)
    rd <- run_artifact$meta$first_ready_exec_date %||% as.Date(NA)
    perf_post_ready <- NULL
    if (!is.na(rd) && is.data.frame(run_artifact$nav_series) && nrow(run_artifact$nav_series) > 1) {
        nav2 <- run_artifact$nav_series
        nav2 <- nav2[nav2$date >= rd, , drop = FALSE]
        if (nrow(nav2) >= 2) perf_post_ready <- bt_compute_performance(nav2)
    }

    list(
        performance_full = perf_full,
        performance_post_ready = perf_post_ready,
        first_ready_exec_date = rd,
        turnover = turnover_summary,
        total_costs = cost_total,
        n_rebalances = length(run_artifact$rebalance_log)
    )
}

#' @export
bt_print_summary <- function(analytics) {
    p <- analytics$performance_full
    cat(sprintf(
        "FULL: CAGR=%.2f%% Vol=%.2f%% Sharpe=%.2f MaxDD=%.2f%%\n",
        p$cagr * 100, p$vol * 100, p$sharpe, p$max_dd * 100
    ))
    cat(sprintf(
        "FULL Period: %s to %s (%d days, %d observations)\n",
        p$start, p$end, p$n_days, p$n_obs
    ))

    if (!is.null(analytics$performance_post_ready)) {
        pr <- analytics$performance_post_ready
        cat(sprintf(
            "POST-READY (from %s): CAGR=%.2f%% Vol=%.2f%% Sharpe=%.2f MaxDD=%.2f%%\n",
            as.character(analytics$first_ready_exec_date),
            pr$cagr * 100, pr$vol * 100, pr$sharpe, pr$max_dd * 100
        ))
    } else {
        cat("POST-READY: not available (model never became ready or insufficient points).\n")
    }

    cat(sprintf("Rebalances: %d | Total Costs: %.2f\n", analytics$n_rebalances, analytics$total_costs))
    t <- analytics$turnover
    cat(sprintf("Turnover: mean=%.4f median=%.4f max=%.4f\n", t$mean_turnover, t$median_turnover, t$max_turnover))
}
