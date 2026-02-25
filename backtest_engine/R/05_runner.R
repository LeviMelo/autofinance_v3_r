#' @title Backtest Engine — Runner
#' @description Walk-forward simulation loop with stateful strategy support.

#' @export
bt_run_backtest <- function(data_bundle_or_panel, strategy_fn,
                            strategy_spec, bt_spec = NULL) {
    bt_spec <- bt_get_spec(bt_spec)
    bt_validate_spec(bt_spec)

    data_ctx <- bt_make_data_context(data_bundle_or_panel)
    cal <- bt_calendar(data_ctx)
    rebal_dates <- bt_rebalance_dates(cal, bt_spec$clock)

    if (length(rebal_dates) == 0) {
        warning("No rebalance dates in calendar", call. = FALSE)
        res <- list(
            nav_series = data.frame(
                date = as.Date(character(0)),
                nav = numeric(0)
            ),
            rebalance_log = list(),
            execution_log = list(),
            target_log = list(),
            cost_log = list(),
            warnings = "No rebalance dates",
            meta = list(spec = bt_spec, strategy_spec = strategy_spec)
        )
        bt_validate_run_artifact(res)
        return(res)
    }

    # ── Initialize ──
    state <- bt_init_portfolio_state(
        initial_nav  = bt_spec$accounting$initial_nav,
        initial_cash = bt_spec$accounting$initial_cash
    )

    strategy_state <- NULL
    nav_history <- list()
    rebal_log <- list()
    exec_log <- list()
    target_log <- list()
    cost_log <- list()
    turnover_log <- list()
    all_warnings <- character(0)

    verbose <- isTRUE(bt_spec$runner$verbose)
    robust <- identical(bt_spec$runner$mode, "robust")

    # ── Walk-forward loop ──
    for (i in seq_along(rebal_dates)) {
        decision_date <- rebal_dates[i]

        # Map to execution date
        exec_date <- bt_map_execution_date(cal, decision_date, bt_spec$execution)
        if (is.na(exec_date)) {
            if (verbose) {
                message(sprintf(
                    "  [%s] No execution date available, skipping",
                    decision_date
                ))
            }
            next
        }

        dd_key <- as.character(decision_date)
        ed_key <- as.character(exec_date)

        # Mark to market at decision time (using latest available close)
        mark_p <- bt_get_mark_prices(
            data_ctx, decision_date,
            bt_spec$accounting$mark_field
        )
        state <- bt_mark_to_market(state, mark_p, decision_date)
        weights_before <- bt_compute_weights(state)

        # ── Call strategy ──
        proposal <- tryCatch(
            strategy_fn(
                decision_date, data_ctx, state,
                strategy_state, strategy_spec
            ),
            error = function(e) {
                all_warnings <<- c(
                    all_warnings,
                    sprintf("[%s] Strategy failed: %s", dd_key, e$message)
                )
                NULL
            }
        )

        if (is.null(proposal)) {
            if (robust) next else stop(sprintf("Strategy failed on %s", dd_key))
        }

        # Carry forward strategy state
        strategy_state <- proposal$strategy_state_out

        # Validate proposal
        tryCatch(bt_validate_proposal(proposal), error = function(e) {
            all_warnings <<- c(
                all_warnings,
                sprintf("[%s] Proposal invalid: %s", dd_key, e$message)
            )
            if (!robust) stop(e)
        })

        # ── Execute ──
        exec_prices <- bt_get_exec_prices(
            data_ctx, exec_date,
            bt_spec$execution$price_field,
            proposal$target_weights$symbol
        )

        exec_report <- bt_execute_rebalance(
            state, proposal, exec_prices,
            bt_spec$execution, bt_spec$costs
        )
        exec_report$execution_date <- exec_date

        # ── Update accounting ──
        state <- bt_apply_fills(state, exec_report)

        # MTM at execution date
        mark_exec <- bt_get_mark_prices(
            data_ctx, exec_date,
            bt_spec$accounting$mark_field
        )
        state <- bt_mark_to_market(state, mark_exec, exec_date)

        weights_after <- bt_compute_weights(state)
        turnover <- bt_compute_turnover(weights_before, weights_after)

        # ── Log ──
        nav_history[[ed_key]] <- state$nav
        rebal_log[[ed_key]] <- list(
            decision_date = decision_date,
            exec_date = exec_date,
            nav = state$nav, cash = state$cash,
            n_positions = length(state$positions)
        )
        exec_log[[ed_key]] <- exec_report
        target_log[[ed_key]] <- proposal$target_weights
        cost_log[[ed_key]] <- exec_report$costs
        turnover_log[[ed_key]] <- turnover

        if (length(proposal$warnings) > 0) {
            all_warnings <- c(all_warnings, proposal$warnings)
        }

        if (verbose && (i %% 5 == 0 || i == length(rebal_dates))) {
            message(sprintf(
                "  Rebalance %d/%d [%s→%s] NAV=%.0f Cash=%.0f Pos=%d TO=%.4f",
                i, length(rebal_dates), dd_key, ed_key,
                state$nav, state$cash, length(state$positions), turnover
            ))
        }
    }

    # ── Build NAV series ──
    nav_vals <- unlist(nav_history)
    if (length(nav_vals) > 0) {
        nav_df <- data.frame(
            date = as.Date(names(nav_vals)),
            nav = unname(nav_vals),
            stringsAsFactors = FALSE
        )
    } else {
        nav_df <- data.frame(date = as.Date(character(0)), nav = numeric(0))
    }

    # ── Assemble artifact ──
    res <- list(
        nav_series = nav_df,
        rebalance_log = rebal_log,
        execution_log = exec_log,
        target_log = target_log,
        cost_log = cost_log,
        turnover_log = turnover_log,
        warnings = unique(all_warnings),
        meta = list(
            spec = bt_spec,
            strategy_spec = strategy_spec,
            n_rebalances = length(rebal_log),
            final_nav = state$nav,
            final_cash = state$cash,
            final_positions = length(state$positions)
        )
    )

    bt_validate_run_artifact(res)
    res
}
