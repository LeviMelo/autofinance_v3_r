#' @title Backtest Engine — Runner
#' @description Walk-forward simulation loop with stateful strategy support,
#'              backtester-owned universe selection and readiness policies.

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
            nav_series = data.frame(date = as.Date(character(0)), nav = numeric(0)),
            rebalance_log = list(),
            execution_log = list(),
            target_log = list(),
            cost_log = list(),
            turnover_log = list(),
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
    readiness_log <- list()
    universe_log <- list()
    all_warnings <- character(0)

    verbose <- isTRUE(bt_spec$runner$verbose)
    robust <- identical(bt_spec$runner$mode, "robust")

    first_ready_decision_date <- as.Date(NA)
    first_ready_exec_date <- as.Date(NA)

    # ── Walk-forward loop (rebalance dates) ──
    for (i in seq_along(rebal_dates)) {
        decision_date <- as.Date(rebal_dates[i])

        # Map to execution date
        exec_date <- bt_map_execution_date(cal, decision_date, bt_spec$execution)
        if (is.na(exec_date)) {
            if (verbose) message(sprintf("  [%s] No execution date available, skipping", decision_date))
            next
        }

        dd_key <- as.character(decision_date)
        ed_key <- as.character(exec_date)

        # Mark to market at decision time (using close by default)
        mark_p <- bt_get_mark_prices(data_ctx, decision_date, bt_spec$accounting$mark_field)
        state <- bt_mark_to_market(state, mark_p, decision_date)
        weights_before <- bt_compute_weights(state)

        holdings_syms <- names(state$positions)

        # ── Backtester-owned base universe selection U(t) ──
        ures <- bt_select_universe(data_ctx, decision_date, bt_spec$universe)
        base_univ <- ures$universe
        universe_log[[ed_key]] <- ures

        # Build strategy-visible context: no-lookahead slice AND universe AND holdings carry
        data_ctx_strategy <- bt_build_strategy_context(
            data_ctx = data_ctx,
            decision_date = decision_date,
            base_universe = base_univ,
            holdings_symbols = holdings_syms,
            keep_holdings = bt_spec$universe$keep_holdings %||% TRUE
        )

        # Optional: tradability mask (for diagnostics / model use)
        trad_mask <- bt_tradability_mask(data_ctx, decision_date, unique(c(base_univ, holdings_syms)))

        runtime_ctx <- list(
            base_universe = base_univ,
            universe_status = ures$status,
            universe_reason = ures$reason,
            universe_diag = ures$diag,
            tradability_mask = trad_mask,
            exec_date = exec_date,
            bt_spec = bt_spec
        )

        # ── Call strategy (model) ──
        proposal <- tryCatch(
            bt_call_strategy(
                strategy_fn = strategy_fn,
                decision_date = decision_date,
                data_context = data_ctx_strategy,
                portfolio_state = state,
                strategy_state_in = strategy_state,
                strategy_spec = strategy_spec,
                runtime_ctx = runtime_ctx
            ),
            error = function(e) {
                all_warnings <<- c(all_warnings, sprintf("[%s] Strategy failed: %s", dd_key, e$message))
                NULL
            }
        )

        if (is.null(proposal)) {
            if (robust) next else stop(sprintf("Strategy failed on %s", dd_key))
        }

        # Carry forward strategy state (always)
        strategy_state <- proposal$strategy_state_out

        # Validate proposal shape
        tryCatch(bt_validate_proposal(proposal), error = function(e) {
            all_warnings <<- c(all_warnings, sprintf("[%s] Proposal invalid: %s", dd_key, e$message))
            if (!robust) stop(e)
        })

        # ── Backtester readiness + policy enforcement ──
        # Universe warmup is also a "warmup condition" regardless of model status.
        rd <- bt_assess_readiness(proposal)
        stage <- rd$stage
        status <- rd$status
        reason <- rd$reason

        if (!identical(ures$status, "OK")) {
            stage <- "warmup"
            status <- "WARMUP"
            reason <- paste0("universe_", ures$status, ": ", ures$reason)
            proposal$meta$status <- "WARMUP"
            proposal$meta$ready_to_trade <- FALSE
            proposal$meta$reason <- reason
        }

        # Apply policy
        if (!isTRUE(proposal$meta$ready_to_trade %||% rd$ready)) {
            if (identical(stage, "warmup")) {
                pa <- bt_spec$policies$warmup$action %||% "hold_cash"
                bs <- bt_spec$policies$warmup$baseline_strategy %||% "all_cash"
                proposal <- bt_apply_policy(proposal, pa, bs, decision_date, data_ctx_strategy, state, strategy_spec, runtime_ctx)
            } else if (identical(stage, "cold_start")) {
                pa <- bt_spec$policies$cold_start$action %||% "hold_cash"
                bs <- bt_spec$policies$cold_start$baseline_strategy %||% "all_cash"
                proposal <- bt_apply_policy(proposal, pa, bs, decision_date, data_ctx_strategy, state, strategy_spec, runtime_ctx)
            } else {
                # unknown not-ready treated as warmup for safety
                pa <- bt_spec$policies$warmup$action %||% "hold_cash"
                bs <- bt_spec$policies$warmup$baseline_strategy %||% "all_cash"
                proposal <- bt_apply_policy(proposal, pa, bs, decision_date, data_ctx_strategy, state, strategy_spec, runtime_ctx)
            }
        } else {
            proposal$meta$bt_override <- FALSE
            proposal$meta$bt_override_action <- "none"
        }

        # Normalize (backtester-owned)
        proposal <- bt_normalize_proposal(proposal, tol = bt_spec$policies$weight_tol %||% 1e-6)

        # Record first readiness (model readiness, not override)
        if (is.na(first_ready_decision_date)) {
            if (isTRUE(rd$ready) && identical(rd$status, "OK")) {
                first_ready_decision_date <- decision_date
                first_ready_exec_date <- exec_date
            }
        }

        readiness_log[[ed_key]] <- list(
            decision_date = decision_date,
            execution_date = exec_date,
            model_status = rd$status,
            model_ready = rd$ready,
            model_reason = rd$reason,
            bt_override = proposal$meta$bt_override %||% FALSE,
            bt_override_action = proposal$meta$bt_override_action %||% "none",
            universe_status = ures$status,
            n_universe = length(base_univ),
            n_holdings = length(holdings_syms),
            n_tradable = sum(trad_mask[base_univ] %||% FALSE, na.rm = TRUE)
        )

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

        # MTM at execution date (close)
        mark_exec <- bt_get_mark_prices(data_ctx, exec_date, bt_spec$accounting$mark_field)
        state <- bt_mark_to_market(state, mark_exec, exec_date)

        weights_after <- bt_compute_weights(state)
        turnover <- bt_compute_turnover(weights_before, weights_after)

        # ── Log ──
        nav_history[[ed_key]] <- state$nav
        rebal_log[[ed_key]] <- list(
            decision_date = decision_date,
            exec_date = exec_date,
            nav = state$nav,
            cash = state$cash,
            n_positions = length(state$positions),
            proposal_status = proposal$meta$status %||% NA_character_,
            bt_override = proposal$meta$bt_override %||% FALSE,
            bt_override_action = proposal$meta$bt_override_action %||% "none",
            universe_status = ures$status,
            n_universe = length(base_univ)
        )
        exec_log[[ed_key]] <- exec_report
        target_log[[ed_key]] <- proposal$target_weights
        cost_log[[ed_key]] <- exec_report$costs
        turnover_log[[ed_key]] <- turnover

        if (length(proposal$warnings) > 0) all_warnings <- c(all_warnings, proposal$warnings)

        if (verbose && (i %% 5 == 0 || i == length(rebal_dates))) {
            message(sprintf(
                "  Rebalance %d/%d [%s→%s] NAV=%.0f Cash=%.0f Pos=%d TO=%.4f U=%d Override=%s",
                i, length(rebal_dates), dd_key, ed_key,
                state$nav, state$cash, length(state$positions), turnover,
                length(base_univ),
                as.character(proposal$meta$bt_override %||% FALSE)
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
        readiness_log = readiness_log,
        universe_log = universe_log,
        warnings = unique(all_warnings),
        meta = list(
            spec = bt_spec,
            strategy_spec = strategy_spec,
            n_rebalances = length(rebal_log),
            final_nav = state$nav,
            final_cash = state$cash,
            final_positions = length(state$positions),
            first_ready_decision_date = first_ready_decision_date,
            first_ready_exec_date = first_ready_exec_date
        )
    )

    bt_validate_run_artifact(res)
    res
}
