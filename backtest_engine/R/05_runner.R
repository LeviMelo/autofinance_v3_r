# backtest_engine/R/05_runner.R
# Backtest Engine v3 — strict BacktestWindow protocol, daily NAV, no warmups/policies.

.bt_build_window <- function(data_ctx, bt_spec, strategy_fn, strategy_spec, start_date, end_date) {
    cal <- bt_calendar(data_ctx)
    if (length(cal) < 2) .bt_stop("calendar_short", "Calendar too short")

    A_req <- as.Date(start_date)
    B_req <- as.Date(end_date)
    if (is.na(A_req) || is.na(B_req)) .bt_stop("range_dates", "start_date/end_date must be Date-coercible")
    if (A_req > B_req) .bt_stop("range_order", "start_date must be <= end_date")

    A0 <- .bt_snap_left(cal, A_req)
    B0 <- .bt_snap_right(cal, B_req)
    if (is.na(A0) || is.na(B0)) .bt_stop("range_outside_calendar", "Requested range outside available calendar")
    if (A0 > B0) .bt_stop("range_empty", "No trading dates inside requested range after snapping")

    req <- bt_strategy_requirements(strategy_fn, strategy_spec)
    W_model <- as.integer(req$prehistory_days)

    u <- bt_spec$universe
    W_univ <- as.integer(u$lookback_days %||% 0L)
    if (!is.null(u$min_price_history_days)) W_univ <- max(W_univ, as.integer(u$min_price_history_days))
    W_star <- max(W_model, W_univ)

    idxA <- match(A0, cal)
    if (is.na(idxA)) .bt_stop("range_internal", "A0 not in calendar (unexpected)")
    if (idxA - W_star < 1L) {
        .bt_stop(
            "insufficient_prehistory",
            sprintf(
                "Need %d trading days of prehistory before %s; calendar starts at %s",
                W_star, as.character(A0), as.character(cal[1])
            )
        )
    }
    S <- cal[idxA - W_star]

    exec_lag <- as.integer(bt_spec$execution$exec_lag %||% 1L)

    # decision schedule over [A0,B0]
    sched <- bt_rebalance_dates(cal, bt_spec$clock)
    sched <- sched[sched >= A0 & sched <= B0]

    if (length(sched) == 0) {
        decisions <- data.frame(decision_date = as.Date(character(0)), exec_date = as.Date(character(0)))
    } else {
        execs <- vapply(sched, function(d) as.character(bt_map_execution_date(cal, d, exec_lag)), character(1))
        execs <- as.Date(execs)
        ok <- !is.na(execs) & execs <= B0
        decisions <- data.frame(decision_date = sched[ok], exec_date = execs[ok], stringsAsFactors = FALSE)
    }

    B_last_decision <- if (nrow(decisions) > 0) max(decisions$decision_date) else as.Date(NA)
    B_last_exec <- if (nrow(decisions) > 0) max(decisions$exec_date) else as.Date(NA)

    status <- "OK"
    status_reason <- "window feasible"
    if (length(sched) > nrow(decisions)) {
        status <- "TRUNCATED_EXEC_TAIL"
        status_reason <- sprintf("Dropped %d decision date(s) near end due to missing execution date within score end", length(sched) - nrow(decisions))
    }
    if (length(sched) == 0) {
        status <- "NO_DECISIONS"
        status_reason <- "No rebalance dates in scored interval after applying clock frequency"
    }

    # enforce unique exec dates (one pending plan per exec day)
    if (nrow(decisions) > 0 && anyDuplicated(decisions$exec_date) > 0) {
        .bt_stop("exec_date_collision", "Two decisions map to the same execution date (unsupported)")
    }

    list(
        requested = list(start = A_req, end = B_req),
        effective = list(A0 = A0, B0 = B0),
        score = list(A_score = A0, B_score = B0, B_last_decision = B_last_decision, B_last_exec = B_last_exec),
        model_life = list(S = S, W_star = W_star, W_model = W_model, W_universe = W_univ),
        exec_lag = exec_lag,
        status = status,
        status_reason = status_reason,
        decisions = decisions,
        requirements = req
    )
}

.bt_assert_symbols_allowed <- function(target_symbols, base_universe, holdings, keep_holdings) {
    t <- unique(as.character(target_symbols))
    t <- t[!is.na(t) & nzchar(t)]
    if (length(t) == 0) {
        return(invisible(TRUE))
    }

    allow <- unique(as.character(base_universe))
    if (isTRUE(keep_holdings) && length(holdings) > 0) allow <- unique(c(allow, as.character(holdings)))

    bad <- setdiff(t, allow)
    if (length(bad) > 0) .bt_stop("proposal_outside_universe", paste("Target symbols not allowed:", paste(bad, collapse = ", ")))
    invisible(TRUE)
}

.bt_bootstrap_universe <- function(data_ctx, as_of_date, universe_spec) {
    d <- as.Date(as_of_date)
    if (is.na(d)) .bt_stop("date_invalid", "Invalid as_of_date")

    dt <- data_ctx$dt[refdate == d]
    if (nrow(dt) == 0) .bt_stop("universe_empty", paste("No rows on", as.character(d)))

    if ("asset_type" %in% names(dt)) {
        types <- universe_spec$include_types %||% NULL
        if (!is.null(types)) dt <- dt[asset_type %in% types]
    }

    dt <- dt[
        is.finite(close) & close > 0 &
            is.finite(traded_value) & traded_value > 0 &
            is.finite(n_trades) & n_trades > 0
    ]
    if (nrow(dt) == 0) .bt_stop("universe_empty", paste("Universe empty on", as.character(d), "during bootstrap"))

    maxu <- universe_spec$max_universe %||% NULL
    if (!is.null(maxu)) {
        maxu <- as.integer(maxu)
        dt <- dt[order(-traded_value)]
        dt <- head(dt, maxu)
    }

    u <- unique(as.character(dt$symbol))
    u <- u[!is.na(u) & nzchar(u)]
    if (length(u) < 1) .bt_stop("universe_empty", paste("Universe empty on", as.character(d), "during bootstrap"))
    u
}

#' @export
bt_run_backtest_range <- function(data_bundle_or_panel, strategy_fn, strategy_spec,
                                  start_date, end_date, bt_spec = NULL) {
    bt_spec <- bt_get_spec(bt_spec)
    data_ctx <- bt_make_data_context(data_bundle_or_panel)

    win <- .bt_build_window(data_ctx, bt_spec, strategy_fn, strategy_spec, start_date, end_date)
    cal <- bt_calendar(data_ctx)
    max_stale_mark_days <- if ("max_stale_mark_days" %in% names(bt_spec$accounting)) bt_spec$accounting$max_stale_mark_days else 5L

    S <- win$model_life$S
    B0 <- win$effective$B0
    A0 <- win$effective$A0

    # calendar path we actually simulate (model life to scored end)
    sim_dates <- cal[cal >= S & cal <= B0]
    if (length(sim_dates) < 2) .bt_stop("sim_dates_short", "Simulation date range too short")

    decisions <- win$decisions
    dec_map <- if (nrow(decisions) > 0) setNames(as.character(decisions$exec_date), as.character(decisions$decision_date)) else character(0)
    exec_to_dec <- if (nrow(decisions) > 0) setNames(as.character(decisions$decision_date), as.character(decisions$exec_date)) else character(0)

    req <- win$requirements
    stateful <- isTRUE(req$stateful)

    # state
    state <- bt_init_portfolio_state(bt_spec$accounting$initial_nav, bt_spec$accounting$initial_cash)
    strategy_state <- NULL
    pending <- list() # keyed by exec_date string -> proposal

    nav_rows <- vector("list", length(sim_dates))
    exec_rows <- list()
    skip_rows <- list()
    dec_rows <- list()

    verbose <- isTRUE(bt_spec$runner$verbose)
    if (verbose) {
        message(sprintf(
            "[BT] mark policy: %s | max_stale_mark_days=%s",
            bt_spec$accounting$mark_missing_policy %||% "carry_last",
            if (is.null(max_stale_mark_days)) "NULL" else as.character(max_stale_mark_days)
        ))
    }

    # initialize mark prices at S close
    mp0 <- .bt_prices_on_date_mark(
        data_ctx = data_ctx,
        cal = cal,
        date = S,
        field = bt_spec$accounting$mark_field,
        symbols = names(state$positions) %||% character(0),
        missing_policy = bt_spec$accounting$mark_missing_policy %||% "carry_last",
        max_stale_days = max_stale_mark_days
    )
    state <- bt_mark_to_market(state, mp0, S)

    for (i in seq_along(sim_dates)) {
        t <- as.Date(sim_dates[i])
        t_key <- as.character(t)

        # 1) execute at OPEN if pending exists for today
        if (t_key %in% names(exec_to_dec)) {
            if (!t_key %in% names(pending)) .bt_stop("pending_missing", paste("Missing pending proposal for exec date", t_key))
            proposal <- pending[[t_key]]
            pending[[t_key]] <- NULL

            # prices for all symbols that might be traded (union of current + target)
            cur_syms <- names(state$positions) %||% character(0)
            tgt_syms <- as.character(proposal$target_weights$symbol) %||% character(0)
            px_syms <- unique(c(cur_syms, tgt_syms))

            exec_px <- .bt_prices_on_date_available(data_ctx, t, bt_spec$execution$price_field, px_syms)
            exec_rep <- bt_execute_rebalance(state, proposal, exec_px, bt_spec$costs)
            state <- bt_apply_fills(state, exec_rep)
            skipped <- unique(as.character(exec_rep$skipped_symbols %||% character(0)))
            skipped <- skipped[!is.na(skipped) & nzchar(skipped)]
            if (length(skipped) > 0) {
                dd <- as.Date(exec_to_dec[[t_key]])
                for (sym in skipped) {
                    skip_rows[[length(skip_rows) + 1L]] <- data.frame(
                        decision_date = dd,
                        exec_date = t,
                        symbol = sym,
                        reason = "missing_exec_price",
                        stringsAsFactors = FALSE
                    )
                }
            }
            if (verbose && length(exec_rep$skipped_symbols %||% character(0)) > 0) {
                message(sprintf(
                    "[BT] exec %s skipped symbols without valid %s price: %s",
                    t_key, bt_spec$execution$price_field,
                    paste(unique(exec_rep$skipped_symbols), collapse = ", ")
                ))
            }

            # execution log rows (one per fill)
            if (nrow(exec_rep$fills) > 0) {
                cs <- exec_rep$costs$per_symbol
                cs_map_fee <- setNames(as.numeric(cs$fee), as.character(cs$symbol))
                cs_map_slp <- setNames(as.numeric(cs$slippage), as.character(cs$symbol))

                dd <- as.Date(exec_to_dec[[t_key]])
                fills <- exec_rep$fills
                for (k in seq_len(nrow(fills))) {
                    sym <- as.character(fills$symbol[k])
                    exec_rows[[length(exec_rows) + 1L]] <- data.frame(
                        decision_date = dd,
                        exec_date = t,
                        symbol = sym,
                        shares = as.numeric(fills$shares_filled[k]),
                        fill_price = as.numeric(fills$fill_price[k]),
                        notional = as.numeric(fills$notional[k]),
                        fee = as.numeric(cs_map_fee[[sym]] %||% 0),
                        slippage = as.numeric(cs_map_slp[[sym]] %||% 0),
                        stringsAsFactors = FALSE
                    )
                }
            }
        }

        # 2) mark-to-market at CLOSE and record daily NAV
        hold_syms <- names(state$positions) %||% character(0)
        mp <- if (length(hold_syms) > 0) {
            .bt_prices_on_date_mark(
                data_ctx = data_ctx,
                cal = cal,
                date = t,
                field = bt_spec$accounting$mark_field,
                symbols = hold_syms,
                missing_policy = bt_spec$accounting$mark_missing_policy %||% "carry_last",
                max_stale_days = max_stale_mark_days
            )
        } else {
            setNames(numeric(0), character(0))
        }
        state <- bt_mark_to_market(state, mp, t)

        nav_rows[[i]] <- data.frame(
            date = t,
            nav = state$nav,
            cash = state$cash,
            n_positions = length(state$positions),
            in_score = (t >= A0 & t <= B0),
            stringsAsFactors = FALSE
        )

        # 3) strategy update (stateful) on non-decision days (no trade)
        is_decision <- nrow(decisions) > 0 && (t %in% decisions$decision_date)
        if (stateful && !is_decision) {
            # Before score start, bootstrap state with a causal relaxed universe.
            # From A0 onward, enforce strict investability rules.
            base_u <- if (t < A0) {
                .bt_bootstrap_universe(data_ctx, t, bt_spec$universe)
            } else {
                bt_select_universe(data_ctx, t, bt_spec$universe)
            }
            ctx <- bt_build_strategy_context(
                data_ctx = data_ctx,
                decision_date = t,
                base_universe = base_u,
                holdings_symbols = names(state$positions) %||% character(0),
                keep_holdings = bt_spec$universe$keep_holdings %||% TRUE
            )

            runtime_ctx <- list(
                base_universe = base_u,
                tradability_mask = bt_tradability_mask(data_ctx, t, unique(c(base_u, names(state$positions) %||% character(0)))),
                no_trade = TRUE,
                bt_spec = bt_spec,
                window = win,
                event = "update"
            )

            prop <- bt_call_strategy(strategy_fn, t, ctx, state, strategy_state, strategy_spec, runtime_ctx)
            bt_validate_proposal(prop, tol = 1e-8)
            strategy_state <- prop$strategy_state_out
        }

        # 4) decision at CLOSE: schedule execution in the future (if in decision set)
        if (is_decision) {
            ed <- as.Date(decisions$exec_date[match(t, decisions$decision_date)])
            ed_key <- as.character(ed)
            if (is.na(ed)) .bt_stop("exec_date_na", paste("No exec date for decision", t_key))
            if (ed < t) .bt_stop("exec_date_order", "exec_date must be >= decision_date")
            if (ed_key %in% names(pending)) .bt_stop("pending_collision", paste("Pending already exists for exec date", ed_key))

            base_u <- bt_select_universe(data_ctx, t, bt_spec$universe)
            hold_syms <- names(state$positions) %||% character(0)
            ctx <- bt_build_strategy_context(
                data_ctx = data_ctx,
                decision_date = t,
                base_universe = base_u,
                holdings_symbols = hold_syms,
                keep_holdings = bt_spec$universe$keep_holdings %||% TRUE
            )

            runtime_ctx <- list(
                base_universe = base_u,
                tradability_mask = bt_tradability_mask(data_ctx, t, unique(c(base_u, hold_syms))),
                no_trade = FALSE,
                bt_spec = bt_spec,
                window = win,
                event = "decision",
                exec_date = ed
            )

            prop <- bt_call_strategy(strategy_fn, t, ctx, state, strategy_state, strategy_spec, runtime_ctx)
            bt_validate_proposal(prop, tol = 1e-8)

            # enforce allowed symbols
            .bt_assert_symbols_allowed(prop$target_weights$symbol, base_u, hold_syms, bt_spec$universe$keep_holdings %||% TRUE)

            # attach nav basis for sizing at execution
            if (is.null(prop$meta)) prop$meta <- list()
            prop$meta$nav_basis <- state$nav

            strategy_state <- prop$strategy_state_out
            pending[[ed_key]] <- prop

            # decision log row
            dec_rows[[length(dec_rows) + 1L]] <- data.frame(
                decision_date = t,
                exec_date = ed,
                nav_basis = state$nav,
                cash = state$cash,
                n_universe = length(base_u),
                n_holdings = length(hold_syms),
                n_targets = nrow(prop$target_weights),
                stringsAsFactors = FALSE
            )

            if (verbose) {
                message(sprintf(
                    "[BT] decision %s -> exec %s | NAV=%.0f Cash=%.0f U=%d Targets=%d",
                    t_key, ed_key, state$nav, state$cash, length(base_u), nrow(prop$target_weights)
                ))
            }
        }
    }

    nav <- do.call(rbind, nav_rows)
    exec_tbl <- if (length(exec_rows) > 0) {
        do.call(rbind, exec_rows)
    } else {
        data.frame(
            decision_date = as.Date(character(0)), exec_date = as.Date(character(0)),
            symbol = character(0), shares = numeric(0), fill_price = numeric(0), notional = numeric(0),
            fee = numeric(0), slippage = numeric(0), stringsAsFactors = FALSE
        )
    }
    dec_tbl <- if (length(dec_rows) > 0) {
        do.call(rbind, dec_rows)
    } else {
        data.frame(
            decision_date = as.Date(character(0)), exec_date = as.Date(character(0)),
            nav_basis = numeric(0), cash = numeric(0), n_universe = integer(0), n_holdings = integer(0), n_targets = integer(0),
            stringsAsFactors = FALSE
        )
    }
    skip_tbl <- if (length(skip_rows) > 0) {
        do.call(rbind, skip_rows)
    } else {
        data.frame(
            decision_date = as.Date(character(0)),
            exec_date = as.Date(character(0)),
            symbol = character(0),
            reason = character(0),
            stringsAsFactors = FALSE
        )
    }

    res <- list(
        nav = nav,
        decisions = dec_tbl,
        executions = exec_tbl,
        execution_skips = skip_tbl,
        meta = list(
            window = win,
            bt_spec = bt_spec,
            strategy_spec = strategy_spec,
            final_nav = tail(nav$nav, 1),
            final_cash = tail(nav$cash, 1),
            final_positions = tail(nav$n_positions, 1)
        )
    )

    bt_validate_run_artifact(res)
    res
}

# Backward-compatible wrapper: uses full available calendar.
#' @export
bt_run_backtest <- function(data_bundle_or_panel, strategy_fn, strategy_spec, bt_spec = NULL) {
    data_ctx <- bt_make_data_context(data_bundle_or_panel)
    cal <- bt_calendar(data_ctx)
    bt_run_backtest_range(
        data_bundle_or_panel = data_bundle_or_panel,
        strategy_fn = strategy_fn,
        strategy_spec = strategy_spec,
        start_date = min(cal),
        end_date = max(cal),
        bt_spec = bt_spec
    )
}
