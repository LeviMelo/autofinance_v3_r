#' @title Backtest Engine — Strategy Adapter and Bridge
#' @description Model-agnostic strategy interface with built-in adapters.

# ── Equal-weight adapter ──────────────────────────────────────────────────────

#' @export
bt_strategy_equal_weight <- function(decision_date, data_context,
                                     portfolio_state, strategy_state_in,
                                     strategy_spec, runtime_ctx = list()) {
    # Get tradable symbols as of decision date
    sub <- data_context$dt[refdate == decision_date]
    syms <- unique(sub$symbol)
    syms <- syms[!is.na(syms) & nchar(syms) > 0]

    if (length(syms) == 0) {
        return(list(
            decision_date = decision_date,
            target_weights = data.frame(
                symbol = character(0),
                weight_target = numeric(0)
            ),
            cash_weight = 1.0,
            warnings = "No symbols on decision date",
            diagnostics = list(),
            strategy_state_out = strategy_state_in,
            meta = list()
        ))
    }

    w <- rep(1 / length(syms), length(syms))
    names(w) <- syms

    list(
        decision_date = decision_date,
        target_weights = data.frame(
            symbol = syms, weight_target = w,
            stringsAsFactors = FALSE
        ),
        cash_weight = 0.0,
        warnings = character(0),
        diagnostics = list(n_symbols = length(syms)),
        strategy_state_out = strategy_state_in,
        meta = list(strategy = "equal_weight")
    )
}

# ── All-cash adapter ──────────────────────────────────────────────────────────

#' @export
bt_strategy_all_cash <- function(decision_date, data_context,
                                 portfolio_state, strategy_state_in,
                                 strategy_spec, runtime_ctx = list()) {
    list(
        decision_date = decision_date,
        target_weights = data.frame(
            symbol = character(0),
            weight_target = numeric(0)
        ),
        cash_weight = 1.0,
        warnings = character(0),
        diagnostics = list(),
        strategy_state_out = strategy_state_in,
        meta = list(strategy = "all_cash")
    )
}

# ── Model-engine adapter ─────────────────────────────────────────────────────

#' @export
bt_strategy_from_model_engine <- function(decision_date, data_context,
                                          portfolio_state, strategy_state_in,
                                          strategy_spec, runtime_ctx = list()) {
    decision_date <- as.Date(decision_date)

    # ---- initialize / restore CAME state ----
    came_state <- NULL
    last_model_date <- as.Date(NA)
    prev_target <- NULL

    if (!is.null(strategy_state_in) && is.list(strategy_state_in)) {
        if (!is.null(strategy_state_in$came_state)) came_state <- strategy_state_in$came_state
        if (!is.null(strategy_state_in$last_model_date)) last_model_date <- as.Date(strategy_state_in$last_model_date)
        if (!is.null(strategy_state_in$prev_target)) prev_target <- strategy_state_in$prev_target
    }
    if (is.null(came_state)) came_state <- came_state_init()

    # ---- build trading calendar up to decision_date ----
    cal <- sort(unique(data_context$dt$refdate))
    cal <- cal[cal <= decision_date]
    if (length(cal) == 0) {
        return(list(
            decision_date = decision_date,
            target_weights = data.frame(symbol = character(0), weight_target = numeric(0)),
            cash_weight = 1.0,
            warnings = "No calendar data up to decision_date",
            diagnostics = list(),
            strategy_state_out = list(came_state = came_state, last_model_date = last_model_date, prev_target = prev_target),
            meta = list(strategy = "came", failed = TRUE)
        ))
    }

    # ---- prevent early failure: only start once CAME has enough lookback ----
    L <- as.integer(strategy_spec$risk$lookback %||% 252L)
    H <- as.integer(strategy_spec$forecast$H %||% 21L)
    mom_h <- strategy_spec$signals$mom_horizons %||% c(21L, 63L, 126L, 252L)

    need_days <- max(L, max(as.integer(mom_h)), 63L) + H + 5L + 1L
    if (length(cal) < need_days) {
        return(list(
            decision_date = decision_date,
            target_weights = data.frame(symbol = character(0), weight_target = numeric(0)),
            cash_weight = 1.0,
            warnings = sprintf("Not enough warmup days for CAME (have %d need %d)", length(cal), need_days),
            diagnostics = list(have_days = length(cal), need_days = need_days),
            strategy_state_out = list(came_state = came_state, last_model_date = last_model_date, prev_target = prev_target),
            meta = list(strategy = "came", failed = TRUE)
        ))
    }

    first_run_date <- cal[need_days]

    # ---- decide which days to run (catch-up) ----
    if (is.na(last_model_date) || last_model_date < first_run_date) {
        run_dates <- cal[cal >= first_run_date]
    } else {
        run_dates <- cal[cal > last_model_date]
    }
    run_dates <- run_dates[run_dates <= decision_date]

    if (length(run_dates) == 0) {
        # nothing to do; just return previous target (or cash)
        tw <- prev_target
        if (is.null(tw)) tw <- data.frame(symbol = character(0), weight_target = numeric(0))
        cw <- 1.0
        if (is.data.frame(tw) && nrow(tw) > 0) {
            s <- sum(tw$weight_target, na.rm = TRUE)
            if (is.finite(s)) cw <- max(0, 1 - s)
        }
        return(list(
            decision_date = decision_date,
            target_weights = tw,
            cash_weight = cw,
            warnings = character(0),
            diagnostics = list(catchup_days = 0L),
            strategy_state_out = list(came_state = came_state, last_model_date = last_model_date, prev_target = prev_target),
            meta = list(strategy = "came", reused = TRUE)
        ))
    }

    # ---- for turnover/frozen-carry correctness, use ACTUAL holdings as prev_target on the trade day ----
    # decision is made after close; portfolio_state has mark_prices at decision_date in bt_run_backtest
    w_hold <- bt_compute_weights(portfolio_state)
    prev_target_trade <- data.frame(
        symbol = names(w_hold),
        weight_target = as.numeric(w_hold),
        stringsAsFactors = FALSE
    )
    prev_target_trade <- prev_target_trade[is.finite(prev_target_trade$weight_target) & prev_target_trade$weight_target > 0, , drop = FALSE]

    # ---- run catch-up days in no-trade mode, final day in trade mode ----
    snap <- NULL
    n_catch <- 0L

    for (d in run_dates) {
        is_last <- identical(as.Date(d), decision_date)

        spec_d <- strategy_spec
        if (is.null(spec_d$meta)) spec_d$meta <- list()

        # Ensure backtest never hard-stops on cold start
        spec_d$meta$strict <- FALSE
        spec_d$forecast$cold_start_policy <- "skip"

        spec_d$meta$no_trade <- !is_last

        # use prev_target only on the trade day (to reflect real holdings);
        # Warmup/catch-up days run in no-trade mode and should not inject portfolio constraints.
        pt <- if (is_last) prev_target_trade else NULL

        snap <- tryCatch(
            came_run_snapshot(
                data_bundle_or_panel = data_context$dt,
                as_of_date = d,
                spec = spec_d,
                state = came_state,
                prev_target = pt
            ),
            error = function(e) e
        )

        if (inherits(snap, "error")) {
            return(list(
                decision_date = decision_date,
                target_weights = data.frame(symbol = character(0), weight_target = numeric(0)),
                cash_weight = 1.0,
                warnings = paste("CAME failed on", as.character(d), ":", conditionMessage(snap)),
                diagnostics = list(failed_date = as.character(d)),
                strategy_state_out = list(came_state = came_state, last_model_date = last_model_date, prev_target = prev_target),
                meta = list(strategy = "came", failed = TRUE)
            ))
        }

        came_state <- snap$state_out
        last_model_date <- as.Date(d)
        n_catch <- n_catch + 1L
    }

    # snap is decision_date output
    # Optional: do not trade until forecast has a real panel fit
    cold <- isTRUE(snap$forecast$diag$cold_start %||% FALSE)
    panel_n <- snap$forecast$diag$panel_n %||% 0L

    if (cold || panel_n == 0L) {
        # keep cash until fitted (recommended for realistic backtests)
        target_weights <- data.frame(symbol = character(0), weight_target = numeric(0))
        cash_weight <- 1.0
        warn <- sprintf("CAME cold-start: panel_n=%d; staying in cash", as.integer(panel_n))
    } else {
        target_weights <- snap$weights
        cash_weight <- snap$cash_weight
        warn <- character(0)
    }

    # persist prev_target as the EXECUTED target (not the warmup internals)
    prev_target <- target_weights

    list(
        decision_date = decision_date,
        target_weights = target_weights,
        cash_weight = cash_weight,
        warnings = warn,
        diagnostics = list(
            catchup_days = n_catch,
            panel_n = panel_n,
            cold_start = cold,
            gating_pi = snap$forecast$pi,
            m_t = snap$m_t
        ),
        strategy_state_out = list(
            came_state = came_state,
            last_model_date = last_model_date,
            prev_target = prev_target
        ),
        meta = list(strategy = "came", spec_hash = snap$meta$spec_hash)
    )
}
