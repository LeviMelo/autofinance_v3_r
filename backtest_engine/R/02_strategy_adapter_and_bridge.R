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
    # Build no-lookahead data slice
    dt_slice <- data_context$dt[refdate <= decision_date]

    # Extract previous target for carry-forward
    prev_target <- NULL
    if (!is.null(strategy_state_in) && !is.null(strategy_state_in$prev_target)) {
        prev_target <- strategy_state_in$prev_target
    }

    # Run model engine snapshot
    snap <- tryCatch(
        me_run_snapshot(dt_slice, decision_date,
            spec = strategy_spec,
            prev_target = prev_target
        ),
        error = function(e) {
            warning(
                sprintf(
                    "Model engine failed on %s: %s",
                    as.character(decision_date), e$message
                ),
                call. = FALSE
            )
            NULL
        }
    )

    if (is.null(snap)) {
        return(list(
            decision_date = decision_date,
            target_weights = data.frame(
                symbol = character(0),
                weight_target = numeric(0)
            ),
            cash_weight = 1.0,
            warnings = paste("Model engine failed on", decision_date),
            diagnostics = list(model_failed = TRUE),
            strategy_state_out = strategy_state_in,
            meta = list(strategy = "model_engine", failed = TRUE)
        ))
    }

    # Build state for carry-forward
    state_out <- list(prev_target = snap$target_weights)

    list(
        decision_date = decision_date,
        target_weights = snap$target_weights,
        cash_weight = snap$cash_weight,
        warnings = snap$warnings,
        diagnostics = list(
            risk_universe  = snap$tradable_symbols,
            gating         = snap$gating,
            portfolio_diag = snap$portfolio_diag
        ),
        strategy_state_out = state_out,
        meta = list(
            strategy  = "model_engine",
            spec_hash = snap$meta$spec_hash
        )
    )
}
