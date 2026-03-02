# backtest_engine/R/02_strategy_adapter_and_bridge.R
# Backtest Engine v3 — strict proposals (must sum to 1), adapters.

#' @export
bt_strategy_all_cash <- function(decision_date, data_context,
                                 portfolio_state, strategy_state_in,
                                 strategy_spec, runtime_ctx = list()) {
    d <- as.Date(decision_date)
    list(
        decision_date = d,
        target_weights = data.frame(symbol = character(0), weight_target = numeric(0)),
        cash_weight = 1.0,
        strategy_state_out = strategy_state_in,
        meta = list(strategy = "all_cash")
    )
}

# You MUST set requirements for non-CAME custom strategies:
# attr(fn, "bt_requirements") <- list(model_id="...", prehistory_days=..., stateful=FALSE, supports_replay=TRUE)

#' @export
bt_strategy_equal_weight <- function(decision_date, data_context,
                                     portfolio_state, strategy_state_in,
                                     strategy_spec, runtime_ctx = list()) {
    d <- as.Date(decision_date)
    syms <- unique(as.character(data_context$universe %||% character(0)))
    syms <- syms[!is.na(syms) & nzchar(syms)]
    if (length(syms) < 1) .bt_stop("equal_weight_empty", paste("No universe symbols on", as.character(d)))

    w <- rep(1 / length(syms), length(syms))
    list(
        decision_date = d,
        target_weights = data.frame(symbol = syms, weight_target = w, stringsAsFactors = FALSE),
        cash_weight = 0.0,
        strategy_state_out = strategy_state_in,
        meta = list(strategy = "equal_weight")
    )
}

# ── CAME adapter (strict trading: runner controls trade/no_trade) ────────────
# We do NOT change model_engine_came internals.
# We configure the model in a deterministic way for backtests:
# - meta$no_trade is controlled by runtime_ctx$no_trade
# - meta$strict is set to FALSE (required for cold-start bootstrapping; no trades happen during no_trade days)
# - forecast$cold_start_policy = "skip" (required for bootstrapping; runner forbids trading when no_trade=TRUE)

#' @export
bt_strategy_from_model_engine <- function(decision_date, data_context,
                                          portfolio_state, strategy_state_in,
                                          strategy_spec, runtime_ctx = list()) {
    d <- as.Date(decision_date)
    if (is.na(d)) .bt_stop("came_date", "Invalid decision_date")

    # restore state
    came_state <- NULL
    last_model_date <- as.Date(NA)
    if (!is.null(strategy_state_in) && is.list(strategy_state_in)) {
        came_state <- strategy_state_in$came_state %||% NULL
        last_model_date <- as.Date(strategy_state_in$last_model_date %||% NA)
    }
    if (is.null(came_state)) came_state <- came_state_init()

    # prev_target from ACTUAL holdings (executed reality)
    w_hold <- bt_compute_weights(portfolio_state)
    prev_target <- data.frame(
        symbol = names(w_hold),
        weight_target = as.numeric(w_hold),
        stringsAsFactors = FALSE
    )
    prev_target <- prev_target[is.finite(prev_target$weight_target) & prev_target$weight_target > (strategy_spec$data$eps_hold %||% 1e-8), , drop = FALSE]

    # configure spec for this call
    spec_d <- strategy_spec
    if (is.null(spec_d$meta)) spec_d$meta <- list()

    no_trade <- isTRUE(runtime_ctx$no_trade %||% FALSE)
    spec_d$meta$no_trade <- no_trade

    # required to let the model bootstrap history before the scored trading starts
    spec_d$meta$strict <- FALSE
    spec_d$forecast$cold_start_policy <- "skip"

    snap <- came_run_snapshot(
        data_bundle_or_panel = data_context$dt,
        as_of_date = d,
        spec = spec_d,
        state = came_state,
        prev_target = prev_target
    )

    # output proposal (weights already sum to 1 by CAME contract)
    list(
        decision_date = d,
        target_weights = snap$weights,
        cash_weight = snap$cash_weight,
        strategy_state_out = list(came_state = snap$state_out, last_model_date = d),
        meta = list(strategy = "came", no_trade = no_trade)
    )
}
