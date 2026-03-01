#' @title Backtest Engine — Contracts and Spec
#' @description BacktestSpec defaults, validation, and artifact contracts.

# ── Helpers ──────────────────────────────────────────────────────────────────

#' @export
`%||%` <- function(a, b) if (is.null(a)) b else a

.bt_is_scalar_num <- function(x) is.numeric(x) && length(x) == 1L && is.finite(x)
.bt_is_scalar_int <- function(x) (is.numeric(x) || is.integer(x)) && length(x) == 1L && is.finite(x)

# ── BacktestSpec default ─────────────────────────────────────────────────────
# Backtester owns: clock, universe, policies, execution, costs, accounting.

#' @export
bt_spec_default <- function() {
    list(
        # ── clock section ──
        clock = list(
            freq = "months" # "months", "weeks", "days", or integer (every N days)
        ),

        # ── universe section (BASE opportunity set U(t)) ──
        # Must be causal (uses only data <= decision_date).
        universe = list(
            # trailing window used for liquidity/coverage filters
            lookback_days = 63L,

            # require trading on at least this fraction of the lookback window
            min_days_traded_ratio = 0.75,

            # median traded value threshold over lookback window
            min_median_traded_value = 5e5,

            # allowed asset types if column exists; otherwise ignored
            include_types = c("equity", "fii", "etf", "bdr"),

            # optional hard cap: keep top N by median traded value (after filters)
            max_universe = NULL,

            # require strict close price history on trailing window (for comparability)
            # If NULL: do not enforce rectangular price history at the backtester layer.
            min_price_history_days = 252L,

            # always include current holdings symbols in strategy-visible data slice
            keep_holdings = TRUE
        ),

        # ── policies section ──
        # Backtester decides what happens when model isn't ready.
        policies = list(
            warmup = list(
                action = "hold_cash", # "hold_cash" | "baseline" | "allow_trade"
                baseline_strategy = "all_cash" # "all_cash" | "equal_weight"
            ),
            cold_start = list(
                action = "hold_cash", # "hold_cash" | "baseline" | "allow_trade"
                baseline_strategy = "all_cash"
            ),

            # normalization tolerance for proposals
            weight_tol = 1e-6
        ),

        # ── execution section ──
        execution = list(
            price_field   = "open", # field for execution prices
            exec_lag      = 1L, # +1 day from decision to execution
            fill_policy   = "full" # "full" fill assumption
        ),

        # ── costs section ──
        costs = list(
            fee_rate      = 0.0003, # proportional fee
            slippage_rate = 0.0010 # proportional slippage
        ),

        # ── accounting section ──
        accounting = list(
            initial_nav   = 100000,
            initial_cash  = 100000,
            mark_field    = "close" # field for mark-to-market
        ),

        # ── runner section ──
        runner = list(
            mode          = "robust", # "strict" or "robust"
            verbose       = TRUE
        ),

        # ── analytics section ──
        analytics = list(
            compute_drawdown  = TRUE,
            compute_turnover  = TRUE
        ),

        # ── meta section ──
        meta = list(
            description = NULL,
            run_id = NULL,
            contract_version = "bt_contract_v2"
        )
    )
}

# ── Spec access ──────────────────────────────────────────────────────────────

#' @export
bt_get_spec <- function(overrides = NULL) {
    spec <- bt_spec_default()
    if (!is.null(overrides)) {
        spec <- utils::modifyList(spec, overrides)
    }
    spec
}

# ── Spec validation ──────────────────────────────────────────────────────────

#' @export
bt_validate_spec <- function(spec) {
    req <- c("clock", "universe", "policies", "execution", "costs", "accounting", "runner", "analytics", "meta")
    miss <- setdiff(req, names(spec))
    if (length(miss) > 0) stop("BacktestSpec missing: ", paste(miss, collapse = ", "))

    # Clock
    if (is.null(spec$clock$freq)) stop("clock$freq is required")

    # Universe
    u <- spec$universe
    if (!.bt_is_scalar_int(u$lookback_days) || as.integer(u$lookback_days) < 5L) {
        stop("universe$lookback_days must be integer >= 5")
    }
    if (!.bt_is_scalar_num(u$min_days_traded_ratio) || u$min_days_traded_ratio <= 0 || u$min_days_traded_ratio > 1) {
        stop("universe$min_days_traded_ratio must be in (0,1]")
    }
    if (!.bt_is_scalar_num(u$min_median_traded_value) || u$min_median_traded_value < 0) {
        stop("universe$min_median_traded_value must be >= 0")
    }
    if (!is.null(u$max_universe)) {
        if (!.bt_is_scalar_int(u$max_universe) || as.integer(u$max_universe) < 3L) {
            stop("universe$max_universe must be NULL or integer >= 3")
        }
    }
    if (!is.null(u$min_price_history_days)) {
        if (!.bt_is_scalar_int(u$min_price_history_days) || as.integer(u$min_price_history_days) < 5L) {
            stop("universe$min_price_history_days must be NULL or integer >= 5")
        }
    }
    if (!is.logical(u$keep_holdings) || length(u$keep_holdings) != 1L) {
        stop("universe$keep_holdings must be TRUE/FALSE")
    }

    # Policies
    pol <- spec$policies
    for (k in c("warmup", "cold_start")) {
        if (is.null(pol[[k]]$action)) stop("policies$", k, "$action is required")
        if (!pol[[k]]$action %in% c("hold_cash", "baseline", "allow_trade")) {
            stop("policies$", k, "$action must be hold_cash|baseline|allow_trade")
        }
        if (!pol[[k]]$baseline_strategy %in% c("all_cash", "equal_weight")) {
            stop("policies$", k, "$baseline_strategy must be all_cash|equal_weight")
        }
    }
    if (!.bt_is_scalar_num(pol$weight_tol) || pol$weight_tol <= 0) stop("policies$weight_tol must be > 0")

    # Execution
    if (!spec$execution$price_field %in% c("open", "close")) {
        stop("execution$price_field must be 'open' or 'close'")
    }

    # Costs
    if (spec$costs$fee_rate < 0) stop("costs$fee_rate must be >= 0")
    if (spec$costs$slippage_rate < 0) stop("costs$slippage_rate must be >= 0")

    # Accounting
    if (spec$accounting$initial_nav <= 0) stop("accounting$initial_nav must be > 0")
    if (spec$accounting$initial_cash < 0) stop("accounting$initial_cash must be >= 0")

    invisible(spec)
}

# ── Proposal helpers (readiness + normalization + baseline) ───────────────────

# Readiness contract (OPTIONAL for models, but strongly recommended):
# proposal$meta$status ∈ { "OK", "WARMUP", "COLD_START", "ERROR" }
# proposal$meta$ready_to_trade ∈ TRUE/FALSE
# proposal$meta$reason is optional string.

#' @export
bt_assess_readiness <- function(proposal) {
    st <- proposal$meta$status %||% NA_character_
    rdy <- proposal$meta$ready_to_trade %||% NA

    if (isTRUE(rdy)) {
        return(list(status = "OK", ready = TRUE, stage = "ok", reason = proposal$meta$reason %||% "ready"))
    }
    if (identical(rdy, FALSE)) {
        # fall back to status to classify stage
        if (identical(st, "WARMUP")) {
            return(list(status = "WARMUP", ready = FALSE, stage = "warmup", reason = proposal$meta$reason %||% "warmup"))
        }
        if (identical(st, "COLD_START")) {
            return(list(status = "COLD_START", ready = FALSE, stage = "cold_start", reason = proposal$meta$reason %||% "cold_start"))
        }
        if (identical(st, "ERROR")) {
            return(list(status = "ERROR", ready = FALSE, stage = "error", reason = proposal$meta$reason %||% "error"))
        }
        return(list(status = st %||% "NOT_READY", ready = FALSE, stage = "unknown", reason = proposal$meta$reason %||% "not_ready"))
    }

    # If model doesn't report readiness, we assume it's ready (backward compatible).
    # This is intentional: otherwise older strategy functions would be forced into warmup mode.
    if (is.na(st) || !nzchar(st)) st <- "OK"
    list(status = st, ready = TRUE, stage = "ok", reason = proposal$meta$reason %||% "default_ready")
}

# Normalize proposal weights to sum exactly to 1 (within tolerance).
# This is backtester-owned to keep execution protocol consistent across models.
#' @export
bt_normalize_proposal <- function(proposal, tol = 1e-6) {
    tw <- proposal$target_weights
    if (!is.data.frame(tw)) stop("proposal$target_weights must be data.frame")
    if (nrow(tw) > 0 && !all(c("symbol", "weight_target") %in% names(tw))) {
        stop("proposal$target_weights must have symbol, weight_target")
    }

    # sanitize
    if (nrow(tw) > 0) {
        tw$symbol <- as.character(tw$symbol)
        tw$weight_target <- as.numeric(tw$weight_target)
        tw <- tw[is.finite(tw$weight_target) & tw$weight_target > 0 & nzchar(tw$symbol), , drop = FALSE]
    }

    risky <- if (nrow(tw) > 0) sum(tw$weight_target, na.rm = TRUE) else 0
    cash <- as.numeric(proposal$cash_weight %||% (1 - risky))
    if (!is.finite(cash)) cash <- 1 - risky

    # clamp cash
    cash <- max(0, min(1, cash))
    target_risky <- 1 - cash

    if (risky <= 0 || target_risky <= 0) {
        proposal$target_weights <- data.frame(symbol = character(0), weight_target = numeric(0))
        proposal$cash_weight <- 1.0
        return(proposal)
    }

    # rescale risky to target_risky
    tw$weight_target <- tw$weight_target * (target_risky / risky)

    # final correction for numerical drift
    risky2 <- sum(tw$weight_target, na.rm = TRUE)
    tot <- risky2 + cash
    if (abs(tot - 1.0) > tol && risky2 > 0) {
        tw$weight_target <- tw$weight_target * ((1 - cash) / risky2)
    }

    proposal$target_weights <- tw
    proposal$cash_weight <- max(0, 1 - sum(tw$weight_target, na.rm = TRUE))
    proposal
}

# Baseline strategy resolver (string -> function)
#' @export
bt_resolve_baseline_strategy <- function(name) {
    nm <- name %||% "all_cash"
    if (identical(nm, "all_cash")) {
        return(bt_strategy_all_cash)
    }
    if (identical(nm, "equal_weight")) {
        return(bt_strategy_equal_weight)
    }
    stop("Unknown baseline strategy: ", nm)
}

# Apply backtester policy override (warmup/cold-start) to a proposal.
# Always preserves strategy_state_out from the original model call.
#' @export
bt_apply_policy <- function(proposal, policy_action, baseline_name,
                            decision_date, data_context, portfolio_state,
                            strategy_spec, runtime_ctx = list()) {
    if (policy_action %in% c("allow_trade")) {
        proposal$meta$bt_override <- FALSE
        proposal$meta$bt_override_action <- "allow_trade"
        return(proposal)
    }

    if (policy_action %in% c("hold_cash")) {
        proposal$target_weights <- data.frame(symbol = character(0), weight_target = numeric(0))
        proposal$cash_weight <- 1.0
        proposal$meta$bt_override <- TRUE
        proposal$meta$bt_override_action <- "hold_cash"
        return(proposal)
    }

    if (policy_action %in% c("baseline")) {
        base_fn <- bt_resolve_baseline_strategy(baseline_name)
        # baseline is stateless; do NOT replace strategy_state_out
        base_prop <- bt_call_strategy(
            strategy_fn = base_fn,
            decision_date = decision_date,
            data_context = data_context,
            portfolio_state = portfolio_state,
            strategy_state_in = NULL,
            strategy_spec = strategy_spec,
            runtime_ctx = runtime_ctx
        )
        # merge baseline portfolio into original proposal
        proposal$target_weights <- base_prop$target_weights
        proposal$cash_weight <- base_prop$cash_weight
        proposal$warnings <- unique(c(proposal$warnings, paste0("BT baseline applied: ", baseline_name)))
        proposal$meta$bt_override <- TRUE
        proposal$meta$bt_override_action <- paste0("baseline:", baseline_name)
        return(proposal)
    }

    stop("Unknown policy action: ", policy_action)
}

# ── StrategyProposal validation ──────────────────────────────────────────────

#' @export
bt_validate_proposal <- function(proposal) {
    req <- c("decision_date", "target_weights", "cash_weight")
    miss <- setdiff(req, names(proposal))
    if (length(miss) > 0) stop("StrategyProposal missing: ", paste(miss, collapse = ", "))

    tgt <- proposal$target_weights
    if (!is.data.frame(tgt)) stop("target_weights must be a data.frame")
    if (nrow(tgt) > 0 && !all(c("symbol", "weight_target") %in% names(tgt))) {
        stop("target_weights must have 'symbol' and 'weight_target' columns")
    }

    # Optional: locked_symbols (character vector)
    if (!is.null(proposal$locked_symbols)) {
        if (!is.character(proposal$locked_symbols)) stop("locked_symbols must be character")
    }

    # Budget check (runner will normalize; this warns for visibility)
    risky <- sum(tgt$weight_target, na.rm = TRUE)
    tot <- risky + proposal$cash_weight
    if (is.finite(tot) && abs(tot - 1.0) > 1e-3) {
        warning(sprintf("Proposal weights sum to %.4f (runner will normalize)", tot), call. = FALSE)
    }

    invisible(proposal)
}

# ── ExecutionReport validation ───────────────────────────────────────────────

#' @export
bt_validate_execution <- function(report) {
    req <- c(
        "decision_date", "execution_date", "orders", "fills",
        "costs", "cash_delta"
    )
    miss <- setdiff(req, names(report))
    if (length(miss) > 0) stop("ExecutionReport missing: ", paste(miss, collapse = ", "))

    if (report$execution_date <= report$decision_date) {
        stop("execution_date must be after decision_date")
    }

    invisible(report)
}

# ── PortfolioState validation ────────────────────────────────────────────────

#' @export
bt_validate_state <- function(state) {
    req <- c("as_of_date", "positions", "cash", "nav")
    miss <- setdiff(req, names(state))
    if (length(miss) > 0) stop("PortfolioState missing: ", paste(miss, collapse = ", "))

    if (state$cash < 0) {
        warning("Portfolio cash is negative", call. = FALSE)
    }
    if (state$nav <= 0) {
        warning("Portfolio NAV is non-positive", call. = FALSE)
    }

    invisible(state)
}

# ── BacktestRunArtifact validation ───────────────────────────────────────────

#' @export
bt_validate_run_artifact <- function(result) {
    req <- c("nav_series", "rebalance_log")
    miss <- setdiff(req, names(result))
    if (length(miss) > 0) stop("BacktestRunArtifact missing: ", paste(miss, collapse = ", "))

    nav <- result$nav_series
    if (!is.data.frame(nav) || !all(c("date", "nav") %in% names(nav))) {
        stop("nav_series must be a data.frame with 'date' and 'nav' columns")
    }

    invisible(result)
}

# ── Strategy caller (runtime_ctx optional) ───────────────────────────────────
# This keeps backwards compatibility with strategy functions that don't accept runtime_ctx.

#' @export
bt_call_strategy <- function(strategy_fn, decision_date, data_context,
                             portfolio_state, strategy_state_in,
                             strategy_spec, runtime_ctx = list()) {
    fmls <- names(formals(strategy_fn))
    if (!is.null(fmls) && "runtime_ctx" %in% fmls) {
        return(strategy_fn(decision_date, data_context, portfolio_state,
            strategy_state_in, strategy_spec,
            runtime_ctx = runtime_ctx
        ))
    }
    strategy_fn(decision_date, data_context, portfolio_state, strategy_state_in, strategy_spec)
}
