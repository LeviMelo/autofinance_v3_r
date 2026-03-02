# backtest_engine/R/00_contracts_and_spec.R
# Backtest Engine v3 — strict, window-based, no warmup/cold-start/policy fallbacks.

# ── helpers ────────────────────────────────────────────────────────────────
#' @export
`%||%` <- function(a, b) if (is.null(a)) b else a

.bt_is_scalar_num <- function(x) is.numeric(x) && length(x) == 1L && is.finite(x)
.bt_is_scalar_int <- function(x) (is.numeric(x) || is.integer(x)) && length(x) == 1L && is.finite(x) && (as.numeric(x) %% 1 == 0)

.bt_stop <- function(code, msg) stop(sprintf("[%s] %s", code, msg), call. = FALSE)

.bt_clean_symbols <- function(x) {
    s <- as.character(x)
    s <- s[!is.na(s) & nzchar(s)]
    unique(s)
}

.bt_scalar_or_default <- function(x, default = 0) {
    if (length(x) < 1) return(default)
    y <- as.numeric(x[1])
    if (!is.finite(y)) return(default)
    y
}

# Strict deep-merge: override keys MUST exist in base spec at every level.
.bt_merge_strict <- function(base, overrides, path = "spec") {
    if (is.null(overrides)) {
        return(base)
    }
    if (!is.list(overrides)) .bt_stop("spec_overrides_type", sprintf("%s overrides must be a list", path))

    out <- base
    for (nm in names(overrides)) {
        if (is.null(nm) || !nzchar(nm)) .bt_stop("spec_overrides_key", sprintf("%s overrides contains empty key", path))
        if (!nm %in% names(base)) .bt_stop("spec_overrides_unknown_key", sprintf("Unknown %s.%s", path, nm))

        v <- overrides[[nm]]
        if (is.list(base[[nm]]) && is.list(v)) {
            out[[nm]] <- .bt_merge_strict(base[[nm]], v, paste0(path, ".", nm))
        } else {
            # Keep explicit NULL overrides as named entries (do not drop the key).
            if (is.null(v)) {
                out[nm] <- list(NULL)
            } else {
                out[[nm]] <- v
            }
        }
    }
    out
}

# ── BacktestSpec (strict) ───────────────────────────────────────────────────
#' @export
bt_spec_default <- function() {
    list(
        clock = list(
            freq = "months" # "days" | "weeks" | "months" | integer N (every N trading days)
        ),
        universe = list(
            lookback_days = 63L,
            min_days_traded_ratio = 0.75,
            min_median_traded_value = 5e5,
            include_types = c("equity", "fii", "etf", "bdr"),
            max_universe = NULL, # optional top-K by median traded value
            min_price_history_days = 252L, # strict rectangular close history requirement (NULL disables)
            keep_holdings = TRUE # include holdings in strategy-visible slice
        ),
        execution = list(
            price_field = "open", # "open" or "close"
            exec_lag = 1L # trading days between decision and execution
        ),
        costs = list(
            fee_rate = 0.0003,
            slippage_rate = 0.0010
        ),
        accounting = list(
            initial_nav = 100000,
            initial_cash = 100000,
            mark_field = "close", # mark-to-market field
            mark_missing_policy = "carry_last", # "error" | "carry_last"
            max_stale_mark_days = 5L # max trading-day staleness when carry_last; NULL disables cap
        ),
        runner = list(
            mode = "strict", # "strict" only in v3
            verbose = TRUE
        ),
        analytics = list(
            rf_annual = 0.0 # optional risk-free for sharpe; 0 by default
        ),
        meta = list(
            contract_version = "bt_contract_v3",
            description = NULL,
            run_id = NULL
        )
    )
}

#' @export
bt_get_spec <- function(overrides = NULL) {
    spec <- bt_spec_default()
    if (!is.null(overrides)) spec <- .bt_merge_strict(spec, overrides, path = "spec")
    bt_validate_spec(spec)
    spec
}

#' @export
bt_validate_spec <- function(spec) {
    if (!is.list(spec)) .bt_stop("spec_type", "spec must be a list")

    req <- c("clock", "universe", "execution", "costs", "accounting", "runner", "analytics", "meta")
    miss <- setdiff(req, names(spec))
    if (length(miss) > 0) .bt_stop("spec_missing_sections", paste("Missing:", paste(miss, collapse = ", ")))

    # clock
    freq <- spec$clock$freq %||% NULL
    if (is.null(freq)) .bt_stop("spec_clock_freq", "clock$freq is required")
    if (!(is.character(freq) && freq %in% c("days", "weeks", "months")) && !(is.numeric(freq) && length(freq) == 1L && is.finite(freq))) {
        .bt_stop("spec_clock_freq", "clock$freq must be 'days'|'weeks'|'months' or integer N")
    }
    if (is.numeric(freq) && as.integer(freq) < 1L) .bt_stop("spec_clock_freq", "clock$freq integer N must be >= 1")

    # universe
    u <- spec$universe
    if (!.bt_is_scalar_int(u$lookback_days) || as.integer(u$lookback_days) < 5L) .bt_stop("spec_univ_lb", "universe$lookback_days must be integer >= 5")
    if (!.bt_is_scalar_num(u$min_days_traded_ratio) || u$min_days_traded_ratio <= 0 || u$min_days_traded_ratio > 1) {
        .bt_stop("spec_univ_cov", "universe$min_days_traded_ratio must be in (0,1]")
    }
    if (!.bt_is_scalar_num(u$min_median_traded_value) || u$min_median_traded_value < 0) .bt_stop("spec_univ_tv", "universe$min_median_traded_value must be >= 0")
    if (!is.null(u$max_universe)) {
        if (!.bt_is_scalar_int(u$max_universe) || as.integer(u$max_universe) < 3L) .bt_stop("spec_univ_max", "universe$max_universe must be NULL or integer >= 3")
    }
    if (!is.null(u$min_price_history_days)) {
        if (!.bt_is_scalar_int(u$min_price_history_days) || as.integer(u$min_price_history_days) < 5L) {
            .bt_stop("spec_univ_mph", "universe$min_price_history_days must be NULL or integer >= 5")
        }
    }
    if (!is.logical(u$keep_holdings) || length(u$keep_holdings) != 1L) .bt_stop("spec_univ_keep_hold", "universe$keep_holdings must be TRUE/FALSE")

    # execution
    ex <- spec$execution
    if (!ex$price_field %in% c("open", "close")) .bt_stop("spec_exec_price_field", "execution$price_field must be 'open' or 'close'")
    if (!.bt_is_scalar_int(ex$exec_lag) || as.integer(ex$exec_lag) < 0L) .bt_stop("spec_exec_lag", "execution$exec_lag must be integer >= 0")

    # costs
    cs <- spec$costs
    if (!.bt_is_scalar_num(cs$fee_rate) || cs$fee_rate < 0) .bt_stop("spec_cost_fee", "costs$fee_rate must be >= 0")
    if (!.bt_is_scalar_num(cs$slippage_rate) || cs$slippage_rate < 0) .bt_stop("spec_cost_slip", "costs$slippage_rate must be >= 0")

    # accounting
    ac <- spec$accounting
    if (!.bt_is_scalar_num(ac$initial_nav) || ac$initial_nav <= 0) .bt_stop("spec_acc_nav", "accounting$initial_nav must be > 0")
    if (!.bt_is_scalar_num(ac$initial_cash) || ac$initial_cash < 0) .bt_stop("spec_acc_cash", "accounting$initial_cash must be >= 0")
    if (!ac$mark_field %in% c("close", "open")) .bt_stop("spec_acc_mark", "accounting$mark_field must be 'close' or 'open'")
    pol <- ac$mark_missing_policy %||% "carry_last"
    if (!is.character(pol) || length(pol) != 1L || !pol %in% c("error", "carry_last")) {
        .bt_stop("spec_acc_mark_policy", "accounting$mark_missing_policy must be 'error' or 'carry_last'")
    }
    if (!is.null(ac$max_stale_mark_days)) {
        if (!.bt_is_scalar_int(ac$max_stale_mark_days) || as.integer(ac$max_stale_mark_days) < 0L) {
            .bt_stop("spec_acc_mark_stale", "accounting$max_stale_mark_days must be NULL or integer >= 0")
        }
    }

    # runner
    rn <- spec$runner
    if (!identical(rn$mode %||% "strict", "strict")) .bt_stop("spec_runner_mode", "runner$mode must be 'strict' in v3")
    if (!is.logical(rn$verbose) || length(rn$verbose) != 1L) .bt_stop("spec_runner_verbose", "runner$verbose must be TRUE/FALSE")

    # analytics
    an <- spec$analytics
    if (!.bt_is_scalar_num(an$rf_annual %||% 0) || (an$rf_annual %||% 0) < 0) .bt_stop("spec_rf", "analytics$rf_annual must be numeric >= 0")

    invisible(spec)
}

# ── Strategy requirements (strict) ──────────────────────────────────────────
# Contract:
# list(
#   model_id = <string>,
#   prehistory_days = <int >= 0>,  # trading days required strictly BEFORE first scored decision date
#   stateful = TRUE/FALSE,
#   supports_replay = TRUE/FALSE
# )

#' @export
bt_validate_requirements <- function(req) {
    if (!is.list(req)) .bt_stop("req_type", "requirements must be a list")
    for (k in c("model_id", "prehistory_days", "stateful", "supports_replay")) {
        if (!k %in% names(req)) .bt_stop("req_missing", paste0("requirements missing: ", k))
    }
    if (!is.character(req$model_id) || length(req$model_id) != 1L || !nzchar(req$model_id)) .bt_stop("req_model_id", "requirements$model_id must be non-empty string")
    if (!.bt_is_scalar_int(req$prehistory_days) || as.integer(req$prehistory_days) < 0L) .bt_stop("req_prehistory", "requirements$prehistory_days must be integer >= 0")
    if (!is.logical(req$stateful) || length(req$stateful) != 1L) .bt_stop("req_stateful", "requirements$stateful must be TRUE/FALSE")
    if (!is.logical(req$supports_replay) || length(req$supports_replay) != 1L) .bt_stop("req_replay", "requirements$supports_replay must be TRUE/FALSE")
    invisible(req)
}

.bt_came_prehistory_days <- function(strategy_spec) {
    L <- as.integer(strategy_spec$risk$lookback %||% 252L)
    H <- as.integer(strategy_spec$forecast$H %||% 21L)
    mom_h <- strategy_spec$signals$mom_horizons %||% c(21L, 63L, 126L, 252L)
    mom_h <- as.integer(mom_h)
    need_days_including_first_run <- max(L, max(mom_h), 63L) + H + 5L + 1L
    # prehistory_days is strictly BEFORE first scored decision date
    as.integer(max(0L, need_days_including_first_run - 1L))
}

#' @export
bt_strategy_requirements <- function(strategy_fn, strategy_spec) {
    # Strict: requirements must be declared (attribute), except CAME adapter which is well-defined.
    req_attr <- attr(strategy_fn, "bt_requirements", exact = TRUE)

    if (!is.null(req_attr)) {
        bt_validate_requirements(req_attr)
        return(req_attr)
    }

    # Built-in special-case: CAME adapter
    if (isTRUE(identical(strategy_fn, bt_strategy_from_model_engine))) {
        req <- list(
            model_id = "came",
            prehistory_days = .bt_came_prehistory_days(strategy_spec),
            stateful = TRUE,
            supports_replay = FALSE
        )
        bt_validate_requirements(req)
        return(req)
    }

    .bt_stop("req_missing", "Strategy must declare attr(strategy_fn, 'bt_requirements') as a ModelRequirements list.")
}

# ── StrategyProposal contract (strict, no normalization) ────────────────────
#' @export
bt_validate_proposal <- function(proposal, tol = 1e-8) {
    if (!is.list(proposal)) .bt_stop("proposal_type", "proposal must be a list")

    req <- c("decision_date", "target_weights", "cash_weight", "strategy_state_out")
    miss <- setdiff(req, names(proposal))
    if (length(miss) > 0) .bt_stop("proposal_missing", paste("Missing:", paste(miss, collapse = ", ")))

    dd <- as.Date(proposal$decision_date)
    if (is.na(dd)) .bt_stop("proposal_decision_date", "proposal$decision_date must be Date-coercible")

    tw <- proposal$target_weights
    if (!is.data.frame(tw)) .bt_stop("proposal_target_df", "proposal$target_weights must be a data.frame")
    if (nrow(tw) > 0 && !all(c("symbol", "weight_target") %in% names(tw))) {
        .bt_stop("proposal_target_cols", "proposal$target_weights must have columns: symbol, weight_target")
    }

    cw <- as.numeric(proposal$cash_weight)
    if (!is.finite(cw) || cw < 0 || cw > 1) .bt_stop("proposal_cash", "proposal$cash_weight must be in [0,1]")

    if (nrow(tw) > 0) {
        tw$symbol <- as.character(tw$symbol)
        tw$weight_target <- as.numeric(tw$weight_target)
        if (any(is.na(tw$symbol) | !nzchar(tw$symbol))) .bt_stop("proposal_symbol", "proposal$target_weights has invalid symbol")
        if (anyDuplicated(tw$symbol) > 0) .bt_stop("proposal_symbol_dup", "proposal$target_weights has duplicate symbols")
        if (any(!is.finite(tw$weight_target))) .bt_stop("proposal_weight_nan", "proposal$target_weights has non-finite weights")
        if (any(tw$weight_target < 0)) .bt_stop("proposal_weight_neg", "proposal$target_weights has negative weights")
    }

    tot <- cw + if (nrow(tw) > 0) sum(tw$weight_target, na.rm = TRUE) else 0
    if (!is.finite(tot) || abs(tot - 1.0) > tol) {
        .bt_stop("proposal_budget", sprintf("proposal weights must sum to 1 (got %.10f)", tot))
    }

    invisible(proposal)
}

# ── Run artifact contract (minimal; runner will add fields) ─────────────────
#' @export
bt_validate_run_artifact <- function(res) {
    if (!is.list(res)) .bt_stop("run_type", "result must be a list")
    if (!"nav" %in% names(res)) .bt_stop("run_missing_nav", "result$nav is required")
    nav <- res$nav
    if (!is.data.frame(nav) || !all(c("date", "nav") %in% names(nav))) {
        .bt_stop("run_nav_shape", "result$nav must be data.frame(date, nav, ...)")
    }
    if (nrow(nav) > 0) {
        if (any(is.na(as.Date(nav$date)))) .bt_stop("run_nav_date", "result$nav$date must be Date-coercible")
        if (any(!is.finite(as.numeric(nav$nav))) || any(as.numeric(nav$nav) <= 0)) .bt_stop("run_nav_val", "result$nav$nav must be finite and > 0")
    }
    invisible(res)
}

# ── Strategy caller (kept; runtime_ctx optional) ────────────────────────────
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
