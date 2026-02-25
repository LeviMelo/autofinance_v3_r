#' @title Backtest Engine — Contracts and Spec
#' @description BacktestSpec defaults, validation, and artifact contracts.

# ── Helpers ──────────────────────────────────────────────────────────────────

#' @export
`%||%` <- function(a, b) if (is.null(a)) b else a

# ── BacktestSpec default ─────────────────────────────────────────────────────

#' @export
bt_spec_default <- function() {
    list(
        # ── clock section ──
        clock = list(
            freq = "months" # "months", "weeks", "days", or integer (every N days)
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
            run_id      = NULL
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
    req <- c("clock", "execution", "costs", "accounting", "runner", "analytics", "meta")
    miss <- setdiff(req, names(spec))
    if (length(miss) > 0) stop("BacktestSpec missing: ", paste(miss, collapse = ", "))

    # Clock
    if (is.null(spec$clock$freq)) stop("clock$freq is required")

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

    # Weight budget check
    risky <- sum(tgt$weight_target, na.rm = TRUE)
    tot <- risky + proposal$cash_weight
    if (abs(tot - 1.0) > 1e-3) {
        warning(sprintf("Proposal weights sum to %.4f, normalizing", tot), call. = FALSE)
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
