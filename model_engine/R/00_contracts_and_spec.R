#' @title Model Engine Contracts and Specs
#' @description Define contracts, validators, enums, spec defaults, and shared structural helpers.

#' @export
`%||%` <- function(a, b) if (is.null(a)) b else a

#' @export
me_require <- function(pkgs) {
    for (pkg in pkgs) {
        if (!requireNamespace(pkg, quietly = TRUE)) {
            stop(sprintf("Package '%s' is strictly required but not installed.", pkg), call. = FALSE)
        }
    }
}

#' @export
me_spec_default <- function() {
    list(
        data = list(
            min_coverage_ratio = 0.9,
            min_median_turnover = 1e5,
            allowed_types = c("equity", "unit")
        ),
        risk = list(
            vol = list(lookback = 63L),
            pca = list(lookback = 252L, k = 3L),
            resid = list(use_glasso = TRUE, lambda = 0.05),
            factor = list(),
            hrp = list()
        ),
        signals = list(
            kalman = list(lookback = 252L, q_var = 1e-4, r_var = 1e-2, scale = 1.0),
            tsmom = list(horizon = 252L, scale = 2.0)
        ),
        market_state = list(
            dispersion = list(lookback = 1L),
            eta = list(lookback = 63L),
            vov = list(lookback = 126L)
        ),
        gating = list(
            w0 = c(kalman = 0, tsmom = 0, cash = -1),
            W = NULL
        ),
        portfolio = list(
            tilt = list(max_tilt = 2.0),
            caps = list(max_weight = 0.15)
        ),
        meta = list()
    )
}

#' @export
me_get_spec <- function(overrides = NULL) {
    spec <- me_spec_default()
    if (!is.null(overrides)) {
        spec <- utils::modifyList(spec, overrides)
    }
    spec
}

#' @export
me_validate_spec <- function(spec) {
    req_names <- c("data", "risk", "signals", "market_state", "gating", "portfolio", "meta")
    missing <- setdiff(req_names, names(spec))
    if (length(missing) > 0) stop("ModelSpec missing sections: ", paste(missing, collapse = ", "))

    # Validate scalar domains
    if (!is.null(spec$portfolio$caps$max_weight) &&
        (spec$portfolio$caps$max_weight <= 0 || spec$portfolio$caps$max_weight > 1)) {
        stop("portfolio$caps$max_weight must be in (0, 1]")
    }

    if (!is.null(spec$data$min_coverage_ratio) &&
        (spec$data$min_coverage_ratio < 0 || spec$data$min_coverage_ratio > 1)) {
        stop("data$min_coverage_ratio must be in [0, 1]")
    }

    invisible(spec)
}

#' @export
me_validate_snapshot_artifact <- function(x) {
    req_names <- c(
        "as_of_date", "tradable_symbols", "target_weights", "cash_weight",
        "risk", "signals", "market_state", "gating", "portfolio_diag", "meta", "warnings"
    )
    missing <- setdiff(req_names, names(x))
    if (length(missing) > 0) stop("SnapshotArtifact missing sections: ", paste(missing, collapse = ", "))

    if (!inherits(x$as_of_date, "Date")) stop("as_of_date must be a Date")

    # Check target bounds
    if (!is.data.frame(x$target_weights)) stop("target_weights must be a data.frame")
    if (!all(c("symbol", "weight_target") %in% names(x$target_weights))) {
        stop("target_weights must have 'symbol' and 'weight_target'")
    }
    if (length(unique(x$target_weights$symbol)) != nrow(x$target_weights)) {
        stop("Duplicate symbols in target_weights")
    }
    if (any(x$target_weights$weight_target < 0, na.rm = TRUE)) {
        stop("Negative target weights are not allowed")
    }

    if (!is.numeric(x$cash_weight) || x$cash_weight < -1e-6 || x$cash_weight > 1 + 1e-6) {
        stop("cash_weight must be in [0,1]")
    }

    tot_w <- sum(x$target_weights$weight_target, na.rm = TRUE) + x$cash_weight
    if (abs(tot_w - 1.0) > 1e-4) stop(sprintf("Weights sum to %f, not 1.0", tot_w))

    invisible(x)
}

#' @export
me_hash_spec <- function(spec) {
    me_require("digest")
    digest::digest(spec)
}
