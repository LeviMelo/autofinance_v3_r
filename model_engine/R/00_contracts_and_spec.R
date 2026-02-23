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
    W0 <- matrix(
        0,
        nrow = 3, ncol = 3,
        dimnames = list(
            c("kalman", "tsmom", "cash"),
            c("disp", "eta", "VoV")
        )
    )

    list(
        data = list(
            min_coverage_ratio = 0.9,
            min_median_turnover = 1e5,
            # Aligned with data_engine canonical asset types
            allowed_types = c("equity", "fii", "etf", "bdr")
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
            vov = list(lookback = 126L) # requires R window of L + vol_lookback - 1
        ),
        gating = list(
            w0 = c(kalman = 0, tsmom = 0, cash = -1),
            # Explicit matrix (zero matrix = static gating until calibrated, but no NULL/disabled ambiguity)
            W = W0
        ),
        portfolio = list(
            tilt = list(max_tilt = 2.0),
            caps = list(max_weight = 0.15)
        ),
        meta = list(
            retain_windows = TRUE,
            retain_matrices = TRUE
        )
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

    # ---- data section ----
    if (!is.null(spec$data$min_coverage_ratio) &&
        (spec$data$min_coverage_ratio < 0 || spec$data$min_coverage_ratio > 1)) {
        stop("data$min_coverage_ratio must be in [0, 1]")
    }

    if (!is.null(spec$data$min_median_turnover) &&
        (!is.numeric(spec$data$min_median_turnover) || length(spec$data$min_median_turnover) != 1 ||
            !is.finite(spec$data$min_median_turnover) || spec$data$min_median_turnover < 0)) {
        stop("data$min_median_turnover must be a finite scalar >= 0")
    }

    allowed_types <- spec$data$allowed_types
    if (is.null(allowed_types) || length(allowed_types) == 0 ||
        any(!nzchar(trimws(as.character(allowed_types))))) {
        stop("data$allowed_types must be a non-empty character vector")
    }
    # Keep broad enough for compatibility; default should be data_engine canonical set
    allowed_set <- c("equity", "fii", "etf", "bdr", "unit")
    if (!all(as.character(allowed_types) %in% allowed_set)) {
        stop(
            "data$allowed_types contains invalid values. Allowed: ",
            paste(allowed_set, collapse = ", ")
        )
    }

    # ---- risk section ----
    for (mod in c("vol", "pca")) {
        if (!is.null(spec$risk[[mod]]$lookback) && spec$risk[[mod]]$lookback <= 0) {
            stop(sprintf("risk$%s$lookback must be positive", mod))
        }
    }

    if (!is.null(spec$risk$pca$k) && spec$risk$pca$k < 1) {
        stop("risk$pca$k must be >= 1")
    }

    if (!is.null(spec$risk$resid$lambda) && spec$risk$resid$lambda < 0) {
        stop("risk$resid$lambda must be >= 0")
    }

    # Nonnegotiable in your locked backbone
    if (!isTRUE(spec$risk$resid$use_glasso)) {
        stop("risk$resid$use_glasso must be TRUE (PCA -> Glasso -> HRP is locked in this pipeline)")
    }

    # ---- signals section ----
    if (!is.null(spec$signals$kalman$lookback) && spec$signals$kalman$lookback <= 0) {
        stop("signals$kalman$lookback must be > 0")
    }
    if (!is.null(spec$signals$tsmom$horizon) && spec$signals$tsmom$horizon <= 0) {
        stop("signals$tsmom$horizon must be > 0")
    }

    if (!is.null(spec$signals$kalman$q_var) && spec$signals$kalman$q_var <= 0) {
        stop("signals$kalman$q_var must be > 0")
    }

    if (!is.null(spec$signals$kalman$r_var) && spec$signals$kalman$r_var <= 0) {
        stop("signals$kalman$r_var must be > 0")
    }

    # ---- market_state section ----
    if (!is.null(spec$market_state$dispersion$lookback) && spec$market_state$dispersion$lookback <= 0) {
        stop("market_state$dispersion$lookback must be > 0")
    }
    if (!is.null(spec$market_state$eta$lookback) && spec$market_state$eta$lookback <= 0) {
        stop("market_state$eta$lookback must be > 0")
    }
    if (!is.null(spec$market_state$vov$lookback) && spec$market_state$vov$lookback <= 0) {
        stop("market_state$vov$lookback must be > 0")
    }

    # ---- gating section ----
    w0 <- spec$gating$w0
    if (is.null(w0) || length(w0) != 3 || !all(c("kalman", "tsmom", "cash") %in% names(w0))) {
        stop("gating$w0 must be a 3-element vector named c('kalman', 'tsmom', 'cash')")
    }

    W <- spec$gating$W
    expected_state_names <- c("disp", "eta", "VoV")

    # Nonnegotiable in your adaptive gating design: W must exist
    if (is.null(W)) {
        stop("gating$W must be provided (3x3 matrix with rows kalman/tsmom/cash and cols disp/eta/VoV)")
    }

    if (!is.matrix(W)) stop("gating$W must be a matrix")
    if (nrow(W) != 3 || ncol(W) != length(expected_state_names)) {
        stop("gating$W must be a 3 x 3 matrix")
    }
    if (is.null(rownames(W)) || is.null(colnames(W))) {
        stop("gating$W must have rownames and colnames")
    }
    if (!all(rownames(W) == c("kalman", "tsmom", "cash"))) {
        stop("gating$W rownames must be exactly c('kalman', 'tsmom', 'cash')")
    }
    if (!all(colnames(W) == expected_state_names)) {
        stop("gating$W colnames must be exactly c('disp', 'eta', 'VoV')")
    }
    if (any(!is.finite(W))) {
        stop("gating$W must contain only finite numeric values")
    }

    # ---- portfolio section ----
    if (!is.null(spec$portfolio$caps$max_weight) &&
        (spec$portfolio$caps$max_weight <= 0 || spec$portfolio$caps$max_weight > 1)) {
        stop("portfolio$caps$max_weight must be in (0, 1]")
    }

    if (!is.null(spec$portfolio$tilt$max_tilt) &&
        (!is.numeric(spec$portfolio$tilt$max_tilt) || length(spec$portfolio$tilt$max_tilt) != 1 ||
            !is.finite(spec$portfolio$tilt$max_tilt) || spec$portfolio$tilt$max_tilt < 1)) {
        stop("portfolio$tilt$max_tilt must be a finite scalar >= 1")
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

    # Target bounds
    tgt <- x$target_weights
    if (!is.data.frame(tgt)) stop("target_weights must be a data.frame")
    if (!all(c("symbol", "weight_target") %in% names(tgt))) {
        stop("target_weights must have 'symbol' and 'weight_target'")
    }
    if (length(unique(tgt$symbol)) != nrow(tgt)) stop("Duplicate symbols in target_weights")
    if (any(tgt$weight_target < 0, na.rm = TRUE)) stop("Negative target weights are not allowed")

    if (!is.numeric(x$cash_weight) || length(x$cash_weight) != 1 ||
        x$cash_weight < -1e-6 || x$cash_weight > 1 + 1e-6) {
        stop("cash_weight must be scalar in [0,1]")
    }

    tot_w <- sum(tgt$weight_target, na.rm = TRUE) + x$cash_weight
    if (abs(tot_w - 1.0) > 1e-4) stop(sprintf("Weights sum to %f, not 1.0", tot_w))

    # Gating consistency (if present)
    gating <- x$gating
    if (length(gating) > 0) {
        req_g <- c("w_kalman", "w_tsmom", "w_cash", "gross_exposure")
        if (!all(req_g %in% names(gating))) {
            stop("Gating artifact missing required fields")
        }
        if (abs(gating$gross_exposure - (1 - gating$w_cash)) > 1e-6) {
            stop("Gating consistency: gross_exposure != 1 - w_cash")
        }
        sum_gates <- gating$w_kalman + gating$w_tsmom + gating$w_cash
        if (abs(sum_gates - 1.0) > 1e-6) {
            stop("Gating consistency: softmax weights do not sum to 1")
        }
    }

    # Risk consistency (only validate matrices if retained/present)
    risk <- x$risk
    if (length(risk) > 0) {
        if (!is.null(risk$w_hrp)) {
            risk_univ <- names(risk$w_hrp)
            if (length(risk_univ) > 0 && !all(tgt$symbol %in% risk_univ)) {
                stop("Portfolio target universe contains elements not in canonical risk universe")
            }
        }

        if (!is.null(risk$sigma_t) && !is.null(risk$Sigma_total)) {
            if (is.null(rownames(risk$Sigma_total)) || is.null(colnames(risk$Sigma_total))) {
                stop("Sigma_total must have row/col names when retained")
            }
            if (!all(names(risk$sigma_t) == rownames(risk$Sigma_total))) {
                stop("Risk artifact mismatch: sigma_t names do not align with Sigma_total rownames")
            }
            if (!is.null(risk$w_hrp) && !all(names(risk$w_hrp) == colnames(risk$Sigma_total))) {
                stop("Risk artifact mismatch: w_hrp names do not align with Sigma_total colnames")
            }
        }
    }

    invisible(x)
}

#' @export
me_hash_spec <- function(spec) {
    me_require("digest")
    digest::digest(spec)
}
