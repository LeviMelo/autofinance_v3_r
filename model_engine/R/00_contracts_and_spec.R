#' @title Model Engine — Contracts and Spec
#' @description ModelSpec defaults, validation, artifact contracts, shared helpers.

# ── Helpers ──────────────────────────────────────────────────────────────────

#' @export
`%||%` <- function(a, b) if (is.null(a)) b else a

#' @export
me_require <- function(pkgs) {
    for (pkg in pkgs) {
        if (!requireNamespace(pkg, quietly = TRUE)) {
            stop(sprintf("Package '%s' is required but not installed.", pkg), call. = FALSE)
        }
    }
}

# ── ModelSpec default ────────────────────────────────────────────────────────

#' @export
me_spec_default <- function() {
    # Default gating matrix W (3 experts × 3 state features)
    W0 <- matrix(
        0,
        nrow = 3, ncol = 3,
        dimnames = list(
            c("kalman", "tsmom", "cash"),
            c("disp", "eta", "VoV")
        )
    )

    list(
        # ── data section ──
        data = list(
            min_coverage_ratio = 0.90,
            min_median_turnover = 1e5,
            allowed_types = c("equity", "fii", "etf", "bdr")
        ),

        # ── risk section ──
        risk = list(
            vol = list(
                lookback = 252L,
                method   = "sd" # "sd" or "ewma"
            ),
            pca = list(
                k        = 5L,
                lookback = 252L
            ),
            resid = list(
                use_glasso = FALSE,
                lambda     = 0.1
            ),
            factor = list(),
            hrp = list()
        ),

        # ── signals section ──
        signals = list(
            kalman = list(
                lookback = 252L,
                q_var    = 1e-4,
                r_var    = 1e-2,
                scale    = 1.0
            ),
            tsmom = list(
                horizon = 252L,
                scale   = 2.0
            )
        ),

        # ── market state section ──
        market_state = list(
            dispersion = list(lookback = 63L),
            eta = list(lookback = 126L),
            vov = list(
                lookback     = 63L,
                vol_lookback = 21L
            )
        ),

        # ── gating section ──
        gating = list(
            W           = W0,
            w0          = c(kalman = 0, tsmom = 0, cash = -1),
            temperature = 1.0
        ),

        # ── portfolio section ──
        portfolio = list(
            tilt = list(max_tilt = 2.0),
            caps = list(max_weight = 0.15),
            turnover_penalty = 0.0
        ),

        # ── meta section ──
        meta = list(
            retain_windows  = FALSE,
            retain_matrices = FALSE,
            mode            = "production"
        )
    )
}

# ── Spec access ──────────────────────────────────────────────────────────────

#' @export
me_get_spec <- function(overrides = NULL) {
    spec <- me_spec_default()
    if (!is.null(overrides)) {
        spec <- utils::modifyList(spec, overrides)
    }
    spec
}

# ── Spec validation ──────────────────────────────────────────────────────────

#' @export
me_validate_spec <- function(spec) {
    # ---- top-level structure ----
    req <- c("data", "risk", "signals", "market_state", "gating", "portfolio", "meta")
    miss <- setdiff(req, names(spec))
    if (length(miss) > 0) stop("ModelSpec missing sections: ", paste(miss, collapse = ", "))

    # ---- data section ----
    d <- spec$data
    if (!is.null(d$min_coverage_ratio) && (d$min_coverage_ratio < 0 || d$min_coverage_ratio > 1)) {
        stop("data$min_coverage_ratio must be in [0, 1]")
    }
    if (!is.null(d$min_median_turnover) && d$min_median_turnover < 0) {
        stop("data$min_median_turnover must be >= 0")
    }
    allowed_set <- c("equity", "fii", "etf", "bdr")
    if (!is.null(d$allowed_types) && !all(d$allowed_types %in% allowed_set)) {
        stop("data$allowed_types contains invalid values. Allowed: ", paste(allowed_set, collapse = ", "))
    }

    # ---- risk section ----
    r <- spec$risk
    for (mod in c("vol", "pca")) {
        lkb <- r[[mod]]$lookback
        if (!is.null(lkb) && (!is.finite(lkb) || lkb <= 0)) {
            stop(sprintf("risk$%s$lookback must be a positive finite number", mod))
        }
    }
    if (!is.null(r$pca$k) && (!is.finite(r$pca$k) || r$pca$k < 1)) {
        stop("risk$pca$k must be >= 1")
    }
    if (!is.null(r$resid$lambda) && (!is.finite(r$resid$lambda) || r$resid$lambda < 0)) {
        stop("risk$resid$lambda must be finite and >= 0")
    }

    # ---- signals section ----
    s <- spec$signals
    if (!is.null(s$kalman$lookback) && s$kalman$lookback <= 0) {
        stop("signals$kalman$lookback must be > 0")
    }
    if (!is.null(s$tsmom$horizon) && s$tsmom$horizon <= 0) {
        stop("signals$tsmom$horizon must be > 0")
    }

    # ---- market_state section ----
    ms <- spec$market_state
    for (feat in c("dispersion", "eta", "vov")) {
        lkb <- ms[[feat]]$lookback
        if (!is.null(lkb) && (!is.finite(lkb) || lkb <= 0)) {
            stop(sprintf("market_state$%s$lookback must be > 0", feat))
        }
    }

    # ---- gating section ----
    g <- spec$gating
    W <- g$W
    if (!is.null(W)) {
        if (!is.matrix(W) || !all(dim(W) == c(3, 3))) {
            stop("gating$W must be a 3x3 matrix")
        }
        exp_row <- c("kalman", "tsmom", "cash")
        exp_col <- c("disp", "eta", "VoV")
        if (!is.null(rownames(W)) && !all(rownames(W) == exp_row)) {
            stop("gating$W rownames must be c('kalman', 'tsmom', 'cash')")
        }
        if (!is.null(colnames(W)) && !all(colnames(W) == exp_col)) {
            stop("gating$W colnames must be c('disp', 'eta', 'VoV')")
        }
        if (any(!is.finite(W))) {
            stop("gating$W must contain only finite values")
        }
    }
    w0 <- g$w0
    if (!is.null(w0)) {
        if (length(w0) != 3) stop("gating$w0 must have length 3")
        if (any(!is.finite(w0))) stop("gating$w0 must contain only finite values")
    }
    if (!is.null(g$temperature) && (!is.finite(g$temperature) || g$temperature <= 0)) {
        stop("gating$temperature must be a positive scalar")
    }

    # ---- portfolio section ----
    p <- spec$portfolio
    if (!is.null(p$caps$max_weight) && (p$caps$max_weight <= 0 || p$caps$max_weight > 1)) {
        stop("portfolio$caps$max_weight must be in (0, 1]")
    }
    if (!is.null(p$tilt$max_tilt) && (!is.finite(p$tilt$max_tilt) || p$tilt$max_tilt < 1)) {
        stop("portfolio$tilt$max_tilt must be >= 1")
    }

    invisible(spec)
}

# ── Snapshot artifact validation ─────────────────────────────────────────────

#' @export
me_validate_snapshot_artifact <- function(x) {
    req <- c(
        "as_of_date", "tradable_symbols", "target_weights",
        "cash_weight", "risk", "signals", "market_state",
        "gating", "portfolio_diag", "meta", "warnings"
    )
    miss <- setdiff(req, names(x))
    if (length(miss) > 0) stop("Snapshot artifact missing: ", paste(miss, collapse = ", "))

    # Target weights structure
    tgt <- x$target_weights
    if (!is.data.frame(tgt)) stop("target_weights must be a data.frame")
    if (!all(c("symbol", "weight_target") %in% names(tgt))) {
        stop("target_weights must have columns 'symbol' and 'weight_target'")
    }

    # Weight budget
    tot <- sum(tgt$weight_target, na.rm = TRUE) + x$cash_weight
    if (abs(tot - 1.0) > 1e-4) stop(sprintf("Weights sum to %f, not 1.0", tot))

    # Cash validity
    if (!is.finite(x$cash_weight) || x$cash_weight < 0 || x$cash_weight > 1) {
        stop("cash_weight must be in [0, 1]")
    }

    # Gating consistency
    g <- x$gating
    if (length(g) > 0 && !is.null(g$w_kalman)) {
        gate_sum <- g$w_kalman + g$w_tsmom + g$w_cash
        if (abs(gate_sum - 1.0) > 1e-6) {
            stop("Gating softmax weights do not sum to 1")
        }
        if (!is.null(g$gross_exposure) && abs(g$gross_exposure - (1 - x$cash_weight)) > 1e-4) {
            stop("Gating gross_exposure inconsistent with cash_weight")
        }
    }

    # Risk universe alignment
    risk <- x$risk
    if (!is.null(risk$w_hrp) && length(risk$w_hrp) > 0) {
        risk_univ <- names(risk$w_hrp)
        if (!all(tgt$symbol %in% risk_univ)) {
            stop("Some target symbols are not in risk universe")
        }
        # Sigma_total alignment
        if (!is.null(risk$Sigma_total)) {
            if (!all(names(risk$w_hrp) == colnames(risk$Sigma_total))) {
                stop("w_hrp names do not align with Sigma_total colnames")
            }
        }
    }

    invisible(x)
}

# ── Model state validation (for recursive carry-forward) ─────────────────────

#' @export
me_validate_model_state <- function(state) {
    if (is.null(state)) {
        return(invisible(NULL))
    }

    # State must be a list with at minimum these markers
    if (!is.list(state)) stop("model_state must be a list or NULL")

    # If it has prev_target, validate structure
    if (!is.null(state$prev_target)) {
        pt <- state$prev_target
        if (!is.data.frame(pt) || !all(c("symbol", "weight_target") %in% names(pt))) {
            stop("model_state$prev_target must be a data.frame with symbol + weight_target")
        }
    }

    invisible(state)
}

# ── Spec hash ────────────────────────────────────────────────────────────────

#' @export
me_hash_spec <- function(spec) {
    me_require("digest")
    digest::digest(spec)
}
