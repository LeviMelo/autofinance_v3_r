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

# ── Π_t Universe mapping operators (architecture §4.2) ──────────────────────
# Reindex recursive state objects from U_{t-1} to U_t:
#   - carry values on U_t ∩ U_{t-1}
#   - initialize new names with configured priors
#   - drop departed names

#' @export
me_pi_map_vector <- function(v, new_univ, init_val = 0) {
    # Map a named numeric vector to new universe
    if (is.null(v) || length(v) == 0) {
        out <- setNames(rep(init_val, length(new_univ)), new_univ)
        return(out)
    }
    out <- setNames(rep(init_val, length(new_univ)), new_univ)
    common <- intersect(names(v), new_univ)
    if (length(common) > 0) out[common] <- v[common]
    out
}

#' @export
me_pi_map_matrix <- function(M, new_univ, init_diag = 1e-4) {
    # Map a named symmetric matrix to new universe
    # Carry intersection, initialize new diagonal entries, zero off-diag for new
    p_new <- length(new_univ)
    if (is.null(M) || !is.matrix(M) || p_new == 0) {
        out <- diag(init_diag, p_new)
        dimnames(out) <- list(new_univ, new_univ)
        return(out)
    }
    out <- matrix(0, p_new, p_new, dimnames = list(new_univ, new_univ))
    old_names <- rownames(M)
    if (is.null(old_names)) old_names <- colnames(M)
    common <- intersect(old_names, new_univ)
    if (length(common) > 0) {
        out[common, common] <- M[common, common, drop = FALSE]
    }
    # Initialize diagonal for new names
    new_names <- setdiff(new_univ, common)
    if (length(new_names) > 0) {
        for (nm in new_names) out[nm, nm] <- init_diag
    }
    # Ensure symmetry
    out <- (out + t(out)) / 2
    out
}

#' @export
me_pi_map_list <- function(lst, new_univ, init_fn = function() NULL) {
    # Map a per-asset named list to new universe
    if (is.null(lst) || !is.list(lst)) {
        out <- setNames(lapply(new_univ, function(x) init_fn()), new_univ)
        return(out)
    }
    out <- setNames(vector("list", length(new_univ)), new_univ)
    common <- intersect(names(lst), new_univ)
    for (nm in common) out[[nm]] <- lst[[nm]]
    new_names <- setdiff(new_univ, common)
    for (nm in new_names) out[[nm]] <- init_fn()
    out
}

# ── ModelSpec default ────────────────────────────────────────────────────────

#' @export
me_spec_default <- function() {
    # Default legacy gating matrix W (3 experts × 3 state features)
    W0 <- matrix(
        0,
        nrow = 3, ncol = 3,
        dimnames = list(
            c("kalman", "tsmom", "cash"),
            c("disp", "eta", "VoV")
        )
    )

    # Architecture §12.4: 5-component gating matrix (A_pi)
    # Rows = components, Cols = global state features
    # Initial: identity-like (configurable, not learned)
    n_state_dim <- 9L # disp, eta, VoV, dens, eto, chi, liq1, liq2, liq3
    A_pi_default <- matrix(0, nrow = 5, ncol = n_state_dim)
    b_pi_default <- rep(0, 5)

    list(
        # ── data section ──
        data = list(
            min_coverage_ratio = 0.90,
            min_median_turnover = 1e5,
            allowed_types = c("equity", "fii", "etf", "bdr")
        ),

        # ── risk section (architecture §5) ──
        risk = list(
            vol = list(
                lookback     = 252L,
                method       = "ewma", # architecture: recursive EWMA
                lambda_sigma = 0.94 # §5.1 EWMA decay for volatility
            ),
            pca = list(
                k = 5L,
                lookback = 252L,
                align_factors = TRUE # §5.3 factor identity alignment
            ),
            resid = list(
                use_glasso = TRUE,
                lambda     = 0.1,
                lambda_e   = 0.97 # §5.5 EWMA decay for residual cov target
            ),
            factor = list(
                lambda_f = 0.97 # §5.4 EWMA decay for factor covariance
            ),
            hrp = list()
        ),

        # ── graph section ──
        graph = list(
            smoothing_alpha = 0.3,
            activation_thr  = 0.05,
            top_k           = 10L,
            K_min           = 2L,
            K_max           = 8L
        ),

        # ── signals section (architecture §7) ──
        signals = list(
            kalman = list(
                lookback  = 252L,
                q_var     = 1e-4,
                r_var     = 1e-2,
                scale     = 1.0,
                recursive = TRUE # §7.2: per-asset recursive Kalman
            ),
            tsmom = list(
                horizon  = 252L,
                horizons = c(21L, 63L, 126L, 252L), # §7.1 multiscale
                scale    = 2.0
            ),
            factor = list(
                horizons = c(21L, 63L) # §7.3 factor trend horizons
            ),
            scalarization = list(
                lambda_omega = 0.95 # §7.4 recursive weight smoothing
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

        # ── gating section (legacy 3-way + architecture 5-component §12.4) ──
        gating = list(
            # Legacy 3-way gating (backward compat)
            W = W0,
            w0 = c(kalman = 0, tsmom = 0, cash = -1),
            temperature = 1.0,
            # Architecture 5-component gating
            A_pi = A_pi_default,
            b_pi = b_pi_default,
            n_components = 5L
        ),

        # ── forecast section (architecture §12) ──
        forecast = list(
            label_horizon = 21L,
            ridge_lambda = 0.01,
            confidence_eps = 0.01,
            uncertainty_scale = 1.0,
            n_components = 5L, # §12.1
            kappa_min = 0.5, # §12.5 bounded confidence
            kappa_max = 1.5,
            lambda_err = 0.97, # §12.6 error state decay
            history_snapshots_keep = 252L,
            refit_every = 1L # daily refit (can be slowed)
        ),

        # ── portfolio section ──
        portfolio = list(
            gamma = 1.0,
            alpha_scale = 1.0,
            tilt = list(max_tilt = 2.0),
            caps = list(max_weight = 0.15),
            turnover_penalty = 0.0,
            max_turnover = 0.5
        ),

        # ── meta section ──
        meta = list(
            retain_windows         = FALSE,
            retain_matrices        = FALSE,
            mode                   = "production",
            strict_fallbacks       = FALSE,
            strict_warnings        = FALSE,
            strict_architecture    = FALSE,
            capture_stage_warnings = TRUE
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
    req <- c("data", "risk", "graph", "signals", "market_state", "gating", "forecast", "portfolio", "meta")
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

    # ---- graph section ----
    gr <- spec$graph
    if (!is.null(gr$smoothing_alpha) && (gr$smoothing_alpha < 0 || gr$smoothing_alpha > 1)) {
        stop("graph$smoothing_alpha must be in [0, 1]")
    }
    if (!is.null(gr$activation_thr) && (!is.finite(gr$activation_thr) || gr$activation_thr < 0)) {
        stop("graph$activation_thr must be finite and >= 0")
    }
    if (!is.null(gr$top_k) && (!is.finite(gr$top_k) || gr$top_k < 1)) {
        stop("graph$top_k must be >= 1")
    }
    if (!is.null(gr$K_min) && (!is.finite(gr$K_min) || gr$K_min < 1)) {
        stop("graph$K_min must be >= 1")
    }
    if (!is.null(gr$K_max) && (!is.finite(gr$K_max) || gr$K_max < 1)) {
        stop("graph$K_max must be >= 1")
    }
    if (!is.null(gr$K_min) && !is.null(gr$K_max) && gr$K_min > gr$K_max) {
        stop("graph$K_min cannot exceed graph$K_max")
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

    # ---- forecast section ----
    fc <- spec$forecast
    if (!is.null(fc$label_horizon) && (!is.finite(fc$label_horizon) || fc$label_horizon < 1)) {
        stop("forecast$label_horizon must be >= 1")
    }
    if (!is.null(fc$ridge_lambda) && (!is.finite(fc$ridge_lambda) || fc$ridge_lambda < 0)) {
        stop("forecast$ridge_lambda must be >= 0")
    }
    if (!is.null(fc$confidence_eps) && (!is.finite(fc$confidence_eps) || fc$confidence_eps <= 0)) {
        stop("forecast$confidence_eps must be > 0")
    }
    if (!is.null(fc$uncertainty_scale) && (!is.finite(fc$uncertainty_scale) || fc$uncertainty_scale < 0)) {
        stop("forecast$uncertainty_scale must be >= 0")
    }

    # ---- portfolio section ----
    p <- spec$portfolio
    if (!is.null(p$caps$max_weight) && (p$caps$max_weight <= 0 || p$caps$max_weight > 1)) {
        stop("portfolio$caps$max_weight must be in (0, 1]")
    }
    if (!is.null(p$tilt$max_tilt) && (!is.finite(p$tilt$max_tilt) || p$tilt$max_tilt < 1)) {
        stop("portfolio$tilt$max_tilt must be >= 1")
    }
    if (!is.null(p$gamma) && !is.finite(p$gamma)) {
        stop("portfolio$gamma must be finite")
    }
    if (!is.null(p$alpha_scale) && !is.finite(p$alpha_scale)) {
        stop("portfolio$alpha_scale must be finite")
    }
    if (!is.null(p$turnover_penalty) && (!is.finite(p$turnover_penalty) || p$turnover_penalty < 0)) {
        stop("portfolio$turnover_penalty must be >= 0")
    }
    if (!is.null(p$max_turnover) && (!is.finite(p$max_turnover) || p$max_turnover < 0 || p$max_turnover > 1)) {
        stop("portfolio$max_turnover must be in [0, 1]")
    }

    # ---- meta section ----
    m <- spec$meta
    bool_fields <- c(
        "retain_windows", "retain_matrices",
        "strict_fallbacks", "strict_warnings",
        "strict_architecture", "capture_stage_warnings"
    )
    for (nm in bool_fields) {
        if (!is.null(m[[nm]]) && (!is.logical(m[[nm]]) || length(m[[nm]]) != 1L || is.na(m[[nm]]))) {
            stop(sprintf("meta$%s must be TRUE/FALSE", nm))
        }
    }
    if (!is.null(m$mode) && (!is.character(m$mode) || length(m$mode) != 1L)) {
        stop("meta$mode must be a length-1 character string")
    }

    invisible(spec)
}

# ── Snapshot artifact validation ─────────────────────────────────────────────

#' @export
me_validate_snapshot_artifact <- function(x) {
    req <- c(
        "as_of_date", "tradable_symbols", "target_weights",
        "cash_weight", "risk", "signals", "market_state",
        "gating", "portfolio_diag", "meta", "warnings",
        "model_state_out"
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

    # Gating consistency — legacy 3-way path
    g <- x$gating
    if (length(g) > 0 && !is.null(g$w_kalman)) {
        gate_sum <- g$w_kalman + g$w_tsmom + g$w_cash
        if (is.finite(gate_sum) && abs(gate_sum - 1.0) > 1e-6) {
            stop("Gating softmax weights do not sum to 1")
        }
        # Note: gross_exposure vs cash_weight check relaxed because optimizer controls
        # may legitimately adjust gross_exposure after gating (§12.8).
    }

    # Gating consistency — architecture 5-component path (if present)
    if (!is.null(g$pi_t) && is.numeric(g$pi_t)) {
        if (abs(sum(g$pi_t) - 1.0) > 1e-6) {
            stop("Architecture gating pi_t does not sum to 1")
        }
    }

    # Risk universe alignment (covariance-based, not HRP-based)
    risk <- x$risk
    Sigma_ref <- risk$Sigma_risk_H %||% risk$Sigma_total %||% risk$Sigma_risk_1

    if (!is.null(Sigma_ref)) {
        if (!is.matrix(Sigma_ref)) stop("Risk covariance must be a matrix")
        if (nrow(Sigma_ref) != ncol(Sigma_ref)) stop("Risk covariance must be square")
        if (is.null(rownames(Sigma_ref)) || is.null(colnames(Sigma_ref))) {
            stop("Risk covariance must have rownames and colnames")
        }
        if (!identical(rownames(Sigma_ref), colnames(Sigma_ref))) {
            stop("Risk covariance rownames/colnames must match and be in same order")
        }
        if (nrow(tgt) > 0 && !all(tgt$symbol %in% colnames(Sigma_ref))) {
            stop("Some target symbols are not in risk covariance universe")
        }
    }

    # Optional baseline alignment check (transitional; not architecture-defining)
    if (!is.null(risk$w_baseline) && length(risk$w_baseline) > 0 && !is.null(Sigma_ref)) {
        if (!all(names(risk$w_baseline) %in% colnames(Sigma_ref))) {
            stop("risk$w_baseline symbols are not contained in risk covariance universe")
        }
    }

    invisible(x)
}

# ── Model state validation (for recursive carry-forward) ─────────────────────
# All recursive state fields are optional (NULL is valid for cold start).
# When present, basic type checks apply.

#' @export
me_validate_model_state <- function(state) {
    if (is.null(state)) {
        return(invisible(NULL))
    }

    if (!is.list(state)) stop("model_state must be a list or NULL")

    # prev_target validation
    if (!is.null(state$prev_target)) {
        pt <- state$prev_target
        if (!is.data.frame(pt) || !all(c("symbol", "weight_target") %in% names(pt))) {
            stop("model_state$prev_target must be a data.frame with symbol + weight_target")
        }
    }

    # Recursive risk states (all optional, type-checked if present)
    if (!is.null(state$ewma_vol_state) && !is.numeric(state$ewma_vol_state)) {
        stop("model_state$ewma_vol_state must be a named numeric vector or NULL")
    }
    if (!is.null(state$factor_cov_state) && !is.matrix(state$factor_cov_state)) {
        stop("model_state$factor_cov_state must be a matrix or NULL")
    }
    if (!is.null(state$resid_cov_state) && !is.matrix(state$resid_cov_state)) {
        stop("model_state$resid_cov_state must be a matrix or NULL")
    }
    if (!is.null(state$B_prev) && !is.matrix(state$B_prev)) {
        stop("model_state$B_prev must be a matrix or NULL")
    }

    # Graph recursive states
    if (!is.null(state$edge_stability) && !is.matrix(state$edge_stability)) {
        stop("model_state$edge_stability must be a matrix or NULL")
    }
    if (!is.null(state$node_stability) && !is.numeric(state$node_stability)) {
        stop("model_state$node_stability must be a named numeric vector or NULL")
    }

    # Kalman states
    if (!is.null(state$kalman_states) && !is.list(state$kalman_states)) {
        stop("model_state$kalman_states must be a list or NULL")
    }

    # Scalarization w eights
    if (!is.null(state$scalar_weights) && !is.list(state$scalar_weights)) {
        stop("model_state$scalar_weights must be a list or NULL")
    }

    invisible(state)
}

# ── Spec hash ────────────────────────────────────────────────────────────────

#' @export
me_hash_spec <- function(spec) {
    me_require("digest")
    digest::digest(spec)
}
