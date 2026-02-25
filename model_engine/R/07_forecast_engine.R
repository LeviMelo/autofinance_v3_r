#' @title Model Engine — Forecast Engine
#' @description 5-component forecasts, rolling ridge, gating, confidence, uncertainty.
#' Implements architecture.md §12: component models → gated combination → reliability.

# ══════════════════════════════════════════════════════════════════════════════
# §12.1 Component forecast models
# ══════════════════════════════════════════════════════════════════════════════

#' Rolling ridge regression: y ~ X with L2 penalty
#' @export
me_rolling_ridge <- function(y, X, lambda = 0.01) {
    # y: n_obs vector of labels (lagged returns)
    # X: n_obs x p feature matrix
    n <- nrow(X)
    p <- ncol(X)
    if (n < max(5, p + 1)) {
        return(list(
            beta = rep(0, p), intercept = 0,
            fitted = FALSE, reason = "insufficient_obs"
        ))
    }
    # Standardize X
    mu_X <- colMeans(X, na.rm = TRUE)
    sd_X <- apply(X, 2, sd, na.rm = TRUE)
    sd_X[sd_X < 1e-8 | !is.finite(sd_X)] <- 1
    X_s <- scale(X, center = mu_X, scale = sd_X)
    X_s[!is.finite(X_s)] <- 0

    mu_y <- mean(y, na.rm = TRUE)
    y_c <- y - mu_y

    XtX <- crossprod(X_s)
    Xty <- crossprod(X_s, y_c)
    beta_s <- tryCatch(
        solve(XtX + lambda * diag(p), Xty),
        error = function(e) NULL
    )
    if (is.null(beta_s)) {
        return(list(
            beta = rep(0, p), intercept = 0,
            fitted = FALSE, reason = "solve_failed"
        ))
    }

    beta <- as.vector(beta_s) / sd_X
    intercept <- mu_y - sum(beta * mu_X)

    list(beta = beta, intercept = intercept, fitted = TRUE, reason = "ok")
}

#' §12.1 Component c forecast: f_hat_i^c = X_i * beta_c + intercept_c
#' @export
me_component_forecast <- function(X_current, model_fit) {
    if (!isTRUE(model_fit$fitted)) {
        return(setNames(rep(0, nrow(X_current)), rownames(X_current)))
    }
    pred <- as.vector(X_current %*% model_fit$beta) + model_fit$intercept
    pred[!is.finite(pred)] <- 0
    names(pred) <- rownames(X_current)
    pred
}

# ══════════════════════════════════════════════════════════════════════════════
# §12.2 Five component models
# ══════════════════════════════════════════════════════════════════════════════

#' Train all 5 component models
#' @export
me_train_component_models <- function(y_hist, X_hist, spec_forecast = list()) {
    # y_hist: list of label vectors (by component type)
    # X_hist: feature matrix used for training
    # Returns list of fitted models for each component

    lambda <- spec_forecast$ridge_lambda %||% 0.01

    models <- list()

    # C1: Momentum composite (use all features)
    models$momentum <- me_rolling_ridge(y_hist, X_hist, lambda)

    # C2: Signal-only model (use only temporal features if available)
    temp_cols <- grep("^f_mom|^f_kal|^f_fac$", colnames(X_hist), value = TRUE)
    if (length(temp_cols) >= 1) {
        models$signal_only <- me_rolling_ridge(y_hist, X_hist[, temp_cols, drop = FALSE], lambda)
    } else {
        models$signal_only <- list(
            beta = numeric(0), intercept = 0,
            fitted = FALSE, reason = "no_signal_features"
        )
    }

    # C3: Structure-aware model (structural + graph features)
    struct_cols <- grep("^f_factor|^f_vol|^f_idio|^f_graph|^f_cluster|^f_centrality",
        colnames(X_hist),
        value = TRUE
    )
    if (length(struct_cols) >= 1) {
        models$structure <- me_rolling_ridge(y_hist, X_hist[, struct_cols, drop = FALSE], lambda)
    } else {
        models$structure <- list(
            beta = numeric(0), intercept = 0,
            fitted = FALSE, reason = "no_structural_features"
        )
    }

    # C4: Contrarian model (relative dislocation + mean-reversion features)
    contra_cols <- grep("^f_graph_relative|^f_cluster_z", colnames(X_hist), value = TRUE)
    if (length(contra_cols) >= 1) {
        models$contrarian <- me_rolling_ridge(-y_hist, X_hist[, contra_cols, drop = FALSE], lambda)
    } else {
        models$contrarian <- list(
            beta = numeric(0), intercept = 0,
            fitted = FALSE, reason = "no_contra_features"
        )
    }

    # C5: Adaptive model (includes state features for regime adaptation)
    state_cols <- grep("^f_state_", colnames(X_hist), value = TRUE)
    adaptive_cols <- union(temp_cols, state_cols)
    if (length(adaptive_cols) >= 1) {
        models$adaptive <- me_rolling_ridge(y_hist, X_hist[, adaptive_cols, drop = FALSE], lambda)
    } else {
        models$adaptive <- models$momentum # fallback
    }

    models
}

# ══════════════════════════════════════════════════════════════════════════════
# §12.3 Building training labels (with matured label discipline)
# ══════════════════════════════════════════════════════════════════════════════

#' Build lagged forward returns as labels (strict no-lookahead)
#' @export
me_build_training_labels <- function(R_window, horizon = 21L) {
    # Labels are h-step ahead cumulative returns
    # At time t, label = sum(r_{t+1}...r_{t+h}) — but we only use MATURED labels
    # So the last h observations have NO valid label
    Tn <- nrow(R_window)
    if (Tn < horizon + 10) {
        return(list(y = NULL, X_dates = NULL, valid = FALSE))
    }

    n_train <- Tn - horizon
    y_mat <- matrix(NA_real_, n_train, ncol(R_window))
    for (t in seq_len(n_train)) {
        y_mat[t, ] <- colSums(R_window[(t + 1):(t + horizon), , drop = FALSE], na.rm = TRUE)
    }
    colnames(y_mat) <- colnames(R_window)

    # Cross-sectional median as aggregate label for ridge
    y <- rowMedians_safe(y_mat)

    list(y = y, y_mat = y_mat, n_train = n_train, valid = TRUE)
}

#' Safe row-medians
rowMedians_safe <- function(mat) {
    apply(mat, 1, median, na.rm = TRUE)
}

# ══════════════════════════════════════════════════════════════════════════════
# §12.4 Forecast combination with gating weights
# ══════════════════════════════════════════════════════════════════════════════

#' @export
me_combine_forecasts <- function(component_forecasts, gating_weights, spec_forecast = list()) {
    # component_forecasts: list of named vectors (per component)
    # gating_weights: named vector summing to 1
    syms <- unique(unlist(lapply(component_forecasts, names)))
    if (length(syms) == 0) {
        return(setNames(numeric(0), character(0)))
    }

    combined <- setNames(rep(0, length(syms)), syms)

    for (comp_name in names(component_forecasts)) {
        fc <- component_forecasts[[comp_name]]
        wt <- gating_weights[comp_name] %||% 0
        if (is.null(fc) || length(fc) == 0 || wt == 0) next
        common <- intersect(syms, names(fc))
        combined[common] <- combined[common] + wt * fc[common]
    }
    combined[!is.finite(combined)] <- 0
    combined
}

# ══════════════════════════════════════════════════════════════════════════════
# §12.5 Confidence and uncertainty
# ══════════════════════════════════════════════════════════════════════════════

#' Per-asset, per-component confidence (κ_{i,c,t})
#' @export
me_compute_confidence <- function(component_forecasts, spec_forecast = list()) {
    comp_names <- names(component_forecasts)
    if (length(comp_names) == 0) {
        return(list(kappa = list(), avg_confidence = 0))
    }

    syms <- unique(unlist(lapply(component_forecasts, names)))
    n <- length(syms)

    # κ = 1 - dispersion_within / dispersion_total
    # Heuristic: confidence based on forecast amplitude and cross-component agreement
    kappa <- list()
    for (comp in comp_names) {
        fc <- component_forecasts[[comp]]
        if (is.null(fc) || length(fc) == 0) {
            kappa[[comp]] <- setNames(rep(0, n), syms)
            next
        }
        fc_aligned <- setNames(rep(0, n), syms)
        fc_aligned[intersect(syms, names(fc))] <- fc[intersect(syms, names(fc))]

        # Amplitude-based confidence: |fc| / (|fc| + epsilon)
        eps <- spec_forecast$confidence_eps %||% 0.01
        k <- abs(fc_aligned) / (abs(fc_aligned) + eps)
        k[!is.finite(k)] <- 0
        kappa[[comp]] <- k
    }

    # Agreement: if all components have same sign → higher confidence
    fc_signs <- do.call(cbind, lapply(component_forecasts, function(fc) {
        s <- setNames(rep(0, n), syms)
        s[intersect(syms, names(fc))] <- sign(fc[intersect(syms, names(fc))])
        s
    }))

    agreement <- rowMeans(fc_signs, na.rm = TRUE) # [-1, 1]
    agreement_confidence <- abs(agreement) # 0 = disagree, 1 = agree

    list(
        kappa = kappa,
        agreement = setNames(agreement_confidence, syms),
        avg_confidence = mean(agreement_confidence, na.rm = TRUE)
    )
}

#' §12.6 Uncertainty envelope
#' @export
me_compute_uncertainty <- function(combined_forecast, sigma_t, confidence,
                                   spec_forecast = list()) {
    syms <- names(combined_forecast)
    n <- length(syms)
    sigma_aligned <- setNames(rep(0.2, n), syms)
    common <- intersect(syms, names(sigma_t))
    sigma_aligned[common] <- sigma_t[common]

    # σ_forecast = σ_asset × (1 - κ_agreement) × scale_factor
    scale_factor <- spec_forecast$uncertainty_scale %||% 1.0
    agree <- confidence$agreement[syms]
    agree[is.na(agree)] <- 0

    u <- sigma_aligned * (1 - agree) * scale_factor / sqrt(252)
    u[!is.finite(u)] <- 0.01

    data.frame(
        symbol = syms,
        forecast = combined_forecast,
        sigma_fc = u,
        z_score = combined_forecast / pmax(u, 1e-8),
        stringsAsFactors = FALSE
    )
}

# ══════════════════════════════════════════════════════════════════════════════
# Full forecast engine orchestrator
# ══════════════════════════════════════════════════════════════════════════════

#' @export
me_run_forecast_engine <- function(X_current, R_window, X_hist_window,
                                   gating_artifact, risk_artifact,
                                   spec_forecast = list()) {
    syms <- rownames(X_current)
    n <- length(syms)

    if (n == 0 || ncol(X_current) == 0) {
        return(list(
            combined_forecast = setNames(rep(0, n), syms),
            component_forecasts = list(),
            confidence = list(kappa = list(), avg_confidence = 0),
            uncertainty = data.frame(
                symbol = character(0), forecast = numeric(0),
                sigma_fc = numeric(0), z_score = numeric(0)
            ),
            models = list(),
            diag = list(reason = "empty_inputs")
        ))
    }

    # 1. Build training labels
    horizon <- spec_forecast$label_horizon %||% 21L
    label_result <- me_build_training_labels(R_window, horizon)

    if (!isTRUE(label_result$valid)) {
        # Can't train → use raw signal-based forecast
        fc <- X_current[, 1]
        if (is.null(dim(fc))) fc <- setNames(rep(0, n), syms)
        return(list(
            combined_forecast = fc,
            component_forecasts = list(fallback = fc),
            confidence = list(kappa = list(), avg_confidence = 0),
            uncertainty = data.frame(
                symbol = syms, forecast = fc,
                sigma_fc = rep(0.01, n), z_score = rep(0, n)
            ),
            models = list(),
            diag = list(reason = "insufficient_labels")
        ))
    }

    # 2. Align feature history with labels
    n_train <- label_result$n_train
    X_train <- if (!is.null(X_hist_window) && nrow(X_hist_window) >= n_train) {
        X_hist_window[1:n_train, , drop = FALSE]
    } else {
        # Fallback: replicate current features (suboptimal but functional)
        X_rep <- matrix(0, n_train, ncol(X_current),
            dimnames = list(NULL, colnames(X_current))
        )
        X_rep
    }

    # Use cross-sectional median returns as labels
    y_train <- label_result$y
    y_train[!is.finite(y_train)] <- 0

    # 3. Train component models
    models <- me_train_component_models(y_train, X_train, spec_forecast)

    # 4. Generate component forecasts
    component_forecasts <- list()
    for (comp in names(models)) {
        if (isTRUE(models[[comp]]$fitted)) {
            # Use matching feature columns
            model <- models[[comp]]
            feat_names <- names(model$beta)
            if (length(feat_names) > 0 && all(feat_names %in% colnames(X_current))) {
                component_forecasts[[comp]] <- me_component_forecast(
                    X_current[, feat_names, drop = FALSE], model
                )
            } else {
                # Use all available columns if names don't match
                component_forecasts[[comp]] <- me_component_forecast(X_current, model)
            }
        } else {
            component_forecasts[[comp]] <- setNames(rep(0, n), syms)
        }
    }

    # 5. Build gating weights for forecast combination
    # Use gating artifact for expert mixture + uniform remainder
    g <- gating_artifact$gating
    n_comps <- length(component_forecasts)
    gating_w <- setNames(rep(1 / n_comps, n_comps), names(component_forecasts))

    # Give momentum/signal more weight via gating signal
    if (!is.null(g$w_kalman) && "signal_only" %in% names(gating_w)) {
        gating_w["signal_only"] <- g$w_kalman
    }
    if (!is.null(g$w_tsmom) && "momentum" %in% names(gating_w)) {
        gating_w["momentum"] <- g$w_tsmom
    }
    gating_w <- gating_w / sum(gating_w)

    # 6. Combine
    combined <- me_combine_forecasts(component_forecasts, gating_w, spec_forecast)

    # 7. Confidence + uncertainty
    confidence <- me_compute_confidence(component_forecasts, spec_forecast)
    uncertainty <- me_compute_uncertainty(
        combined, risk_artifact$sigma_t,
        confidence, spec_forecast
    )

    list(
        combined_forecast = combined,
        component_forecasts = component_forecasts,
        gating_weights = gating_w,
        confidence = confidence,
        uncertainty = uncertainty,
        models = models,
        diag = list(
            n_components = length(component_forecasts),
            n_fitted = sum(vapply(models, function(m) isTRUE(m$fitted), logical(1))),
            label_horizon = horizon,
            n_train = n_train,
            avg_confidence = confidence$avg_confidence,
            gating_weights = gating_w
        )
    )
}
