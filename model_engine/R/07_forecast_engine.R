#' @title Model Engine — Forecast Engine
#' @description Architecture §12: 5-component asset-date panel forecast with global softmax
#' mixture, bounded confidence multipliers, stagger-bucket error states, asset reliability.
#' Preserves legacy fallback path explicitly flagged.

# ══════════════════════════════════════════════════════════════════════════════
# §12.0 Training panel construction (causal)
# ══════════════════════════════════════════════════════════════════════════════

#' Build causal training panel: {(X_{i,s}, y^(H)_{i,s}) : s <= t-H}
#' @export
me_build_training_panel <- function(feature_history, R_history, horizon = 21L,
                                    current_date_idx = NULL) {
    # feature_history: list of snapshots, each with $X (data.frame/matrix n×p) and $date
    # R_history: matrix of returns (T × n_assets, with rownames = dates)
    # Returns: list(X = matrix, y = vector, asset = char, date = char)

    if (is.null(feature_history) || length(feature_history) < horizon + 1) {
        return(list(X = NULL, y = NULL, n_obs = 0, n_features = 0))
    }

    all_X <- list()
    all_y <- c()
    all_asset <- c()
    all_date <- c()

    n_snaps <- length(feature_history)
    # Only use snapshots where labels have matured: up to t - H
    usable_end <- n_snaps - horizon

    if (usable_end < 1) {
        return(list(X = NULL, y = NULL, n_obs = 0, n_features = 0))
    }

    for (s in seq_len(usable_end)) {
        snap <- feature_history[[s]]
        if (is.null(snap$X)) next

        X_s <- as.matrix(snap$X)
        snap_syms <- rownames(X_s)
        if (is.null(snap_syms) || length(snap_syms) == 0) next

        # Forward returns as labels (H-day cumulative)
        # Need returns from s+1 to s+H
        date_s <- snap$date_idx %||% s
        label_start <- date_s + 1
        label_end <- date_s + horizon

        if (label_end > nrow(R_history)) next

        for (sym in snap_syms) {
            if (!(sym %in% colnames(R_history))) next
            r_forward <- R_history[label_start:label_end, sym]
            if (any(!is.finite(r_forward))) next
            y_label <- sum(r_forward) # H-day cumulative return

            all_X[[length(all_X) + 1]] <- X_s[sym, , drop = TRUE]
            all_y <- c(all_y, y_label)
            all_asset <- c(all_asset, sym)
            all_date <- c(all_date, snap$date %||% as.character(s))
        }
    }

    if (length(all_y) < 10) {
        return(list(X = NULL, y = NULL, n_obs = 0, n_features = 0))
    }

    X_panel <- do.call(rbind, all_X)
    X_panel[!is.finite(X_panel)] <- 0

    list(
        X = X_panel,
        y = all_y,
        asset = all_asset,
        date = all_date,
        n_obs = length(all_y),
        n_features = ncol(X_panel)
    )
}

# ══════════════════════════════════════════════════════════════════════════════
# §12.1 Component feature selectors
# ══════════════════════════════════════════════════════════════════════════════

.component_feature_selector <- function(feature_names, component_id) {
    # Select features relevant to each architecture component
    switch(as.character(component_id),
        "1" = {
            # C1: temporal continuation (raw/resid TSMOM, Kalman, factor, scalar, liquidity interactions)
            grep("^f_(mom|kal|fac|tsmom|resid_tsmom|scalar|kal_slope|kal_uncert|kal_innov|mom_x_liq|kal_x_liq)",
                feature_names,
                value = TRUE
            )
        },
        "2" = {
            # C2: structural continuation (graph peer, shrinkage, mixed, signed)
            grep("^f_(graph_peer|graph_shr|graph_mixed|graph_signed|cluster_z)",
                feature_names,
                value = TRUE
            )
        },
        "3" = {
            # C3: neighborhood mean-reversion (relative, tension, cluster)
            grep("^f_(graph_relative|graph_tension|cluster_z|centrality)",
                feature_names,
                value = TRUE
            )
        },
        "4" = {
            # C4: PCA-graph dislocation (pca_graph features, factor exposure, structural)
            grep("^f_(pca_graph|factor_exposure|idio_frac|vol_rank)",
                feature_names,
                value = TRUE
            )
        },
        "5" = {
            # C5: liquidity-conditioned correction (liquidity, illiq, liq interactions)
            grep("^f_(liquidity|traded|illiq|liq_zscore|avg_trade|n_trades|mom_x_liq|kal_x_liq)",
                feature_names,
                value = TRUE
            )
        },
        feature_names # fallback: all features
    )
}

# ══════════════════════════════════════════════════════════════════════════════
# §12.2 Per-component ridge regression
# ══════════════════════════════════════════════════════════════════════════════

#' @export
me_train_component_model <- function(X, y, feature_cols, lambda = 0.01) {
    # Ridge regression for a single component
    if (is.null(X) || length(y) < 10 || length(feature_cols) == 0) {
        return(list(beta = NULL, intercept = 0, n_obs = 0, n_features = 0, fitted = FALSE))
    }

    # Select columns
    avail_cols <- intersect(feature_cols, colnames(X))
    if (length(avail_cols) < 1) {
        return(list(beta = NULL, intercept = 0, n_obs = 0, n_features = 0, fitted = FALSE))
    }

    Xc <- X[, avail_cols, drop = FALSE]
    Xc[!is.finite(Xc)] <- 0

    # Standardize X columns
    x_means <- colMeans(Xc)
    x_sds <- apply(Xc, 2, sd)
    x_sds[x_sds <= 0 | !is.finite(x_sds)] <- 1
    Xc <- scale(Xc, center = x_means, scale = x_sds)
    Xc[!is.finite(Xc)] <- 0

    y_mean <- mean(y, na.rm = TRUE)
    yc <- y - y_mean

    p <- ncol(Xc)
    XtX <- t(Xc) %*% Xc + lambda * diag(p)
    Xty <- t(Xc) %*% yc

    beta_std <- tryCatch(
        as.vector(solve(XtX, Xty)),
        error = function(e) rep(0, p)
    )

    # Unstandardize
    beta <- beta_std / x_sds
    intercept <- y_mean - sum(beta * x_means)
    names(beta) <- avail_cols

    list(
        beta = beta,
        intercept = intercept,
        x_means = x_means,
        x_sds = x_sds,
        y_mean = y_mean,
        feature_cols = avail_cols,
        n_obs = nrow(Xc),
        n_features = p,
        fitted = TRUE
    )
}

#' @export
me_predict_component <- function(X_new, model) {
    # Predict from trained component model
    if (is.null(model) || !isTRUE(model$fitted) || is.null(model$beta)) {
        return(rep(0, nrow(X_new)))
    }

    avail <- intersect(model$feature_cols, colnames(X_new))
    if (length(avail) == 0) {
        return(rep(0, nrow(X_new)))
    }

    Xc <- X_new[, avail, drop = FALSE]
    Xc[!is.finite(Xc)] <- 0

    pred <- as.vector(Xc %*% model$beta[avail]) + model$intercept
    pred[!is.finite(pred)] <- 0
    pred
}

# ══════════════════════════════════════════════════════════════════════════════
# §12.4 Global softmax mixture weights π_t
# ══════════════════════════════════════════════════════════════════════════════

#' @export
me_forecast_softmax_weights <- function(m_t, A_pi, b_pi, temperature = 1.0) {
    # π_t = softmax((A_pi %*% m_t + b_pi) / temperature)
    n_comp <- nrow(A_pi)
    z <- as.vector(A_pi %*% m_t + b_pi) / temperature
    z <- z - max(z) # numerical stability
    e <- exp(z)
    pi_t <- e / sum(e)
    pi_t[!is.finite(pi_t)] <- 1 / n_comp
    names(pi_t) <- paste0("C", seq_len(n_comp))
    pi_t
}

# ══════════════════════════════════════════════════════════════════════════════
# §12.5 Bounded confidence multipliers κ_{i,c,t}
# ══════════════════════════════════════════════════════════════════════════════

#' @export
me_bounded_kappa <- function(X_i, kappa_min = 0.5, kappa_max = 1.5) {
    # κ_{i,c,t} = kappa_min + (kappa_max - kappa_min) * σ(||x_i||_2 - threshold)
    # Simple: scale by feature magnitude
    x_norm <- sqrt(sum(X_i^2, na.rm = TRUE))
    # Sigmoid centered at median feature norm
    raw <- 1 / (1 + exp(-(x_norm - 1)))
    kappa <- kappa_min + (kappa_max - kappa_min) * raw
    if (!is.finite(kappa)) kappa <- 1.0
    kappa
}

# ══════════════════════════════════════════════════════════════════════════════
# §12.6 Stagger-bucket component error states
# ══════════════════════════════════════════════════════════════════════════════

#' @export
me_update_error_states <- function(error_states_prev, predictions, actuals, horizon,
                                   lambda_err = 0.97) {
    # H stagger buckets per component
    # Only update bucket (t mod H) when labels mature
    n_comp <- length(predictions)

    if (is.null(error_states_prev)) {
        # Initialize: one variance per component
        error_states_prev <- lapply(seq_len(n_comp), function(c) {
            list(
                sigma2 = rep(0.01, horizon), # H buckets
                n_updates = rep(0L, horizon)
            )
        })
        names(error_states_prev) <- names(predictions)
    }

    bucket <- ((error_states_prev[[1]]$n_updates[1] %||% 0) %% horizon) + 1

    out <- error_states_prev
    for (c in seq_len(n_comp)) {
        nm <- names(predictions)[c]
        if (is.null(nm)) nm <- paste0("C", c)
        if (!is.null(actuals) && !is.null(actuals[[nm]])) {
            err <- (predictions[[nm]] - actuals[[nm]])^2
            err <- mean(err, na.rm = TRUE)
            if (is.finite(err)) {
                if (is.null(out[[nm]])) {
                    out[[nm]] <- list(sigma2 = rep(0.01, horizon), n_updates = rep(0L, horizon))
                }
                out[[nm]]$sigma2[bucket] <- lambda_err * out[[nm]]$sigma2[bucket] +
                    (1 - lambda_err) * err
                out[[nm]]$n_updates[bucket] <- out[[nm]]$n_updates[bucket] + 1L
            }
        }
    }
    out
}

#' @export
me_component_uncertainty <- function(error_states, horizon) {
    # σ̂^(H) per component = sqrt(mean across H buckets)
    if (is.null(error_states)) {
        return(NULL)
    }
    sapply(error_states, function(es) {
        if (is.null(es) || is.null(es$sigma2)) {
            return(0.01)
        }
        sqrt(mean(es$sigma2, na.rm = TRUE))
    })
}

# ══════════════════════════════════════════════════════════════════════════════
# §12.7 Reliability score ρ_rel and effective forecast
# ══════════════════════════════════════════════════════════════════════════════

#' @export
me_reliability_score <- function(X_i, liquidity_features = NULL,
                                 node_stability = NULL) {
    # ρ_rel = σ(θ_rel' z_rel)
    # z_rel = [liquidity_z, node_stability, data_coverage]
    # Simple deterministic version
    z_sum <- 0
    n_features <- 0

    if (!is.null(liquidity_features)) {
        liq <- liquidity_features$f_liquidity %||% 0
        z_sum <- z_sum + liq
        n_features <- n_features + 1
    }
    if (!is.null(node_stability)) {
        z_sum <- z_sum + node_stability
        n_features <- n_features + 1
    }

    if (n_features == 0) {
        return(0.8)
    } # default
    z_mean <- z_sum / n_features
    rho <- 1 / (1 + exp(-z_mean)) # (0,1)
    rho <- max(0.1, min(1.0, rho))
    rho
}

# ══════════════════════════════════════════════════════════════════════════════
# §12 Full forecast engine orchestrator
# ══════════════════════════════════════════════════════════════════════════════

#' @export
me_run_forecast_engine <- function(feature_artifact, risk_artifact, graph_artifact,
                                   signal_artifact, market_state_vec,
                                   spec_forecast, spec_gating,
                                   feature_history = NULL, R_history = NULL,
                                   model_state = NULL) {
    syms <- rownames(feature_artifact$X)
    n_assets <- length(syms)
    X_now <- as.matrix(feature_artifact$X)
    feature_names <- colnames(X_now)

    n_comp <- spec_forecast$n_components %||% 5L
    horizon <- spec_forecast$label_horizon %||% 21L
    ridge_lambda <- spec_forecast$ridge_lambda %||% 0.01
    kappa_min <- spec_forecast$kappa_min %||% 0.5
    kappa_max <- spec_forecast$kappa_max %||% 1.5
    lambda_err <- spec_forecast$lambda_err %||% 0.97
    refit_every <- spec_forecast$refit_every %||% 1L

    # Extract previous model state
    comp_models_prev <- if (!is.null(model_state)) model_state$component_model_fits else NULL
    error_states_prev <- if (!is.null(model_state)) model_state$component_error_states else NULL
    forecast_step <- if (!is.null(model_state)) (model_state$forecast_step %||% 0L) + 1L else 1L

    # Flag: architecture vs fallback
    use_arch_path <- !is.null(feature_history) && length(feature_history) >= horizon + 10 &&
        !is.null(R_history) && nrow(R_history) >= horizon + 10

    if (!use_arch_path) {
        # ── FALLBACK: Legacy confidence-scaled forecast ──
        # FALLBACK:forecast_legacy_path
        s_mom <- signal_artifact$s_mom[syms]
        s_kal <- signal_artifact$s_kal[syms]

        mu_hat <- (s_mom + s_kal) / 2
        mu_hat[!is.finite(mu_hat)] <- 0

        conf_scale <- spec_forecast$confidence_eps %||% 0.01
        unc <- rep(spec_forecast$uncertainty_scale %||% 1.0, n_assets)
        names(unc) <- syms

        mu_eff <- mu_hat * 0.8 # conservative reliability scaling
        s_eff <- unc / sqrt(0.8)

        return(list(
            mu_hat = mu_hat,
            mu_eff = mu_eff,
            sigma_hat = unc,
            s_eff = s_eff,
            confidence = rep(0.6, n_assets),
            pi_t = setNames(rep(1 / n_comp, n_comp), paste0("C", seq_len(n_comp))),
            component_mu = NULL,
            rho_rel = rep(0.8, n_assets),
            diag = list(
                method = "FALLBACK:forecast_legacy_path",
                reason = if (is.null(feature_history) || length(feature_history) < horizon + 10) {
                    "insufficient_feature_history"
                } else {
                    "insufficient_R_history"
                },
                n_history = length(feature_history),
                forecast_step = forecast_step
            ),
            forecast_state_out = list(
                forecast_step = forecast_step
            )
        ))
    }

    # ── ARCHITECTURE PATH: 5-component asset-date panel ──

    # 1. Build training panel (causal)
    panel <- me_build_training_panel(feature_history, R_history, horizon)

    if (panel$n_obs < 20) {
        # Not enough data yet → fallback
        mu_hat <- rep(0, n_assets)
        names(mu_hat) <- syms
        return(list(
            mu_hat = mu_hat,
            mu_eff = mu_hat,
            sigma_hat = rep(1, n_assets),
            s_eff = rep(1, n_assets),
            confidence = rep(0.5, n_assets),
            pi_t = setNames(rep(1 / n_comp, n_comp), paste0("C", seq_len(n_comp))),
            component_mu = NULL,
            rho_rel = rep(0.5, n_assets),
            diag = list(
                method = "FALLBACK:insufficient_panel", n_obs = panel$n_obs,
                forecast_step = forecast_step
            ),
            forecast_state_out = list(forecast_step = forecast_step)
        ))
    }

    # 2. Train (or reuse) component models
    should_refit <- is.null(comp_models_prev) || (forecast_step %% refit_every == 0)

    comp_models <- if (should_refit) {
        lapply(seq_len(n_comp), function(c) {
            feat_cols <- .component_feature_selector(colnames(panel$X), c)
            me_train_component_model(panel$X, panel$y, feat_cols, ridge_lambda)
        })
    } else {
        comp_models_prev
    }
    names(comp_models) <- paste0("C", seq_len(n_comp))

    # 3. Generate component forecasts μ^(c)_{i,t}
    component_mu <- matrix(0, n_assets, n_comp,
        dimnames = list(syms, paste0("C", seq_len(n_comp)))
    )
    for (c in seq_len(n_comp)) {
        component_mu[, c] <- me_predict_component(X_now, comp_models[[c]])
    }

    # 4. Global softmax mixture weights π_t (§12.4)
    A_pi <- spec_gating$A_pi
    b_pi <- spec_gating$b_pi
    if (is.null(A_pi) || is.null(market_state_vec)) {
        pi_t <- setNames(rep(1 / n_comp, n_comp), paste0("C", seq_len(n_comp)))
    } else {
        m_t <- market_state_vec
        # Pad or trim m_t to match A_pi columns
        if (length(m_t) < ncol(A_pi)) {
            m_t <- c(m_t, rep(0, ncol(A_pi) - length(m_t)))
        } else if (length(m_t) > ncol(A_pi)) {
            m_t <- m_t[seq_len(ncol(A_pi))]
        }
        pi_t <- me_forecast_softmax_weights(m_t, A_pi, b_pi,
            temperature = spec_gating$temperature %||% 1.0
        )
    }

    # 5. Bounded κ_{i,c,t} and normalized q_{i,c,t} (§12.5)
    kappa <- matrix(1, n_assets, n_comp, dimnames = list(syms, paste0("C", seq_len(n_comp))))
    for (i in seq_len(n_assets)) {
        kappa[i, ] <- me_bounded_kappa(X_now[i, ], kappa_min, kappa_max)
    }

    # q_{i,c} = π_c × κ_{i,c} / Σ_c(π_c × κ_{i,c})
    q <- sweep(kappa, 2, pi_t, "*")
    q_sums <- rowSums(q)
    q_sums[q_sums <= 0] <- 1
    q <- q / q_sums

    # 6. Final μ̂^(H)
    mu_hat <- rowSums(q * component_mu)
    names(mu_hat) <- syms

    # 7. Component uncertainty (§12.6)
    comp_sigma <- me_component_uncertainty(error_states_prev, horizon)
    if (is.null(comp_sigma) || length(comp_sigma) != n_comp) {
        comp_sigma <- rep(0.01, n_comp)
    }

    # Asset-level forecast uncertainty: weighted by π
    sigma_hat <- rep(0, n_assets)
    for (i in seq_len(n_assets)) {
        sigma_hat[i] <- sqrt(sum(q[i, ]^2 * comp_sigma^2))
    }
    sigma_hat[sigma_hat <= 0 | !is.finite(sigma_hat)] <- 0.01
    names(sigma_hat) <- syms

    # 8. Reliability ρ_rel (§12.7)
    liq_feats <- feature_artifact$X[, grep("^f_liquidity$", names(feature_artifact$X)), drop = FALSE]
    node_stab <- if (!is.null(graph_artifact$graph_state_out)) {
        graph_artifact$graph_state_out$node_stability
    } else {
        NULL
    }

    rho_rel <- rep(0.8, n_assets)
    names(rho_rel) <- syms
    for (i in seq_len(n_assets)) {
        ns <- if (!is.null(node_stab) && syms[i] %in% names(node_stab)) {
            node_stab[syms[i]]
        } else {
            NULL
        }
        lf <- if (ncol(liq_feats) > 0) list(f_liquidity = liq_feats[i, 1]) else NULL
        rho_rel[i] <- me_reliability_score(X_now[i, ], lf, ns)
    }

    # μ_eff = ρ × μ̂,  s_eff = σ̂ / √ρ
    mu_eff <- rho_rel * mu_hat
    s_eff <- sigma_hat / sqrt(pmax(rho_rel, 0.1))
    names(mu_eff) <- syms
    names(s_eff) <- syms

    # 9. Confidence score (for legacy compat)
    confidence <- rho_rel * (1 - sigma_hat / (abs(mu_hat) + sigma_hat + 1e-8))
    confidence[!is.finite(confidence)] <- 0.5
    confidence <- pmin(pmax(confidence, 0), 1)
    names(confidence) <- syms

    list(
        mu_hat = mu_hat,
        mu_eff = mu_eff,
        sigma_hat = sigma_hat,
        s_eff = s_eff,
        confidence = confidence,
        pi_t = pi_t,
        component_mu = component_mu,
        kappa = kappa,
        q = q,
        rho_rel = rho_rel,
        diag = list(
            method = "architecture_5comp_panel",
            n_training_obs = panel$n_obs,
            n_features = panel$n_features,
            component_fitted = sapply(comp_models, function(m) isTRUE(m$fitted)),
            component_n_features = sapply(comp_models, function(m) m$n_features %||% 0),
            pi_t = pi_t,
            mean_kappa = colMeans(kappa),
            mean_rho_rel = mean(rho_rel),
            refit_this_step = should_refit,
            forecast_step = forecast_step,
            comp_sigma = comp_sigma
        ),
        # Recursive state outputs
        forecast_state_out = list(
            component_model_fits = comp_models,
            component_error_states = error_states_prev, # will be updated when labels mature
            forecast_step = forecast_step
        )
    )
}

# ══════════════════════════════════════════════════════════════════════════════
# Legacy forecast helpers (backward compat, explicitly flagged)
# ══════════════════════════════════════════════════════════════════════════════

#' @export
me_forecast_to_alpha <- function(mu, confidence, sigma, spec_forecast) {
    # LEGACY COMPAT: convert forecast to alpha vector
    # In architecture path, portfolio should use mu_eff directly
    alpha_scale <- spec_forecast$alpha_scale %||% 1.0
    alpha <- mu * confidence * alpha_scale
    alpha[!is.finite(alpha)] <- 0
    alpha
}

#' @export
me_forecast_confidence <- function(X, y_hat, sigma_hat, spec_forecast) {
    # LEGACY COMPAT: simple confidence from prediction uncertainty
    eps <- spec_forecast$confidence_eps %||% 0.01
    u_scale <- spec_forecast$uncertainty_scale %||% 1.0
    conf <- 1 / (1 + u_scale * sigma_hat)
    conf[!is.finite(conf)] <- eps
    conf <- pmin(pmax(conf, eps), 1)
    conf
}
