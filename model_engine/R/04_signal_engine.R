#' @title Model Engine — Signal Engine
#' @description Architecture §7-8: Kalman (recursive per-asset), multiscale TSMOM (raw/residual),
#' factor trends, signal scalarization with recursive weights.

.tanh_scale <- function(x, scale = 1.0) tanh(x / scale)

# ── §7.2 Recursive Kalman filter update (per-asset) ──────────────────────────

#' @export
me_kalman_step <- function(x_hat_prev, P_prev, y_new, Q, R) {
    # Single-step local-level Kalman filter
    # State: x_t = x_{t-1} + w, w ~ N(0, Q)
    # Obs:   y_t = x_t + v,   v ~ N(0, R)
    # Returns: x_hat, P, slope estimate, innovation, innovation_var_ratio

    # Predict
    x_pred <- x_hat_prev
    P_pred <- P_prev + Q

    # Update
    innov <- y_new - x_pred
    S <- P_pred + R # Innovation variance
    K_gain <- P_pred / S # Kalman gain

    x_hat <- x_pred + K_gain * innov
    P_new <- (1 - K_gain) * P_pred

    # Innovation-based diagnostics
    innov_var_ratio <- if (S > 0) innov^2 / S else 0

    list(
        x_hat = x_hat,
        P = P_new,
        slope = innov * K_gain, # Simple slope proxy
        slope_var = P_new, # Uncertainty in state
        innovation = innov,
        innov_var_ratio = innov_var_ratio
    )
}

#' @export
me_signal_kalman <- function(prices_window, sigma_t, spec_kalman,
                             kalman_states = NULL) {
    n_assets <- ncol(prices_window)
    syms <- colnames(prices_window)
    scale <- spec_kalman$scale %||% 1.0
    Q <- spec_kalman$q_var %||% 1e-4
    R <- spec_kalman$r_var %||% 1e-2
    use_recursive <- isTRUE(spec_kalman$recursive %||% TRUE)

    kalman_out <- setNames(rep(0, n_assets), syms)
    kalman_slope <- setNames(rep(0, n_assets), syms)
    kalman_uncertainty <- setNames(rep(0, n_assets), syms)
    kalman_innovation <- setNames(rep(0, n_assets), syms)
    kalman_innov_ratio <- setNames(rep(0, n_assets), syms)
    states_out <- list()

    for (j in seq_len(n_assets)) {
        sym <- syms[j]
        p_series <- prices_window[, j]
        valid <- which(is.finite(p_series) & p_series > 0)
        if (length(valid) < 5) {
            states_out[[sym]] <- list(x_hat = 0, P = R)
            next
        }
        y <- log(p_series[valid])

        if (use_recursive && !is.null(kalman_states) && !is.null(kalman_states[[sym]])) {
            # Recursive: single-step update from previous state
            ks <- kalman_states[[sym]]
            y_last <- y[length(y)]
            step <- me_kalman_step(ks$x_hat, ks$P, y_last, Q, R)

            raw <- y_last - step$x_hat
            vol_j <- sigma_t[sym]
            if (!is.finite(vol_j) || vol_j <= 0) vol_j <- 0.01
            kalman_out[sym] <- .tanh_scale(raw / (vol_j / sqrt(252) + 1e-8), scale)
            kalman_slope[sym] <- step$slope
            kalman_uncertainty[sym] <- step$slope_var
            kalman_innovation[sym] <- step$innovation
            kalman_innov_ratio[sym] <- step$innov_var_ratio
            states_out[[sym]] <- list(x_hat = step$x_hat, P = step$P)
        } else {
            # Batch initialization: run full Kalman from scratch
            x_hat <- y[1]
            P <- R
            for (i in 2:length(y)) {
                step <- me_kalman_step(x_hat, P, y[i], Q, R)
                x_hat <- step$x_hat
                P <- step$P
            }
            raw <- y[length(y)] - x_hat
            vol_j <- sigma_t[sym]
            if (!is.finite(vol_j) || vol_j <= 0) vol_j <- 0.01
            kalman_out[sym] <- .tanh_scale(raw / (vol_j / sqrt(252) + 1e-8), scale)
            kalman_slope[sym] <- step$slope
            kalman_uncertainty[sym] <- step$slope_var
            kalman_innovation[sym] <- step$innovation
            kalman_innov_ratio[sym] <- step$innov_var_ratio
            states_out[[sym]] <- list(x_hat = x_hat, P = P)
        }
    }

    list(
        signal = kalman_out,
        slope = kalman_slope,
        uncertainty = kalman_uncertainty,
        innovation = kalman_innovation,
        innov_var_ratio = kalman_innov_ratio,
        kalman_states_out = states_out
    )
}

# ── §7.1 TSMOM (multiscale, raw + residual) ─────────────────────────────────

.tsmom_single <- function(R_col, sigma, horizon, scale) {
    n <- length(R_col)
    if (n < horizon || horizon < 1) {
        return(0)
    }
    cum <- sum(R_col[(n - horizon + 1):n], na.rm = TRUE)
    denom <- if (is.finite(sigma) && sigma > 0) sigma / sqrt(252) * sqrt(horizon) else 1
    .tanh_scale(cum / (denom + 1e-8), scale)
}

#' Single-horizon TSMOM (legacy compat)
#' @export
me_signal_tsmom <- function(R_window, sigma_t, spec_tsmom) {
    horizon <- min(spec_tsmom$horizon %||% 252L, nrow(R_window))
    horizon <- max(1L, horizon)
    scale <- spec_tsmom$scale %||% 2.0
    n_assets <- ncol(R_window)
    syms <- colnames(R_window)

    out <- setNames(rep(0, n_assets), syms)
    for (j in seq_len(n_assets)) {
        out[j] <- .tsmom_single(R_window[, j], sigma_t[j], horizon, scale)
    }
    out
}

#' §7.1 Multiscale TSMOM: returns n_assets × n_horizons matrix
#' @export
me_signal_tsmom_multiscale <- function(R_window, sigma_t, horizons = c(21, 63, 126, 252),
                                       scale = 2.0) {
    n_assets <- ncol(R_window)
    Tn <- nrow(R_window)
    syms <- colnames(R_window)

    hor <- pmin(horizons, Tn)
    hor <- hor[hor >= 1]
    if (length(hor) == 0) hor <- min(21, Tn)

    result <- matrix(0, n_assets, length(hor),
        dimnames = list(syms, paste0("tsmom_h", hor))
    )

    for (j in seq_len(n_assets)) {
        for (h_idx in seq_along(hor)) {
            result[j, h_idx] <- .tsmom_single(
                R_window[, j], sigma_t[j],
                hor[h_idx], scale
            )
        }
    }
    result
}

#' §7.1 Residual TSMOM multiscale: on r^⊥ = D_t e_t
#' @export
me_signal_tsmom_residual_multiscale <- function(E_window, D_t_daily,
                                                horizons = c(21, 63, 126, 252),
                                                scale = 2.0) {
    # E_window: standardized residuals; D_t_daily: daily vol per asset
    n_assets <- ncol(E_window)
    Tn <- nrow(E_window)
    syms <- colnames(E_window)

    # Unstandardize residuals: r^⊥ = D * e
    R_resid <- E_window
    if (!is.null(D_t_daily)) {
        d_vec <- D_t_daily[syms]
        d_vec[!is.finite(d_vec) | d_vec <= 0] <- 1e-4
        for (j in seq_len(n_assets)) R_resid[, j] <- E_window[, j] * d_vec[j]
    }

    # Vol of residual returns (for normalization)
    sigma_resid <- apply(R_resid, 2, sd, na.rm = TRUE) * sqrt(252)
    sigma_resid[!is.finite(sigma_resid) | sigma_resid <= 0] <- 1e-4

    me_signal_tsmom_multiscale(R_resid, sigma_resid, horizons, scale)
}

# ── §7.3 Factor trend signals ───────────────────────────────────────────────

#' @export
me_signal_factor_trend <- function(F_window, spec_factor = list()) {
    horizons <- spec_factor$horizons %||% c(21L, 63L)
    k <- ncol(F_window)
    Tn <- nrow(F_window)

    factor_sigs <- matrix(0, k, length(horizons),
        dimnames = list(colnames(F_window), paste0("h", horizons))
    )

    for (c_idx in seq_len(k)) {
        f_series <- F_window[, c_idx]
        f_sd <- sd(f_series, na.rm = TRUE)
        if (!is.finite(f_sd) || f_sd <= 0) f_sd <- 1

        for (h_idx in seq_along(horizons)) {
            h <- min(horizons[h_idx], Tn)
            if (h < 1) next
            cum <- sum(f_series[(Tn - h + 1):Tn], na.rm = TRUE)
            factor_sigs[c_idx, h_idx] <- .tanh_scale(cum / (f_sd * sqrt(h) + 1e-8), 2.0)
        }
    }

    factor_sigs
}

# ── §7.4 Signal scalarization with recursive weights ─────────────────────────

#' @export
me_scalarize_signals <- function(signal_list, R_window, sigma_t, omega_prev = NULL,
                                 spec_scalar = list()) {
    # signal_list: named list of named numeric vectors (signal name → per-asset values)
    # Returns: scalarized per-asset signals + updated weights

    n_assets <- ncol(R_window)
    syms <- colnames(R_window)
    lambda_omega <- spec_scalar$lambda_omega %||% 0.95
    ridge_lambda <- spec_scalar$ridge_lambda %||% 0.01

    # Build signal matrix: n_assets × n_signals
    sig_names <- names(signal_list)
    n_sig <- length(sig_names)
    S_mat <- matrix(0, n_assets, n_sig, dimnames = list(syms, sig_names))
    for (s_idx in seq_along(sig_names)) {
        sv <- signal_list[[s_idx]]
        sv_aligned <- sv[syms]
        sv_aligned[!is.finite(sv_aligned)] <- 0
        S_mat[, s_idx] <- sv_aligned
    }

    # Compute cross-sectional ridge regression for scalarization weights
    # Target: next-period XS rank of returns (simple proxy)
    y_target <- R_window[nrow(R_window), ]
    y_target <- y_target[syms]
    y_target[!is.finite(y_target)] <- 0
    y_target <- rank(y_target) / (n_assets + 1) - 0.5 # XS rank centered

    # Ridge fit: omega = (S'S + lambda I)^{-1} S' y
    StS <- t(S_mat) %*% S_mat + ridge_lambda * diag(n_sig)
    Sty <- t(S_mat) %*% y_target
    omega_new <- tryCatch(
        as.vector(solve(StS, Sty)),
        error = function(e) rep(1 / n_sig, n_sig)
    )
    names(omega_new) <- sig_names

    # Recursive smoothing with previous weights
    if (!is.null(omega_prev) && length(omega_prev) == n_sig) {
        omega_prev_aligned <- omega_prev[sig_names]
        if (all(is.finite(omega_prev_aligned))) {
            omega_new <- lambda_omega * omega_prev_aligned + (1 - lambda_omega) * omega_new
        }
    }

    # Compute scalarized signal per asset
    s_scalar <- as.vector(S_mat %*% omega_new)
    names(s_scalar) <- syms

    list(
        s_scalar = s_scalar,
        omega = omega_new,
        S_mat = S_mat
    )
}

# ── Full signal engine orchestrator ──────────────────────────────────────────

#' @export
me_run_signal_engine <- function(prices_window, R_window, risk_artifact,
                                 spec_signals, model_state = NULL) {
    sigma_t <- risk_artifact$sigma_t
    syms <- colnames(R_window)
    n_assets <- length(syms)

    # Extract recursive states
    kalman_states <- if (!is.null(model_state)) model_state$kalman_states else NULL
    scalar_weights <- if (!is.null(model_state)) model_state$scalar_weights else NULL

    # ── 1. Kalman (§7.2) ──
    kal <- me_signal_kalman(prices_window, sigma_t, spec_signals$kalman,
        kalman_states = kalman_states
    )
    s_kal <- kal$signal

    # ── 2. TSMOM single-horizon (legacy compat) ──
    s_mom <- me_signal_tsmom(R_window, sigma_t, spec_signals$tsmom)

    # ── 3. Multiscale TSMOM raw (§7.1) ──
    tsmom_horizons <- spec_signals$tsmom$horizons %||% c(21, 63, 126, 252)
    tsmom_multi <- me_signal_tsmom_multiscale(
        R_window, sigma_t,
        horizons = tsmom_horizons,
        scale = spec_signals$tsmom$scale %||% 2.0
    )

    # ── 4. Residual TSMOM multiscale (§7.1) ──
    D_t_daily <- NULL
    ewma_state <- if (!is.null(model_state)) model_state$ewma_vol_state else NULL
    if (!is.null(ewma_state)) {
        D_t_daily <- sqrt(me_pi_map_vector(ewma_state, syms, init_val = 1e-4))
    }
    resid_tsmom_multi <- NULL
    if (!is.null(risk_artifact$E_t) && ncol(risk_artifact$E_t) >= 3) {
        resid_tsmom_multi <- me_signal_tsmom_residual_multiscale(
            risk_artifact$E_t, D_t_daily,
            horizons = tsmom_horizons,
            scale = spec_signals$tsmom$scale %||% 2.0
        )
    }

    # ── 5. Factor trend (§7.3) ──
    s_fac <- rep(0, n_assets)
    names(s_fac) <- syms
    factor_trends <- NULL
    if (!is.null(risk_artifact$F_t) && !is.null(risk_artifact$B_t)) {
        spec_factor <- spec_signals$factor %||% list()
        factor_trends <- me_signal_factor_trend(risk_artifact$F_t, spec_factor)
        # Project factor trends back to asset space: B × latest factor trend vector
        f_trend_latest <- factor_trends[, 1, drop = TRUE] # shortest horizon
        f_proj <- as.vector(risk_artifact$B_t %*% f_trend_latest)
        names(f_proj) <- rownames(risk_artifact$B_t)
        s_fac <- f_proj[syms]
        s_fac[!is.finite(s_fac)] <- 0
    }

    # ── 6. Signal scalarization (§7.4) ──
    signal_list <- list(mom = s_mom, kal = s_kal, fac = s_fac)
    spec_scalar <- spec_signals$scalarization %||% list()
    scalar_result <- me_scalarize_signals(
        signal_list, R_window, sigma_t,
        omega_prev = if (!is.null(scalar_weights)) scalar_weights$omega_combined else NULL,
        spec_scalar = spec_scalar
    )

    list(
        s_mom = s_mom,
        s_kal = s_kal,
        s_fac = s_fac,
        s_scalar = scalar_result$s_scalar,

        # Multiscale features (for feature engine)
        tsmom_multi = tsmom_multi,
        resid_tsmom_multi = resid_tsmom_multi,
        factor_trends = factor_trends,

        # Richer Kalman outputs
        kalman_slope = kal$slope,
        kalman_uncertainty = kal$uncertainty,
        kalman_innovation = kal$innovation,
        kalman_innov_ratio = kal$innov_var_ratio,

        # Recursive state outputs
        signal_state_out = list(
            kalman_states = kal$kalman_states_out,
            scalar_weights = list(omega_combined = scalar_result$omega)
        )
    )
}
