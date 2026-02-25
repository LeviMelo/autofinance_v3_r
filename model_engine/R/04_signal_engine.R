#' @title Model Engine — Signal Engine
#' @description Trend experts, factor trends, signal scalarization.
#' Implements architecture.md §§7-8: Kalman, TSMOM (raw/residual), factor trends.

.tanh_scale <- function(x, scale = 1.0) tanh(x / scale)

# ── Kalman filter signal (§7) ─────────────────────────────────────────────────

#' @export
me_signal_kalman <- function(prices_window, sigma_t, spec_kalman) {
    n_assets <- ncol(prices_window)
    Tn <- nrow(prices_window)
    scores <- rep(0, n_assets)
    names(scores) <- colnames(prices_window)
    if (is.null(n_assets) || n_assets == 0 || Tn < 10) {
        return(scores)
    }

    q_var <- spec_kalman$q_var %||% 1e-4
    r_var <- spec_kalman$r_var %||% 1e-2
    out_scale <- spec_kalman$scale %||% 1.0

    F_mat <- matrix(c(1, 0, 1, 1), nrow = 2, byrow = FALSE)
    H_mat <- matrix(c(1, 0), nrow = 1)
    Q <- diag(c(q_var, q_var))
    I2 <- diag(2)

    for (j in seq_len(n_assets)) {
        y_raw <- prices_window[, j]
        valid <- is.finite(y_raw) & !is.na(y_raw) & (y_raw > 0)
        if (sum(valid) < 10) next
        y <- log(y_raw[valid])

        x_hat <- matrix(c(y[1], 0), nrow = 2)
        P <- diag(c(1, 1))

        for (i in 2:length(y)) {
            x_pred <- F_mat %*% x_hat
            P_pred <- F_mat %*% P %*% t(F_mat) + Q
            y_hat <- (H_mat %*% x_pred)[1, 1]
            err <- y[i] - y_hat
            S <- (H_mat %*% P_pred %*% t(H_mat))[1, 1] + r_var
            if (!is.finite(S) || S <= 0) next
            K <- P_pred %*% t(H_mat) / S
            x_hat <- x_pred + K * err
            KH <- K %*% H_mat
            P <- (I2 - KH) %*% P_pred %*% t(I2 - KH) + K %*% matrix(r_var, 1, 1) %*% t(K)
        }

        slope_daily <- x_hat[2, 1]
        if (!is.finite(slope_daily)) next
        slope_ann <- slope_daily * 252
        vol_j <- sigma_t[colnames(prices_window)[j]]
        if (length(vol_j) != 1 || !is.finite(vol_j) || vol_j <= 0) {
            vol_j <- max(median(sigma_t[is.finite(sigma_t) & sigma_t > 0], na.rm = TRUE), 0.2)
        }
        scores[j] <- slope_ann / vol_j
    }
    .tanh_scale(scores, out_scale)
}

# ── TSMOM signal (§7) ────────────────────────────────────────────────────────

#' @export
me_signal_tsmom <- function(R_window, sigma_t, spec_tsmom) {
    if (is.null(dim(R_window)) || ncol(R_window) == 0) {
        return(setNames(numeric(0), character(0)))
    }

    h <- min(spec_tsmom$horizon %||% 252L, nrow(R_window))
    out_scale <- spec_tsmom$scale %||% 2.0
    if (h < 5) {
        return(setNames(rep(0, ncol(R_window)), colnames(R_window)))
    }

    syms <- colnames(R_window)
    sigma_aligned <- sigma_t[syms]
    sigma_fb <- max(median(sigma_t[is.finite(sigma_t) & sigma_t > 0], na.rm = TRUE), 0.2)
    bad <- !is.finite(sigma_aligned) | sigma_aligned <= 0
    if (any(bad)) sigma_aligned[bad] <- sigma_fb

    ret_cum <- colSums(tail(R_window, h), na.rm = TRUE)
    denom <- sigma_aligned * sqrt(h / 252)
    denom[!is.finite(denom) | denom <= 0] <- sigma_fb * sqrt(h / 252)
    scores <- ret_cum / denom
    scores[!is.finite(scores)] <- 0
    .tanh_scale(scores, out_scale)
}

#' Residual TSMOM on PCA residuals
#' @export
me_signal_residual_tsmom <- function(E_window, sigma_t, spec_tsmom) {
    me_signal_tsmom(E_window, sigma_t, spec_tsmom)
}

# ── Factor trend signals (§7, §11.3) ─────────────────────────────────────────

#' @export
me_signal_factor_trends <- function(F_window, spec_factor = list()) {
    # g_bar: per-factor cumulative return (trend summary)
    if (is.null(F_window) || nrow(F_window) < 5) {
        return(NULL)
    }
    k <- ncol(F_window)
    h <- min(spec_factor$horizon %||% 63L, nrow(F_window))

    g_bar <- colSums(tail(F_window, h), na.rm = TRUE)
    # Normalize by factor vol
    fvol <- apply(F_window, 2, sd, na.rm = TRUE) * sqrt(252)
    fvol[fvol <= 0 | !is.finite(fvol)] <- 1
    g_bar <- g_bar / fvol
    names(g_bar) <- colnames(F_window)
    g_bar
}

#' Factor-projected continuation: phi_facproj = B_i^T * g_bar
#' @export
me_signal_factor_projection <- function(B_t, g_bar) {
    if (is.null(B_t) || is.null(g_bar)) {
        return(NULL)
    }
    k <- min(ncol(B_t), length(g_bar))
    proj <- B_t[, 1:k, drop = FALSE] %*% g_bar[1:k]
    out <- as.vector(proj)
    names(out) <- rownames(B_t)
    .tanh_scale(out, 2.0)
}

# ── Signal alignment ──────────────────────────────────────────────────────────

#' @export
me_align_signal_vectors <- function(...) {
    lst <- list(...)
    syms <- unique(unlist(lapply(lst, names)))
    if (length(syms) == 0) {
        return(list())
    }
    aligned <- lapply(lst, function(v) {
        res <- setNames(rep(0, length(syms)), syms)
        idx <- intersect(names(v), syms)
        res[idx] <- v[idx]
        res
    })
    aligned
}

# ── Signal scalarization (§8) ─────────────────────────────────────────────────

#' @export
me_scalarize_signals <- function(signal_artifact, weights = NULL) {
    # Combine multiple signal channels into per-family scalars
    # s_mom = weighted combo of tsmom + res_tsmom
    # s_kal = kalman
    # s_fac = factor_projection

    kal <- signal_artifact$kalman
    tsmom <- signal_artifact$tsmom
    res_tsmom <- signal_artifact$res_tsmom
    fac_proj <- signal_artifact$factor_projection

    s_kal <- kal
    s_mom <- tsmom
    if (!is.null(res_tsmom)) {
        common <- intersect(names(s_mom), names(res_tsmom))
        s_mom[common] <- 0.6 * s_mom[common] + 0.4 * res_tsmom[common]
    }
    s_fac <- fac_proj

    list(s_mom = s_mom, s_kal = s_kal, s_fac = s_fac)
}

# ── Full signal engine orchestrator ───────────────────────────────────────────

#' @export
me_run_signal_engine <- function(prices_window, R_window, sigma_t,
                                 spec_signals, E_window = NULL,
                                 B_t = NULL, F_window = NULL) {
    if (is.null(dim(prices_window)) || is.null(dim(R_window))) {
        stop("prices_window and R_window must be matrices")
    }

    common <- Reduce(intersect, list(
        colnames(prices_window),
        colnames(R_window), names(sigma_t)
    ))
    if (length(common) == 0) stop("Signal engine: zero common symbols")

    P_use <- prices_window[, common, drop = FALSE]
    R_use <- R_window[, common, drop = FALSE]
    sigma_use <- sigma_t[common]

    kalman_scores <- me_signal_kalman(P_use, sigma_use, spec_signals$kalman)
    tsmom_scores <- me_signal_tsmom(R_use, sigma_use, spec_signals$tsmom)

    res_tsmom_scores <- NULL
    if (!is.null(E_window) && ncol(E_window) > 0) {
        e_common <- intersect(common, colnames(E_window))
        if (length(e_common) > 0) {
            res_tsmom_scores <- me_signal_residual_tsmom(
                E_window[, e_common, drop = FALSE], sigma_use[e_common],
                spec_signals$tsmom
            )
        }
    }

    factor_projection <- NULL
    g_bar <- NULL
    if (!is.null(B_t) && !is.null(F_window)) {
        g_bar <- me_signal_factor_trends(F_window, spec_signals$factor %||% list())
        if (!is.null(g_bar)) {
            factor_projection <- me_signal_factor_projection(B_t, g_bar)
        }
    }

    sig_list <- list(kalman = kalman_scores, tsmom = tsmom_scores)
    if (!is.null(res_tsmom_scores)) sig_list$res_tsmom <- res_tsmom_scores
    if (!is.null(factor_projection)) sig_list$factor_projection <- factor_projection
    aligned <- do.call(me_align_signal_vectors, sig_list)

    result <- list(
        kalman = aligned$kalman, tsmom = aligned$tsmom,
        res_tsmom = aligned$res_tsmom,
        factor_projection = aligned$factor_projection,
        g_bar = g_bar,
        diag = list(
            kalman_coverage = sum(aligned$kalman != 0),
            tsmom_coverage = sum(aligned$tsmom != 0),
            n_common = length(common),
            kalman_all_zero = all(aligned$kalman == 0),
            tsmom_all_zero = all(aligned$tsmom == 0)
        )
    )
    result
}
