#' @title Model Engine — Risk Engine
#' @description Architecture §5: EWMA vol, standardized returns, PCA with factor alignment,
#' recursive factor/residual covariance, Glasso precision, covariance recomposition.
#' Preserves legacy HRP baseline as transitional alias.

# ── §5.1 EWMA volatility update (per-asset recursive) ──────────────────────

#' @export
me_ewma_vol_update <- function(sigma2_prev, r_t, lambda_sigma = 0.94) {
    # sigma2_i,t = lambda * sigma2_i,t-1 + (1-lambda) * r_i,t^2
    # sigma2_prev: named numeric vector of previous variances (NULL for cold start)
    # r_t: named numeric vector of current returns
    syms <- names(r_t)
    r2 <- r_t^2
    r2[!is.finite(r2)] <- 0

    if (is.null(sigma2_prev) || length(sigma2_prev) == 0) {
        # Cold start: use current observed variance as initial
        out <- r2
        out[out <= 0] <- median(r2[r2 > 0], na.rm = TRUE)
        out[!is.finite(out)] <- 1e-4
        names(out) <- syms
        return(out)
    }

    # Align previous state to current universe via Π_t
    sigma2_aligned <- me_pi_map_vector(sigma2_prev, syms, init_val = median(sigma2_prev[sigma2_prev > 0], na.rm = TRUE))
    sigma2_aligned[!is.finite(sigma2_aligned) | sigma2_aligned <= 0] <- 1e-4

    out <- lambda_sigma * sigma2_aligned + (1 - lambda_sigma) * r2
    out[!is.finite(out) | out <= 0] <- 1e-4
    names(out) <- syms
    out
}

#' Batch EWMA volatility from a return window (for initialization)
#' @export
me_estimate_vol <- function(R_window, spec_risk_vol) {
    if (is.null(R_window) || nrow(R_window) == 0 || ncol(R_window) == 0) {
        return(numeric(0))
    }

    method <- spec_risk_vol$method %||% "ewma"
    lambda <- spec_risk_vol$lambda_sigma %||% 0.94

    if (method == "ewma") {
        # Full batch EWMA from window
        n <- ncol(R_window)
        Tn <- nrow(R_window)
        sigma2 <- rep(NA_real_, n)
        names(sigma2) <- colnames(R_window)
        for (j in seq_len(n)) {
            r <- R_window[, j]
            r[!is.finite(r)] <- 0
            # Initialize from first 21 days sample variance
            init_n <- min(21, length(r))
            s2 <- var(r[1:init_n])
            if (!is.finite(s2) || s2 <= 0) s2 <- 1e-4
            for (i in seq_along(r)) {
                s2 <- lambda * s2 + (1 - lambda) * r[i]^2
            }
            sigma2[j] <- s2
        }
        vols <- sqrt(sigma2) * sqrt(252)
    } else {
        # Legacy: simple sample sd
        vols <- apply(R_window, 2, sd, na.rm = TRUE) * sqrt(252)
    }

    vols[!is.finite(vols) | vols <= 0] <- NA_real_
    vols
}

# ── §5.2 PCA on standardized returns ─────────────────────────────────────────

#' @export
me_fit_pca <- function(R_window, spec_pca) {
    k <- spec_pca$k %||% 5L
    n <- ncol(R_window)
    Tn <- nrow(R_window)

    if (n < 3 || Tn < 10) stop("PCA requires >= 3 assets and >= 10 observations")
    k <- min(k, n - 1, Tn - 1)

    centers <- colMeans(R_window, na.rm = TRUE)
    R_centered <- scale(R_window, center = centers, scale = FALSE)
    R_centered[!is.finite(R_centered)] <- 0

    sv <- tryCatch(svd(R_centered, nu = k, nv = k), error = function(e) NULL)
    if (is.null(sv)) stop("SVD failed in PCA")

    B <- sv$v[, 1:k, drop = FALSE]
    rownames(B) <- colnames(R_window)
    colnames(B) <- paste0("PC", seq_len(k))

    F_scores <- R_centered %*% B
    colnames(F_scores) <- paste0("PC", seq_len(k))

    list(
        B = B,
        F = F_scores,
        centers = centers,
        k = k,
        d = sv$d[1:k],
        n_obs = Tn,
        n_assets = n
    )
}

# ── §5.3 PCA factor identity alignment (signed permutation) ─────────────────

#' @export
me_align_pca_factors <- function(B_raw, f_raw, B_prev) {
    # Align current PCA factors to previous basis for temporal consistency.
    # Uses Hungarian matching on |C| = |B_prev' B_raw| + sign correction.
    # Returns aligned (B_t, f_t, R_align, alignment_used).
    k <- ncol(B_raw)
    if (is.null(B_prev) || ncol(B_prev) != k) {
        return(list(B = B_raw, f = f_raw, R_align = diag(k), alignment_used = FALSE))
    }

    # Overlap matrix: C = B_prev' B_raw (k x k)
    # Use common assets
    common <- intersect(rownames(B_prev), rownames(B_raw))
    if (length(common) < max(3, k)) {
        return(list(B = B_raw, f = f_raw, R_align = diag(k), alignment_used = FALSE))
    }

    C <- t(B_prev[common, , drop = FALSE]) %*% B_raw[common, , drop = FALSE]
    absC <- abs(C)

    # Greedy Hungarian-style matching on |C|
    # Find best permutation mapping: new_col -> prev_col
    perm <- rep(NA_integer_, k)
    signs <- rep(1, k)
    used_rows <- rep(FALSE, k)

    for (iter in seq_len(k)) {
        best_val <- -1
        best_i <- 1
        best_j <- 1
        for (i in seq_len(k)) {
            if (used_rows[i]) next
            for (j in seq_len(k)) {
                if (!is.na(perm[j])) next
                if (absC[i, j] > best_val) {
                    best_val <- absC[i, j]
                    best_i <- i
                    best_j <- j
                }
            }
        }
        perm[best_j] <- best_i
        signs[best_j] <- sign(C[best_i, best_j])
        if (signs[best_j] == 0) signs[best_j] <- 1
        used_rows[best_i] <- TRUE
    }

    # Build signed permutation matrix R_align
    R_align <- matrix(0, k, k)
    for (j in seq_len(k)) {
        R_align[perm[j], j] <- signs[j]
    }

    B_aligned <- B_raw %*% R_align
    rownames(B_aligned) <- rownames(B_raw)
    colnames(B_aligned) <- colnames(B_raw)

    # f_raw is T×k matrix or single vector
    if (is.matrix(f_raw)) {
        f_aligned <- f_raw %*% R_align
        colnames(f_aligned) <- colnames(B_raw)
    } else {
        f_aligned <- as.vector(R_align %*% f_raw)
        names(f_aligned) <- colnames(B_raw)
    }

    list(B = B_aligned, f = f_aligned, R_align = R_align, alignment_used = TRUE)
}

# ── Residuals ─────────────────────────────────────────────────────────────────

#' @export
me_compute_residuals <- function(R_window, pca_fit) {
    R_centered <- scale(R_window, center = pca_fit$centers, scale = FALSE)
    R_centered[!is.finite(R_centered)] <- 0
    Sys <- pca_fit$F %*% t(pca_fit$B)
    E <- R_centered - Sys
    dimnames(E) <- dimnames(R_window)
    E
}

# ── §5.4 Factor covariance recursion ─────────────────────────────────────────

#' Recursive EWMA factor covariance update
#' @export
me_factor_cov_recursive <- function(Sigma_f_prev, f_t, lambda_f = 0.97) {
    # Sigma_f,t = lambda_f * Sigma_f,t-1 + (1 - lambda_f) * f_t f_t'
    k <- length(f_t)
    f_t <- as.numeric(f_t)
    f_t[!is.finite(f_t)] <- 0

    ff <- f_t %*% t(f_t)

    if (is.null(Sigma_f_prev) || !is.matrix(Sigma_f_prev) ||
        nrow(Sigma_f_prev) != k || ncol(Sigma_f_prev) != k) {
        # Cold start
        return(list(sigma_f = ff))
    }

    sigma_f <- lambda_f * Sigma_f_prev + (1 - lambda_f) * ff
    sigma_f <- (sigma_f + t(sigma_f)) / 2
    sigma_f[!is.finite(sigma_f)] <- 0
    list(sigma_f = sigma_f)
}

#' Batch factor covariance (legacy compat)
#' @export
me_factor_cov <- function(F_window, spec_factor_cov = list()) {
    sigma_f <- cov(F_window, use = "pairwise.complete.obs")
    sigma_f[!is.finite(sigma_f)] <- 0
    list(sigma_f = sigma_f)
}

# ── §5.5 Recursive residual covariance target ────────────────────────────────

#' @export
me_resid_cov_recursive <- function(S_e_prev, e_t, new_univ, lambda_e = 0.97) {
    # S_e,t = lambda_e * S_e,t-1 + (1-lambda_e) * e_t e_t'
    p <- length(e_t)
    e_t <- as.numeric(e_t)
    e_t[!is.finite(e_t)] <- 0
    ee <- e_t %*% t(e_t)
    dimnames(ee) <- list(new_univ, new_univ)

    if (is.null(S_e_prev) || !is.matrix(S_e_prev)) {
        return(ee)
    }

    # Map previous state to current universe
    S_mapped <- me_pi_map_matrix(S_e_prev, new_univ, init_diag = median(diag(S_e_prev), na.rm = TRUE))

    S_new <- lambda_e * S_mapped + (1 - lambda_e) * ee
    S_new <- (S_new + t(S_new)) / 2
    S_new[!is.finite(S_new)] <- 0
    S_new
}

# ── Residual precision (Glasso) ──────────────────────────────────────────────

#' @export
me_fit_residual_cov <- function(E_window, spec_resid, S_e_target = NULL) {
    use_glasso <- isTRUE(spec_resid$use_glasso)
    lambda <- spec_resid$lambda %||% 0.1

    # Use recursive target S_e if provided; otherwise batch sample cov
    if (!is.null(S_e_target) && is.matrix(S_e_target)) {
        S_eps <- S_e_target
    } else {
        S_eps <- cov(E_window, use = "pairwise.complete.obs")
    }
    S_eps[!is.finite(S_eps)] <- 0
    n <- ncol(S_eps)

    precision <- NULL
    Sigma_eps <- S_eps

    glasso_requested <- use_glasso
    glasso_used <- FALSE
    precision_method <- "ridge_inverse"
    precision_fallback <- FALSE
    fallback_reason <- NULL

    if (use_glasso && n >= 3) {
        if (requireNamespace("glasso", quietly = TRUE)) {
            gl <- tryCatch(
                glasso::glasso(S_eps, rho = lambda, penalize.diagonal = FALSE),
                error = function(e) e
            )
            if (!inherits(gl, "error") && !is.null(gl)) {
                Sigma_eps <- gl$w
                precision <- gl$wi
                glasso_used <- TRUE
                precision_method <- "glasso"
            } else {
                precision_fallback <- TRUE
                fallback_reason <- if (inherits(gl, "error")) {
                    paste("glasso_failed:", gl$message)
                } else {
                    "glasso_failed_unknown"
                }
            }
        } else {
            precision_fallback <- TRUE
            fallback_reason <- "glasso_package_not_installed"
        }
    } else if (use_glasso && n < 3) {
        precision_fallback <- TRUE
        fallback_reason <- "glasso_skipped_n_lt_3"
    }

    if (is.null(precision)) {
        # Ridge fallback for precision approximation
        ridge <- lambda * mean(diag(Sigma_eps), na.rm = TRUE)
        if (!is.finite(ridge) || ridge < 0) ridge <- 1e-6
        Sigma_eps_r <- Sigma_eps
        diag(Sigma_eps_r) <- diag(Sigma_eps_r) + ridge

        precision <- tryCatch(
            solve(Sigma_eps_r),
            error = function(e) {
                d <- diag(Sigma_eps_r)
                d[!is.finite(d) | d <= 0] <- 1
                diag(1 / d)
            }
        )

        if (precision_method != "glasso") {
            precision_method <- "ridge_inverse"
        }
        if (is.null(fallback_reason) && use_glasso) {
            fallback_reason <- "glasso_not_used_ridge_inverse_applied"
        }
    }

    dimnames(Sigma_eps) <- dimnames(S_eps)
    dimnames(precision) <- dimnames(S_eps)

    list(
        sigma_eps = Sigma_eps,
        precision = precision,
        glasso_requested = glasso_requested,
        glasso_used = glasso_used,
        precision_method = precision_method,
        precision_fallback = isTRUE(precision_fallback) || (!glasso_used && glasso_requested),
        fallback_reason = fallback_reason
    )
}

# ── §5.6 Total covariance recomposition + unstandardization ──────────────────

#' @export
me_assemble_total_cov <- function(pca_fit, Sigma_f, Sigma_eps, D_t = NULL) {
    B <- pca_fit$B
    # Standardized daily covariance: Σ̃ = B Σ_f B' + Σ_e
    Sigma_std <- B %*% Sigma_f %*% t(B) + Sigma_eps
    Sigma_std <- (Sigma_std + t(Sigma_std)) / 2

    # Unstandardize: Σ_risk^(1) = D Σ̃ D
    if (!is.null(D_t) && length(D_t) > 0) {
        # D_t = diag(σ_1,...,σ_p) — unstandardization
        d_vec <- D_t[rownames(B)]
        d_vec[!is.finite(d_vec) | d_vec <= 0] <- 1e-4
        Sigma_total <- diag(d_vec) %*% Sigma_std %*% diag(d_vec)
    } else {
        Sigma_total <- Sigma_std
    }

    Sigma_total <- (Sigma_total + t(Sigma_total)) / 2
    dimnames(Sigma_total) <- list(rownames(B), rownames(B))
    Sigma_total
}

# ── Covariance sanity (nearPD repair) ─────────────────────────────────────────

#' @export
me_cov_sanity <- function(Sigma, repair = TRUE) {
    n <- ncol(Sigma)
    was_repaired <- FALSE
    evals <- eigen(Sigma, symmetric = TRUE, only.values = TRUE)$values

    if (any(evals < -1e-10)) {
        if (repair) {
            me_require("Matrix")
            Sigma <- as.matrix(Matrix::nearPD(Sigma, corr = FALSE)$mat)
            was_repaired <- TRUE
        } else {
            stop("Covariance matrix is not PSD")
        }
    }

    list(
        Sigma = Sigma, was_repaired = was_repaired,
        min_eigenvalue = min(evals), n = n
    )
}

# ── HRP allocation (transitional baseline) ───────────────────────────────────

.inv_var_alloc <- function(cov_mat) {
    v <- diag(cov_mat)
    v[v <= 0 | !is.finite(v)] <- max(v[v > 0 & is.finite(v)], 1e-8)
    w <- 1 / v
    w / sum(w)
}

.get_cluster_var <- function(cov_mat, c_ix) {
    cov_slice <- cov_mat[c_ix, c_ix, drop = FALSE]
    w <- .inv_var_alloc(cov_slice)
    v <- sum(w * (cov_slice %*% w))
    if (v <= 0) v <- 1e-8
    v
}

.get_rec_bipart <- function(cov_mat, sort_ix) {
    w <- rep(1, length(sort_ix))
    names(w) <- colnames(cov_mat)[sort_ix]

    clusters <- list(sort_ix)
    while (length(clusters) > 0) {
        c_ix <- clusters[[1]]
        clusters <- clusters[-1]
        if (length(c_ix) > 1) {
            half <- floor(length(c_ix) / 2)
            c1 <- c_ix[1:half]
            c2 <- c_ix[(half + 1):length(c_ix)]
            v1 <- .get_cluster_var(cov_mat, c1)
            v2 <- .get_cluster_var(cov_mat, c2)
            alpha <- 1 - v1 / (v1 + v2)
            w[colnames(cov_mat)[c1]] <- w[colnames(cov_mat)[c1]] * alpha
            w[colnames(cov_mat)[c2]] <- w[colnames(cov_mat)[c2]] * (1 - alpha)
            clusters <- append(clusters, list(c1, c2))
        }
    }
    w
}

#' @export
me_allocate_hrp <- function(Sigma, spec_hrp = list()) {
    n <- ncol(Sigma)

    if (!is.matrix(Sigma)) {
        Sigma <- as.matrix(Sigma)
    }
    if (length(dim(Sigma)) != 2L || nrow(Sigma) != ncol(Sigma)) {
        stop(sprintf(
            "me_allocate_hrp: Sigma must be square, got %s x %s",
            NROW(Sigma), NCOL(Sigma)
        ))
    }
    if (is.null(colnames(Sigma)) || is.null(rownames(Sigma))) {
        stop("me_allocate_hrp: Sigma must have rownames and colnames.")
    }
    if (!identical(rownames(Sigma), colnames(Sigma))) {
        stop("me_allocate_hrp: Sigma rownames/colnames must match and be in same order.")
    }

    if (is.null(n) || n == 0) {
        w <- numeric(0)
        attr(w, "allocator_method") <- "empty"
        attr(w, "allocator_fallback") <- FALSE
        return(w)
    }

    if (n == 1) {
        w <- 1
        names(w) <- colnames(Sigma)
        attr(w, "allocator_method") <- "single_asset"
        attr(w, "allocator_fallback") <- FALSE
        return(w)
    }

    cor_mat <- tryCatch(cov2cor(Sigma), error = function(e) e)
    if (inherits(cor_mat, "error") || is.null(cor_mat)) {
        w <- .inv_var_alloc(Sigma)
        names(w) <- colnames(Sigma)
        attr(w, "allocator_method") <- "inv_var_fallback"
        attr(w, "allocator_fallback") <- TRUE
        attr(w, "allocator_reason") <- if (inherits(cor_mat, "error")) {
            paste("cov2cor_failed:", cor_mat$message)
        } else {
            "cov2cor_failed_unknown"
        }
        return(w)
    }

    if (!is.matrix(cor_mat) || nrow(cor_mat) != ncol(cor_mat)) {
        w <- .inv_var_alloc(Sigma)
        names(w) <- colnames(Sigma)
        attr(w, "allocator_method") <- "inv_var_fallback"
        attr(w, "allocator_fallback") <- TRUE
        attr(w, "allocator_reason") <- sprintf(
            "cor_mat_not_square:%sx%s", NROW(cor_mat), NCOL(cor_mat)
        )
        return(w)
    }

    if (is.null(rownames(cor_mat)) || is.null(colnames(cor_mat)) ||
        !identical(rownames(cor_mat), colnames(cor_mat))) {
        dimnames(cor_mat) <- dimnames(Sigma)
    }

    cor_mat[!is.finite(cor_mat)] <- 0
    diag(cor_mat) <- 1
    cor_mat <- (cor_mat + t(cor_mat)) / 2

    dist_mat <- (1 - cor_mat) / 2
    dist_mat[!is.finite(dist_mat)] <- 0
    dist_mat[dist_mat < 0] <- 0
    dist_mat <- sqrt(dist_mat)

    dist_mat <- as.matrix(dist_mat)
    if (!is.matrix(dist_mat) || nrow(dist_mat) != ncol(dist_mat)) {
        w <- .inv_var_alloc(Sigma)
        names(w) <- colnames(Sigma)
        attr(w, "allocator_method") <- "inv_var_fallback"
        attr(w, "allocator_fallback") <- TRUE
        attr(w, "allocator_reason") <- sprintf(
            "dist_mat_not_square:%sx%s", NROW(dist_mat), NCOL(dist_mat)
        )
        return(w)
    }
    dimnames(dist_mat) <- dimnames(Sigma)
    diag(dist_mat) <- 0

    dist_obj <- tryCatch(as.dist(dist_mat), error = function(e) e)
    if (inherits(dist_obj, "error")) {
        w <- .inv_var_alloc(Sigma)
        names(w) <- colnames(Sigma)
        attr(w, "allocator_method") <- "inv_var_fallback"
        attr(w, "allocator_fallback") <- TRUE
        attr(w, "allocator_reason") <- paste("as.dist_failed:", dist_obj$message)
        return(w)
    }

    hc <- tryCatch(hclust(dist_obj, method = "ward.D2"),
        error = function(e) e
    )

    if (inherits(hc, "error")) {
        w <- .inv_var_alloc(Sigma)
        names(w) <- colnames(Sigma)
        attr(w, "allocator_method") <- "inv_var_fallback"
        attr(w, "allocator_fallback") <- TRUE
        attr(w, "allocator_reason") <- paste("hclust_failed:", hc$message)
        return(w)
    }

    sort_ix <- hc$order
    w_hrp <- .get_rec_bipart(Sigma, sort_ix)
    out <- w_hrp[colnames(Sigma)]
    attr(out, "allocator_method") <- "hrp"
    attr(out, "allocator_fallback") <- FALSE
    out
}

# ── Full risk engine orchestrator (architecture-aligned) ─────────────────────

#' @export
me_run_risk_engine <- function(R_window, spec_risk, model_state = NULL) {
    if (is.null(R_window) || nrow(R_window) == 0 || ncol(R_window) == 0) {
        stop("Risk engine received empty R_window.")
    }

    n_obs <- nrow(R_window)
    n_input <- ncol(R_window)

    # Drop assets with too many NAs
    na_frac <- colMeans(is.na(R_window))
    keep <- na_frac < 0.5
    dropped <- colnames(R_window)[!keep]
    R_clean <- R_window[, keep, drop = FALSE]
    R_clean[is.na(R_clean)] <- 0

    n_kept <- ncol(R_clean)
    if (n_kept < 3) stop("Risk engine: fewer than 3 assets after NA filtering.")

    syms <- colnames(R_clean)
    r_t <- R_clean[nrow(R_clean), ] # Current day return vector

    # Extract recursive states from model_state
    sigma2_prev <- if (!is.null(model_state)) model_state$ewma_vol_state else NULL
    Sigma_f_prev <- if (!is.null(model_state)) model_state$factor_cov_state else NULL
    S_e_prev <- if (!is.null(model_state)) model_state$resid_cov_state else NULL
    B_prev <- if (!is.null(model_state)) model_state$B_prev else NULL

    lambda_sigma <- spec_risk$vol$lambda_sigma %||% 0.94
    lambda_f <- spec_risk$factor$lambda_f %||% 0.97
    lambda_e <- spec_risk$resid$lambda_e %||% 0.97
    align_factors <- isTRUE(spec_risk$pca$align_factors %||% TRUE)

    # ── 1. EWMA volatility (§5.1) ──
    use_ewma <- identical(spec_risk$vol$method %||% "ewma", "ewma")
    standardized_path <- use_ewma # architecture path flag

    if (use_ewma) {
        # Recursive per-asset EWMA volatility
        sigma2_t <- me_ewma_vol_update(sigma2_prev, r_t, lambda_sigma)
        sigma_t <- sqrt(sigma2_t) * sqrt(252) # annualized vol
        sigma_t[!is.finite(sigma_t) | sigma_t <= 0] <- NA_real_

        # Standardized returns: r̃ = D^{-1} r
        D_t_daily <- sqrt(sigma2_t) # daily scale
        D_t_daily[!is.finite(D_t_daily) | D_t_daily <= 0] <- 1e-4

        # Build standardized return window for PCA
        R_std <- R_clean
        for (j in seq_len(n_kept)) {
            R_std[, j] <- R_clean[, j] / D_t_daily[j]
        }
        R_std[!is.finite(R_std)] <- 0
    } else {
        # Legacy: batch vol estimation
        sigma_t <- me_estimate_vol(R_clean, spec_risk$vol)
        sigma2_t <- (sigma_t / sqrt(252))^2
        sigma2_t[!is.finite(sigma2_t)] <- 1e-4
        R_std <- R_clean
        D_t_daily <- NULL
    }

    # ── 2. PCA on standardized returns (§5.2) ──
    pca_fit <- me_fit_pca(R_std, spec_risk$pca)

    # ── 3. Factor identity alignment (§5.3) ──
    factor_alignment_used <- FALSE
    if (align_factors && !is.null(B_prev)) {
        align_result <- me_align_pca_factors(pca_fit$B, pca_fit$F, B_prev)
        pca_fit$B <- align_result$B
        pca_fit$F <- align_result$f
        factor_alignment_used <- align_result$alignment_used
    }

    # Current-day factor return (last row of factor scores)
    f_t <- pca_fit$F[nrow(pca_fit$F), ]

    # ── 4. Residuals on standardized returns (§5.5) ──
    E <- me_compute_residuals(R_std, pca_fit)
    e_t <- E[nrow(E), ] # Current-day standardized residual

    # ── 5. Recursive factor covariance (§5.4) ──
    recursive_factor_cov_used <- FALSE
    if (!is.null(Sigma_f_prev) && is.matrix(Sigma_f_prev)) {
        fac_cov <- me_factor_cov_recursive(Sigma_f_prev, f_t, lambda_f)
        recursive_factor_cov_used <- TRUE
    } else {
        fac_cov <- me_factor_cov(pca_fit$F, spec_risk$factor)
    }
    Sigma_f <- fac_cov$sigma_f

    # ── 6. Recursive residual target + Glasso (§5.5) ──
    recursive_resid_target_used <- FALSE
    if (!is.null(S_e_prev) && is.matrix(S_e_prev)) {
        S_e_t <- me_resid_cov_recursive(S_e_prev, e_t, syms, lambda_e)
        recursive_resid_target_used <- TRUE
    } else {
        S_e_t <- NULL # Will use batch in me_fit_residual_cov
    }

    resid_cov <- me_fit_residual_cov(E, spec_risk$resid, S_e_target = S_e_t)
    Sigma_eps <- resid_cov$sigma_eps
    Theta_eps <- resid_cov$precision

    # ── 7. Recompose + unstandardize (§5.6) ──
    if (standardized_path && !is.null(D_t_daily)) {
        Sigma_total <- me_assemble_total_cov(pca_fit, Sigma_f, Sigma_eps, D_t_daily)
    } else {
        Sigma_total <- me_assemble_total_cov(pca_fit, Sigma_f, Sigma_eps)
    }

    # ── 8. PSD repair ──
    sanity <- me_cov_sanity(Sigma_total, repair = TRUE)
    Sigma_total <- sanity$Sigma

    # ── 9. Baseline allocator (legacy HRP; transitional) ──
    w_hrp <- me_allocate_hrp(Sigma_total, spec_risk$hrp)
    w_baseline <- w_hrp

    # ── 10. Risk covariance objects (§5.7) ──
    H <- spec_risk$horizon$H %||% 1L
    H <- as.integer(H)
    if (!is.finite(H) || H < 1L) H <- 1L

    Sigma_risk_1 <- Sigma_total
    Sigma_risk_H <- H * Sigma_risk_1

    pca_var_explained <- tryCatch(
        {
            denom <- sum(svd(scale(R_std, scale = FALSE))$d^2)
            if (!is.finite(denom) || denom <= 0) NA_real_ else sum(pca_fit$d^2) / denom
        },
        error = function(e) NA_real_
    )

    # ── Output: data + recursive state updates ──
    list(
        sigma_t = sigma_t,
        B_t = pca_fit$B,
        F_t = pca_fit$F,
        E_t = E,
        Sigma_f = Sigma_f,
        Sigma_eps = Sigma_eps,
        Theta_eps = Theta_eps,

        # Architecture-facing risk covariance
        Sigma_total = Sigma_total,
        Sigma_risk_1 = Sigma_risk_1,
        Sigma_risk_H = Sigma_risk_H,

        # Transitional baseline
        w_baseline = w_baseline,
        baseline_method = attr(w_hrp, "allocator_method") %||% "unknown",
        baseline_is_legacy = TRUE,
        w_hrp = w_hrp, # legacy alias

        # Recursive state updates (for model_state_out)
        risk_state_out = list(
            ewma_vol_state = sigma2_t,
            factor_cov_state = Sigma_f,
            resid_cov_state = if (!is.null(S_e_t)) S_e_t else resid_cov$sigma_eps,
            B_prev = pca_fit$B
        ),
        diag = list(
            n_obs_input = n_obs,
            n_assets_input = n_input,
            n_assets_kept = n_kept,
            n_assets_dropped = length(dropped),
            dropped_assets = dropped,
            frac_assets_dropped = length(dropped) / n_input,
            pca_k = pca_fit$k,
            pca_var_explained = pca_var_explained,
            was_repaired = sanity$was_repaired,
            min_eigenvalue = sanity$min_eigenvalue,
            residual_precision_method = resid_cov$precision_method %||% "unknown",
            residual_precision_fallback = isTRUE(resid_cov$precision_fallback),
            residual_precision_fallback_reason = resid_cov$fallback_reason %||% NA_character_,
            glasso_requested = isTRUE(resid_cov$glasso_requested),
            glasso_used = isTRUE(resid_cov$glasso_used),
            allocator_method = attr(w_hrp, "allocator_method") %||% "unknown",
            allocator_fallback = isTRUE(attr(w_hrp, "allocator_fallback")),
            allocator_fallback_reason = attr(w_hrp, "allocator_reason") %||% NA_character_,
            horizon_H = H,
            horizon_covariance_method = if (H > 1L) "scaled_daily_covariance" else "daily_covariance",
            # Architecture diagnostics
            standardized_path_used = standardized_path,
            factor_alignment_used = factor_alignment_used,
            recursive_factor_cov_used = recursive_factor_cov_used,
            recursive_resid_target_used = recursive_resid_target_used
        )
    )
}
