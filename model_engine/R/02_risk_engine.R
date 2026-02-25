#' @title Model Engine — Risk Engine
#' @description PCA factor model, covariance estimation, Glasso, HRP allocation.

# ── Volatility estimation ────────────────────────────────────────────────────

#' @export
me_estimate_vol <- function(R_window, spec_risk_vol) {
    if (is.null(R_window) || nrow(R_window) == 0 || ncol(R_window) == 0) {
        return(numeric(0))
    }
    vols <- apply(R_window, 2, sd, na.rm = TRUE) * sqrt(252)
    vols[!is.finite(vols) | vols <= 0] <- NA_real_
    vols
}

# ── PCA factor model ─────────────────────────────────────────────────────────

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

    # B = loadings (n_assets × k), F = factors (T × k)
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

# ── Factor covariance ─────────────────────────────────────────────────────────

#' @export
me_factor_cov <- function(F_window, spec_factor_cov = list()) {
    sigma_f <- cov(F_window, use = "pairwise.complete.obs")
    sigma_f[!is.finite(sigma_f)] <- 0
    list(sigma_f = sigma_f)
}

# ── Residual covariance (with optional Glasso) ────────────────────────────────

#' @export
me_fit_residual_cov <- function(E_window, spec_resid) {
    use_glasso <- isTRUE(spec_resid$use_glasso)
    lambda <- spec_resid$lambda %||% 0.1

    S_eps <- cov(E_window, use = "pairwise.complete.obs")
    S_eps[!is.finite(S_eps)] <- 0
    n <- ncol(S_eps)

    precision <- NULL
    Sigma_eps <- S_eps

    if (use_glasso && n >= 3) {
        me_require("glasso")
        gl <- tryCatch(
            glasso::glasso(S_eps, rho = lambda, penalize.diagonal = FALSE),
            error = function(e) NULL
        )
        if (!is.null(gl)) {
            Sigma_eps <- gl$w
            precision <- gl$wi
            dimnames(precision) <- dimnames(S_eps)
        }
    }

    if (is.null(precision)) {
        # Ridge fallback for precision approximation
        ridge <- lambda * mean(diag(Sigma_eps), na.rm = TRUE)
        if (!is.finite(ridge) || ridge < 0) ridge <- 1e-6
        Sigma_eps_r <- Sigma_eps
        diag(Sigma_eps_r) <- diag(Sigma_eps_r) + ridge
        precision <- tryCatch(solve(Sigma_eps_r), error = function(e) diag(1 / diag(Sigma_eps_r)))
        dimnames(precision) <- dimnames(S_eps)
    }

    dimnames(Sigma_eps) <- dimnames(S_eps)
    list(sigma_eps = Sigma_eps, precision = precision)
}

# ── Total covariance assembly ─────────────────────────────────────────────────

#' @export
me_assemble_total_cov <- function(pca_fit, Sigma_f, Sigma_eps) {
    B <- pca_fit$B
    Sigma_total <- B %*% Sigma_f %*% t(B) + Sigma_eps
    # Force symmetry
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

# ── HRP allocation ────────────────────────────────────────────────────────────

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

    # Distance matrix from correlation
    cor_mat <- tryCatch(cov2cor(Sigma), error = function(e) NULL)
    if (is.null(cor_mat)) {
        w <- .inv_var_alloc(Sigma)
        names(w) <- colnames(Sigma)
        attr(w, "allocator_method") <- "inv_var_fallback"
        attr(w, "allocator_fallback") <- TRUE
        attr(w, "allocator_reason") <- "cov2cor_failed"
        return(w)
    }
    cor_mat[!is.finite(cor_mat)] <- 0
    diag(cor_mat) <- 1
    dist_mat <- sqrt(pmax(0, (1 - cor_mat) / 2))

    hc <- tryCatch(hclust(as.dist(dist_mat), method = "ward.D2"),
        error = function(e) e
    )

    if (inherits(hc, "error")) {
        w <- .inv_var_alloc(Sigma)
        names(w) <- colnames(Sigma)
        attr(w, "allocator_method") <- "inv_var_fallback"
        attr(w, "allocator_fallback") <- TRUE
        attr(w, "allocator_reason") <- paste("hclust failed:", hc$message)
        return(w)
    }

    sort_ix <- hc$order
    w_hrp <- .get_rec_bipart(Sigma, sort_ix)
    out <- w_hrp[colnames(Sigma)]
    attr(out, "allocator_method") <- "hrp"
    attr(out, "allocator_fallback") <- FALSE
    out
}

# ── Full risk engine orchestrator ─────────────────────────────────────────────

#' @export
me_run_risk_engine <- function(R_window, spec_risk) {
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

    # 1. Volatility
    vols <- me_estimate_vol(R_clean, spec_risk$vol)

    # 2. PCA
    pca_fit <- me_fit_pca(R_clean, spec_risk$pca)

    # 3. Residuals
    E <- me_compute_residuals(R_clean, pca_fit)

    # 4. Factor covariance
    fac_cov <- me_factor_cov(pca_fit$F, spec_risk$factor)
    Sigma_f <- fac_cov$sigma_f

    # 5. Residual covariance
    resid_cov <- me_fit_residual_cov(E, spec_risk$resid)
    Sigma_eps <- resid_cov$sigma_eps
    Theta_eps <- resid_cov$precision

    # 6. Total covariance
    Sigma_total <- me_assemble_total_cov(pca_fit, Sigma_f, Sigma_eps)

    # 7. Sanity
    sanity <- me_cov_sanity(Sigma_total, repair = TRUE)
    Sigma_total <- sanity$Sigma

    # 8. HRP
    w_hrp <- me_allocate_hrp(Sigma_total, spec_risk$hrp)

    list(
        sigma_t = vols,
        B_t = pca_fit$B,
        F_t = pca_fit$F,
        E_t = E,
        Sigma_f = Sigma_f,
        Sigma_eps = Sigma_eps,
        Theta_eps = Theta_eps,
        Sigma_total = Sigma_total,
        w_hrp = w_hrp,
        diag = list(
            n_obs_input = n_obs,
            n_assets_input = n_input,
            n_assets_kept = n_kept,
            n_assets_dropped = length(dropped),
            dropped_assets = dropped,
            frac_assets_dropped = length(dropped) / n_input,
            pca_k = pca_fit$k,
            pca_var_explained = sum(pca_fit$d^2) / sum(svd(scale(R_clean, scale = FALSE))$d^2),
            was_repaired = sanity$was_repaired,
            min_eigenvalue = sanity$min_eigenvalue,
            allocator_method = attr(w_hrp, "allocator_method") %||% "unknown",
            allocator_fallback = isTRUE(attr(w_hrp, "allocator_fallback"))
        )
    )
}
