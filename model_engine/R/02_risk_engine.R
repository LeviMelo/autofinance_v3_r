#' @title Risk Engine
#' @description Risk estimation and baseline allocation logic.

#' @export
me_estimate_vol <- function(R_window, spec_risk_vol) {
    vols <- apply(R_window, 2, sd, na.rm = TRUE) * sqrt(252)
    vols
}

#' @export
me_fit_pca_factor <- function(R_window, spec_pca) {
    me_require("Matrix")
    k <- spec_pca$k %||% 3
    k <- min(k, ncol(R_window))

    # Center, NO SCALING. Track means.
    c_means <- colMeans(R_window, na.rm = TRUE)
    R_centered <- scale(R_window, center = c_means, scale = FALSE)

    # PCA on centered returns
    pca <- prcomp(R_centered, center = FALSE, scale. = FALSE)

    B <- pca$rotation[, 1:k, drop = FALSE]
    F <- pca$x[, 1:k, drop = FALSE]

    list(B = B, F = F, sdev = pca$sdev, centers = c_means)
}

#' @export
me_residualize_returns <- function(R_window, pca_fit) {
    # Reconstruct and subtract in raw space
    R_centered <- scale(R_window, center = pca_fit$centers, scale = FALSE)

    # Systematic = F %*% t(B)
    Sys <- pca_fit$F %*% t(pca_fit$B)

    E <- R_centered - Sys
    dimnames(E) <- dimnames(R_window)
    E
}

#' @export
me_fit_residual_cov <- function(E_window, spec_resid) {
    use_glasso <- isTRUE(spec_resid$use_glasso)
    lambda <- spec_resid$lambda %||% 0.1

    if (use_glasso) {
        me_require("glasso")
    }

    # Sample residual covariance
    Sigma_eps <- cov(E_window, use = "pairwise.complete.obs")
    if (is.null(Sigma_eps) || any(!is.finite(Sigma_eps))) {
        stop("Residual covariance contains non-finite values.")
    }

    # Ensure dimnames from residual matrix columns
    dn <- list(colnames(E_window), colnames(E_window))
    dimnames(Sigma_eps) <- dn

    # Basic diagonal regularization before any inversion/optimization
    d <- diag(Sigma_eps)
    if (any(!is.finite(d)) || any(d <= 0)) {
        bad <- !is.finite(d) | d <= 0
        med_d <- median(d[is.finite(d) & d > 0], na.rm = TRUE)
        if (!is.finite(med_d) || med_d <= 0) med_d <- 1e-6
        d[bad] <- med_d
        diag(Sigma_eps) <- d
    }

    precision <- NULL

    if (use_glasso) {
        gl <- glasso::glasso(Sigma_eps, rho = lambda)
        Sigma_eps <- gl$w
        precision <- gl$wi
        dimnames(Sigma_eps) <- dn
        dimnames(precision) <- dn
    } else {
        ridge <- lambda * mean(diag(Sigma_eps), na.rm = TRUE)
        if (!is.finite(ridge) || ridge < 0) ridge <- 0
        diag(Sigma_eps) <- diag(Sigma_eps) + ridge
    }

    list(sigma_eps = Sigma_eps, precision = precision)
}

#' @export
me_factor_cov <- function(F_window, spec_factor_cov) {
    sigma_f <- cov(F_window, use = "pairwise.complete.obs")
    list(sigma_f = sigma_f)
}

#' @export
me_assemble_total_cov <- function(pca_fit, Sigma_f, Sigma_eps) {
    B <- pca_fit$B
    Sigma_total <- B %*% Sigma_f %*% t(B) + Sigma_eps
    dimnames(Sigma_total) <- dimnames(Sigma_eps)
    Sigma_total
}

#' @export
me_cov_sanity <- function(Sigma, repair = TRUE) {
    me_require("Matrix")
    repaired <- FALSE
    if (!isSymmetric(Sigma)) {
        Sigma <- (Sigma + t(Sigma)) / 2
        repaired <- TRUE
    }

    eig <- eigen(Sigma, symmetric = TRUE, only.values = TRUE)$values
    if (any(eig <= 0) || any(is.na(eig))) {
        if (repair) {
            Sigma <- as.matrix(Matrix::nearPD(Sigma, corr = FALSE, ensureSymmetry = TRUE)$mat)
            repaired <- TRUE
        }
    }
    list(is_valid = !any(eigen(Sigma)$values <= 0), repaired = repaired, Sigma = Sigma)
}

.inv_var_alloc <- function(cov_mat) {
    v <- diag(cov_mat)
    v[v <= 1e-8] <- 1e-8
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
            split_idx <- floor(length(c_ix) / 2)
            c1 <- c_ix[1:split_idx]
            c2 <- c_ix[(split_idx + 1):length(c_ix)]

            v1 <- .get_cluster_var(cov_mat, c1)
            v2 <- .get_cluster_var(cov_mat, c2)

            alpha <- 1 - v1 / (v1 + v2)
            w[c1] <- w[c1] * alpha
            w[c2] <- w[c2] * (1 - alpha)

            clusters <- append(clusters, list(c1, c2))
        }
    }
    return(w)
}

#' @export
me_allocate_hrp <- function(Sigma, spec_hrp) {
    n <- ncol(Sigma)
    if (is.null(n) || n == 0) {
        return(numeric(0))
    }
    if (n == 1) {
        w <- 1.0
        names(w) <- colnames(Sigma)
        return(w)
    }

    # Symmetrize defensively
    Sigma <- (Sigma + t(Sigma)) / 2

    # Safe correlation conversion
    s <- sqrt(pmax(diag(Sigma), 1e-12))
    R <- Sigma / (s %o% s)

    # Numerical cleanup
    R[!is.finite(R)] <- 0
    R <- (R + t(R)) / 2
    diag(R) <- 1
    R <- pmin(pmax(R, -1), 1)

    # Distance for HRP clustering
    D <- 0.5 * (1 - R)
    D[!is.finite(D)] <- 0
    D[D < 0] <- 0
    diag(D) <- 0

    dist_mat <- sqrt(D)

    hc <- tryCatch(
        hclust(as.dist(dist_mat), method = "single"),
        error = function(e) NULL
    )

    if (is.null(hc)) {
        w <- .inv_var_alloc(Sigma)
        names(w) <- colnames(Sigma)
        return(w)
    }

    sort_ix <- hc$order
    w_hrp <- .get_rec_bipart(Sigma, sort_ix)
    w_hrp[colnames(Sigma)]
}

#' @export
me_run_risk_engine <- function(R_window, spec_risk) {
    # Drop assets with insufficient data (e.g. any NAs inside the required window)
    keep_assets <- colSums(is.na(R_window)) == 0
    R_clean <- R_window[, keep_assets, drop = FALSE]

    if (ncol(R_clean) < 2) {
        stop("Risk engine needs >= 2 assets without missing data in risk lookback.")
    }

    vols <- me_estimate_vol(R_clean, spec_risk$vol)
    pca_fit <- me_fit_pca_factor(R_clean, spec_risk$pca)
    E <- me_residualize_returns(R_clean, pca_fit)
    resid_res <- me_fit_residual_cov(E, spec_risk$resid)
    Sigma_eps <- resid_res$sigma_eps
    Theta_eps <- resid_res$precision

    fac_res <- me_factor_cov(pca_fit$F, spec_risk$factor)
    Sigma_f <- fac_res$sigma_f

    Sigma_total <- me_assemble_total_cov(pca_fit, Sigma_f, Sigma_eps)
    sanity <- me_cov_sanity(Sigma_total, repair = TRUE)
    Sigma_repaired <- sanity$Sigma

    w_hrp <- me_allocate_hrp(Sigma_repaired, spec_risk$hrp)

    list(
        sigma_t = vols,
        B_t = pca_fit$B,
        F_t = pca_fit$F,
        E_t = E,
        Sigma_f = Sigma_f,
        Sigma_eps = Sigma_eps,
        Theta_eps = Theta_eps,
        Sigma_total = Sigma_repaired,
        w_hrp = w_hrp,
        diag = list(
            was_repaired = sanity$repaired,
            kept_assets = colnames(R_clean),
            dropped_assets = setdiff(colnames(R_window), colnames(R_clean))
        )
    )
}
