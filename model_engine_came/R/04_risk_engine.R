# 04_risk_engine.R — architecture-aligned risk recursion (EWMA vol, PCA align, EWMA cov, Glasso)

.came_risk_build_sigma2_series <- function(R, sigma2_start, lambda_sigma) {
  # Reconstruct daily sigma2 over the window given starting variance (vector) and realized returns.
  # Returns list(sigma2_T = last, D_series = T x p daily sigma, sigma2_series = T x p)
  Tn <- nrow(R); p <- ncol(R)
  syms <- colnames(R)
  sigma2 <- sigma2_start[syms]
  sigma2[!is.finite(sigma2) | sigma2 <= 0] <- 1e-4

  sigma2_series <- matrix(NA_real_, Tn, p, dimnames = list(rownames(R), syms))
  D_series <- matrix(NA_real_, Tn, p, dimnames = list(rownames(R), syms))

  for (t in seq_len(Tn)) {
    r_t <- R[t, ]
    r_t[!is.finite(r_t)] <- 0
    sigma2 <- lambda_sigma * sigma2 + (1 - lambda_sigma) * (r_t^2)
    sigma2[!is.finite(sigma2) | sigma2 <= 0] <- 1e-4
    sigma2_series[t, ] <- sigma2
    D_series[t, ] <- sqrt(sigma2)
  }

  list(sigma2_T = sigma2, D_series = D_series, sigma2_series = sigma2_series)
}

.came_pca_svd <- function(R_std, k) {
  n <- ncol(R_std); Tn <- nrow(R_std)
  came_assert(n >= 3 && Tn >= 10, "risk_pca_min", "PCA requires >=3 assets and >=10 obs")
  k <- min(as.integer(k), n - 1L, Tn - 1L)
  centers <- colMeans(R_std, na.rm = TRUE)
  X <- scale(R_std, center = centers, scale = FALSE)
  X[!is.finite(X)] <- 0
  sv <- svd(X, nu = k, nv = k)
  B_raw <- sv$v[, 1:k, drop = FALSE]
  rownames(B_raw) <- colnames(R_std)
  colnames(B_raw) <- paste0("PC", seq_len(k))
  F_raw <- X %*% B_raw
  colnames(F_raw) <- colnames(B_raw)
  list(B_raw = B_raw, F_raw = F_raw, centers = centers, d = sv$d[1:k], k = k)
}

.came_align_factors <- function(B_raw, F_raw, B_prev) {
  # exact assignment via clue::solve_LSAP on cost = -abs(C)
  came_require("clue")
  k <- ncol(B_raw)
  if (is.null(B_prev) || !is.matrix(B_prev) || ncol(B_prev) != k) {
    return(list(B = B_raw, F = F_raw, R = diag(k), used = FALSE))
  }
  common <- intersect(rownames(B_prev), rownames(B_raw))
  if (length(common) < max(5L, k)) {
    return(list(B = B_raw, F = F_raw, R = diag(k), used = FALSE))
  }
  C <- t(B_prev[common, , drop = FALSE]) %*% B_raw[common, , drop = FALSE]
  cost <- -abs(C)
  perm <- as.integer(clue::solve_LSAP(cost))  # assignment of rows to columns
  # perm[j] = assigned row for column j
  signs <- rep(1, k)
  for (j in seq_len(k)) {
    v <- C[perm[j], j]
    s <- sign(v); if (!is.finite(s) || s == 0) s <- 1
    signs[j] <- s
  }
  R <- matrix(0, k, k)
  for (j in seq_len(k)) R[perm[j], j] <- signs[j]
  B <- B_raw %*% R
  F <- F_raw %*% R
  colnames(B) <- colnames(B_raw)
  colnames(F) <- colnames(B_raw)
  list(B = B, F = F, R = R, used = TRUE)
}

came_risk_update <- function(R_window, state, spec) {
  came_require(c("glasso","Matrix"))
  came_assert(is.matrix(R_window) && nrow(R_window) >= 10 && ncol(R_window) >= 3,
              "risk_R_window", "R_window must be matrix with >=10 obs and >=3 assets")

  syms <- colnames(R_window)
  L <- spec$risk$lookback %||% nrow(R_window)
  R <- tail(R_window, min(nrow(R_window), L))
  # Ensure rownames are dates (for alignment in training later)
  if (is.null(rownames(R))) rownames(R) <- paste0("t", seq_len(nrow(R)))

  sigma2_prev <- state$risk$sigma2
  if (is.null(sigma2_prev) || length(sigma2_prev) == 0) {
    # initialize from sample variance of first 21 days
    init <- apply(R, 2, function(x) {
      v <- var(x[seq_len(min(21, length(x)))], na.rm = TRUE)
      if (!is.finite(v) || v <= 0) 1e-4 else v
    })
    sigma2_prev <- setNames(init, syms)
  } else {
    sigma2_prev <- came_pi_vector(sigma2_prev, syms, init_val = median(sigma2_prev[sigma2_prev > 0], na.rm = TRUE))
  }

  lambda_sigma <- spec$risk$lambda_sigma %||% 0.94
  sig_series <- .came_risk_build_sigma2_series(R, sigma2_prev, lambda_sigma)
  sigma2_t <- sig_series$sigma2_T
  D_series <- sig_series$D_series
  D_t <- D_series[nrow(D_series), ]
  D_t[D_t <= 0 | !is.finite(D_t)] <- 1e-4

  # standardized window
  R_std <- R
  for (j in seq_len(ncol(R_std))) R_std[, j] <- R_std[, j] / D_series[, j]
  R_std[!is.finite(R_std)] <- 0

  # PCA
  pca <- .came_pca_svd(R_std, spec$risk$k %||% 5L)
  B_raw <- pca$B_raw
  F_raw <- pca$F_raw

  align_used <- FALSE
  if (isTRUE(spec$risk$align_factors %||% TRUE)) {
    al <- .came_align_factors(B_raw, F_raw, state$risk$B_prev)
    B <- al$B; F <- al$F; align_used <- al$used
  } else {
    B <- B_raw; F <- F_raw
  }

  # factor covariance recursion
  k <- ncol(B)
  f_t <- as.numeric(F[nrow(F), ])
  f_t[!is.finite(f_t)] <- 0
  Sigma_f_prev <- state$risk$Sigma_f
  if (is.null(Sigma_f_prev) || !is.matrix(Sigma_f_prev) || nrow(Sigma_f_prev) != k) {
    Sigma_f_prev <- diag(1, k)
  }
  lambda_f <- spec$risk$lambda_f %||% 0.97
  Sigma_f <- lambda_f * Sigma_f_prev + (1 - lambda_f) * (f_t %*% t(f_t))
  Sigma_f <- came_symmetrize(Sigma_f)

  # standardized residuals e_t = r_std - B f_t (per-day residual series)
  Sys <- F %*% t(B)
  E_std <- (scale(R_std, center = pca$centers, scale = FALSE) %>% {.[!is.finite(.)] <- 0; .}) - Sys
  dimnames(E_std) <- dimnames(R_std)
  e_t <- as.numeric(E_std[nrow(E_std), ])
  names(e_t) <- syms

  # residual target recursion S_e (standardized)
  S_e_prev <- state$risk$S_e
  if (is.null(S_e_prev) || !is.matrix(S_e_prev)) {
    S_e_prev <- diag(1e-4, length(syms))
    dimnames(S_e_prev) <- list(syms, syms)
  } else {
    S_e_prev <- came_pi_matrix(S_e_prev, syms, init_diag = median(diag(S_e_prev), na.rm = TRUE))
  }
  lambda_e <- spec$risk$lambda_e %||% 0.97
  ee <- e_t %*% t(e_t)
  dimnames(ee) <- list(syms, syms)
  S_e <- lambda_e * S_e_prev + (1 - lambda_e) * ee
  S_e <- came_symmetrize(S_e)
  diag(S_e) <- pmax(diag(S_e), 1e-10)

  # Glasso precision Θ on S_e (Glasso-only; numeric stabilization allowed)
  lam <- spec$risk$glasso_lambda %||% 0.10
  S_in <- came_symmetrize(S_e)
  diag(S_in) <- diag(S_in) + spec$risk$psd_eps %||% 1e-8
  gl <- tryCatch(glasso::glasso(S_in, rho = lam, penalize.diagonal = FALSE), error = function(e) e)
  if (inherits(gl, "error")) stop(came_error("risk_glasso_failed", gl$message))
  Theta <- gl$wi
  Sigma_e <- gl$w
  dimnames(Theta) <- list(syms, syms)
  dimnames(Sigma_e) <- list(syms, syms)

  # standardized daily covariance Σ̃ = B Σ_f B' + Σ_e
  Sigma_std <- B %*% Sigma_f %*% t(B) + Sigma_e
  Sigma_std <- came_symmetrize(Sigma_std)

  # unstandardize Σ = D Σ̃ D (daily)
  Sigma_1 <- diag(D_t) %*% Sigma_std %*% diag(D_t)
  dimnames(Sigma_1) <- list(syms, syms)

  # PSD repair
  Sigma_1 <- came_near_psd(Sigma_1, eps = spec$risk$psd_eps %||% 1e-8)

  H <- as.integer(spec$forecast$H %||% 1L)
  if (!is.finite(H) || H < 1L) H <- 1L
  Sigma_H <- H * Sigma_1

  list(
    risk = list(
      sigma2 = sigma2_t,
      D_t = D_t,
      B = B,
      F = F,
      Sigma_f = Sigma_f,
      S_e = S_e,
      Theta = Theta,
      Sigma_e = Sigma_e,
      Sigma_1 = Sigma_1,
      Sigma_H = Sigma_H,
      E_std = if (isTRUE(spec$meta$retain_debug)) E_std else NULL,
      D_series = if (isTRUE(spec$meta$retain_debug)) D_series else NULL
    ),
    state_out = within(state, {
      risk$sigma2 <- sigma2_t
      risk$Sigma_f <- Sigma_f
      risk$S_e <- S_e
      risk$B_prev <- B
    }),
    diag = list(
      k = k,
      align_used = align_used,
      min_eig = min(eigen(Sigma_1, symmetric = TRUE, only.values = TRUE)$values),
      glasso_lambda = lam,
      H = H
    )
  )
}
