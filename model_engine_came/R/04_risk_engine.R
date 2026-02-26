# 04_risk_engine.R — Risk engine (architecture §5)
# Outputs: Sigma^(1), Sigma^(H), Theta (Glasso precision), PCA loadings/factors, EWMA vol state.
# Key invariant: residual precision uses Glasso only.

# ---- internal: build/extend sigma2 history over a dated return window ----
.came_risk_build_sigma2_hist <- function(R, sigma2_prev, sigma2_hist_prev, lambda_sigma) {
  came_assert(is.matrix(R) && nrow(R) >= 2, "risk_sigma2_hist_R", "R must be matrix with >=2 rows")
  syms <- colnames(R)
  Tn <- nrow(R)

  # dates must exist and be Date-coercible
  if (is.null(rownames(R))) came_stop("risk_dates_missing", "R must have rownames as Date strings")
  dR <- as.Date(rownames(R))
  came_assert(!any(is.na(dR)), "risk_dates_invalid", "R rownames must be Date-coercible strings")

  lam <- lambda_sigma
  if (!is.finite(lam) || lam <= 0 || lam >= 1) came_stop("risk_lambda_sigma", "lambda_sigma must be in (0,1)")

  # align sigma2_prev to current universe (for correct sigma2_t update)
  if (is.null(sigma2_prev) || length(sigma2_prev) == 0) {
    init <- apply(R[seq_len(min(21, Tn)), , drop = FALSE], 2, function(x) {
      v <- stats::var(x, na.rm = TRUE)
      if (!is.finite(v) || v <= 0) 1e-4 else v
    })
    sigma2_prev_aligned <- setNames(as.numeric(init), syms)
  } else {
    init_val <- stats::median(sigma2_prev[is.finite(sigma2_prev) & sigma2_prev > 0], na.rm = TRUE)
    if (!is.finite(init_val) || init_val <= 0) init_val <- 1e-4
    sigma2_prev_aligned <- came_pi_vector(sigma2_prev, syms, init_val = init_val)
  }
  sigma2_prev_aligned[!is.finite(sigma2_prev_aligned) | sigma2_prev_aligned <= 0] <- 1e-4

  # canonical one-step update for *current* day (t = last row of R)
  r_last <- as.numeric(R[Tn, ])
  r_last[!is.finite(r_last)] <- 0
  sigma2_t <- lam * sigma2_prev_aligned + (1 - lam) * (r_last^2)
  sigma2_t[!is.finite(sigma2_t) | sigma2_t <= 0] <- 1e-4
  names(sigma2_t) <- syms

  # attempt safe extension from sigma2_hist_prev if it contains all dates up to t-1
  can_extend <- FALSE
  if (!is.null(sigma2_hist_prev) && is.matrix(sigma2_hist_prev) && !is.null(rownames(sigma2_hist_prev))) {
    dPrev <- as.Date(rownames(sigma2_hist_prev))
    if (!any(is.na(dPrev))) {
      want_dates <- as.character(dR[seq_len(Tn - 1)])
      have_dates <- as.character(dPrev)
      can_extend <- all(want_dates %in% have_dates)
    }
  }

  if (can_extend) {
    # take prior history for window up to t-1, align columns, then append t
    Hprev <- sigma2_hist_prev[as.character(dR[seq_len(Tn - 1)]), , drop = FALSE]
    H <- matrix(NA_real_, Tn, length(syms), dimnames = list(as.character(dR), syms))

    common <- intersect(colnames(Hprev), syms)
    if (length(common) > 0) H[seq_len(Tn - 1), common] <- Hprev[, common, drop = FALSE]

    # initialize any missing columns on those dates with sigma2_prev_aligned (prior)
    for (nm in syms) {
      if (all(is.na(H[seq_len(Tn - 1), nm]))) H[seq_len(Tn - 1), nm] <- sigma2_prev_aligned[nm]
    }

    # last row: enforce canonical sigma2_t (state-consistent)
    H[Tn, ] <- sigma2_t
  } else {
    # cold start rebuild over the window (legitimate prior for missing latent sigma2_{t-L})
    H <- matrix(NA_real_, Tn, length(syms), dimnames = list(as.character(dR), syms))

    sigma2 <- apply(R[seq_len(min(21, Tn)), , drop = FALSE], 2, function(x) {
      v <- stats::var(x, na.rm = TRUE)
      if (!is.finite(v) || v <= 0) 1e-4 else v
    })
    sigma2 <- setNames(as.numeric(sigma2), syms)

    for (t in seq_len(Tn)) {
      rt <- as.numeric(R[t, ])
      rt[!is.finite(rt)] <- 0
      sigma2 <- lam * sigma2 + (1 - lam) * (rt^2)
      sigma2[!is.finite(sigma2) | sigma2 <= 0] <- 1e-4
      H[t, ] <- sigma2
    }

    # enforce canonical sigma2_t on last row (state-consistent)
    H[Tn, ] <- sigma2_t
  }

  list(sigma2_hist = H, sigma2_t = sigma2_t)
}

# ---- PCA via SVD on centered standardized returns ----
.came_pca_svd <- function(R_std, k) {
  n <- ncol(R_std)
  Tn <- nrow(R_std)
  came_assert(n >= 3 && Tn >= 10, "risk_pca_min", "PCA requires >=3 assets and >=10 observations")

  k <- min(as.integer(k), n - 1L, Tn - 1L)
  came_assert(k >= 1, "risk_pca_k", "k must be >= 1 after bounding")

  centers <- colMeans(R_std, na.rm = TRUE)
  X <- scale(R_std, center = centers, scale = FALSE)
  X[!is.finite(X)] <- 0

  sv <- svd(X, nu = k, nv = k)
  B_raw <- sv$v[, 1:k, drop = FALSE]
  rownames(B_raw) <- colnames(R_std)
  colnames(B_raw) <- paste0("PC", seq_len(k))

  F_raw <- X %*% B_raw
  colnames(F_raw) <- colnames(B_raw)

  list(B_raw = B_raw, F_raw = F_raw, centers = centers, k = k)
}

# ---- PCA factor identity alignment (architecture §5.3) ----
.came_align_factors <- function(B_raw, F_raw, B_prev) {
  came_require("clue")

  k <- ncol(B_raw)
  if (is.null(B_prev) || !is.matrix(B_prev) || ncol(B_prev) != k) {
    return(list(B = B_raw, F = F_raw, R = diag(k), used = FALSE))
  }

  common <- intersect(rownames(B_prev), rownames(B_raw))
  if (length(common) < max(5L, k)) {
    return(list(B = B_raw, F = F_raw, R = diag(k), used = FALSE))
  }

  C <- t(B_prev[common, , drop = FALSE]) %*% B_raw[common, , drop = FALSE] # k x k
  cost <- -abs(C)

  perm <- as.integer(clue::solve_LSAP(cost))
  signs <- rep(1, k)
  for (j in seq_len(k)) {
    v <- C[perm[j], j]
    s <- sign(v)
    if (!is.finite(s) || s == 0) s <- 1
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

# ---- main risk update ----
came_risk_update <- function(R_window, state, spec) {
  came_require(c("glasso", "Matrix", "clue"))

  came_assert(
    is.matrix(R_window) && nrow(R_window) >= 10 && ncol(R_window) >= 3,
    "risk_R_window", "R_window must be matrix with >=10 obs and >=3 assets"
  )
  came_assert(!is.null(colnames(R_window)), "risk_colnames", "R_window must have colnames (symbols)")
  came_assert(!is.null(rownames(R_window)), "risk_rownames", "R_window must have rownames (Date strings)")

  syms <- colnames(R_window)
  L <- as.integer(spec$risk$lookback %||% nrow(R_window))
  R <- tail(R_window, min(nrow(R_window), L))

  # --- EWMA volatility state (architecture §5.1) ---
  lambda_sigma <- spec$risk$lambda_sigma
  sig_hist <- .came_risk_build_sigma2_hist(
    R = R,
    sigma2_prev = state$risk$sigma2,
    sigma2_hist_prev = state$risk$sigma2_hist,
    lambda_sigma = lambda_sigma
  )

  sigma2_hist <- sig_hist$sigma2_hist
  sigma2_t <- sig_hist$sigma2_t

  D_series <- sqrt(pmax(sigma2_hist, 1e-12)) # T x p
  D_t <- D_series[nrow(D_series), ] # p
  D_t[!is.finite(D_t) | D_t <= 0] <- 1e-4

  # standardized returns window: r_tilde = D^{-1} r (architecture §5.1)
  R_std <- R
  for (j in seq_len(ncol(R_std))) {
    R_std[, j] <- R_std[, j] / D_series[, j]
  }
  R_std[!is.finite(R_std)] <- 0

  # --- PCA systematic layer (architecture §5.2) ---
  pca <- .came_pca_svd(R_std, spec$risk$k %||% 5L)
  B_raw <- pca$B_raw
  F_raw <- pca$F_raw
  centers <- pca$centers

  align_used <- FALSE
  if (isTRUE(spec$risk$align_factors %||% TRUE)) {
    al <- .came_align_factors(B_raw, F_raw, state$risk$B_prev)
    B <- al$B
    F <- al$F
    align_used <- al$used
  } else {
    B <- B_raw
    F <- F_raw
  }

  k <- ncol(B)
  came_assert(k >= 1, "risk_k", "k must be >= 1")

  # --- factor covariance recursion (architecture §5.4) ---
  f_t <- as.numeric(F[nrow(F), ])
  f_t[!is.finite(f_t)] <- 0

  Sigma_f_prev <- state$risk$Sigma_f
  if (is.null(Sigma_f_prev) || !is.matrix(Sigma_f_prev) || nrow(Sigma_f_prev) != k || ncol(Sigma_f_prev) != k) {
    Sigma_f_prev <- diag(1, k)
  }
  lambda_f <- spec$risk$lambda_f
  Sigma_f <- lambda_f * Sigma_f_prev + (1 - lambda_f) * (f_t %*% t(f_t))
  Sigma_f <- came_symmetrize(Sigma_f)

  # --- residuals (architecture §5.5): e_t = r_tilde_centered - B f_t ---
  Xc <- scale(R_std, center = centers, scale = FALSE)
  Xc[!is.finite(Xc)] <- 0
  Sys <- F %*% t(B) # T x p reconstruction in centered space
  E_std <- Xc - Sys # T x p residuals in centered standardized space
  dimnames(E_std) <- dimnames(R_std)

  e_t <- as.numeric(E_std[nrow(E_std), ])
  e_t[!is.finite(e_t)] <- 0
  names(e_t) <- syms

  # --- residual target recursion S_e and Glasso precision Theta (architecture §5.5) ---
  S_e_prev <- state$risk$S_e
  if (is.null(S_e_prev) || !is.matrix(S_e_prev)) {
    S_e_prev <- diag(1e-4, length(syms))
    dimnames(S_e_prev) <- list(syms, syms)
  } else {
    initd <- stats::median(diag(S_e_prev), na.rm = TRUE)
    if (!is.finite(initd) || initd <= 0) initd <- 1e-4
    S_e_prev <- came_pi_matrix(S_e_prev, syms, init_diag = initd)
  }

  lambda_e <- spec$risk$lambda_e
  ee <- e_t %*% t(e_t)
  dimnames(ee) <- list(syms, syms)

  S_e <- lambda_e * S_e_prev + (1 - lambda_e) * ee
  S_e <- came_symmetrize(S_e)
  diag(S_e) <- pmax(diag(S_e), 1e-10)

  lam_gl <- spec$risk$glasso_lambda %||% 0.10
  psd_eps <- spec$risk$psd_eps %||% 1e-8

  S_in <- came_symmetrize(S_e)
  diag(S_in) <- diag(S_in) + psd_eps

  gl <- tryCatch(glasso::glasso(S_in, rho = lam_gl, penalize.diagonal = FALSE), error = function(e) e)
  if (inherits(gl, "error")) came_stop("risk_glasso_failed", gl$message)

  Theta <- gl$wi
  Sigma_e <- gl$w
  dimnames(Theta) <- list(syms, syms)
  dimnames(Sigma_e) <- list(syms, syms)

  # --- recomposition and numerical repair (architecture §5.6) ---
  Sigma_std <- B %*% Sigma_f %*% t(B) + Sigma_e
  Sigma_std <- came_symmetrize(Sigma_std)

  Sigma_1 <- diag(D_t) %*% Sigma_std %*% diag(D_t)
  dimnames(Sigma_1) <- list(syms, syms)
  Sigma_1 <- came_near_psd(Sigma_1, eps = psd_eps)

  # --- horizon covariance (architecture §5.7 default approximation) ---
  H <- as.integer(spec$forecast$H %||% 1L)
  if (!is.finite(H) || H < 1L) H <- 1L
  Sigma_H <- H * Sigma_1

  # Keep enough series for residual momentum (architecture §7.1)
  mom_keep <- max(as.integer(spec$signals$mom_horizons %||% 252L))
  keep_T <- min(nrow(E_std), mom_keep)
  E_keep <- tail(E_std, keep_T)
  D_keep <- tail(D_series, keep_T)

  # Update state (store sigma2_hist as rolling window aligned to R dates)
  st_out <- state
  st_out$risk$sigma2 <- sigma2_t
  st_out$risk$sigma2_hist <- sigma2_hist
  st_out$risk$Sigma_f <- Sigma_f
  st_out$risk$S_e <- S_e
  st_out$risk$B_prev <- B

  list(
    risk = list(
      sigma2 = sigma2_t,
      D_t = D_t,
      B = B,
      F = F,
      centers = centers,
      Sigma_f = Sigma_f,
      S_e = S_e,
      Theta = Theta,
      Sigma_e = Sigma_e,
      Sigma_1 = Sigma_1,
      Sigma_H = Sigma_H,
      E_std = E_keep,
      D_series = D_keep
    ),
    state_out = st_out,
    diag = list(
      k = k,
      align_used = align_used,
      glasso_lambda = lam_gl,
      H = H,
      min_eig = min(eigen(Sigma_1, symmetric = TRUE, only.values = TRUE)$values)
    )
  )
}
