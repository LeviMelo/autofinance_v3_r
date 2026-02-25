# 06_signals.R — Kalman (local linear trend), multiscale TSMOM, factor trends, scalarization

.tanh <- function(x, scale) tanh(x / (scale %||% 1.0))

# ---- Kalman local linear trend (2D state: level, slope) ----

came_kalman_init <- function(y0, q_var, r_var) {
  list(m = c(level = y0, slope = 0), P = diag(c(1, 1)))
}

came_kalman_step <- function(state, y, q_var, r_var) {
  Fm <- matrix(c(1, 1, 0, 1), 2, 2, byrow=TRUE)
  Hm <- matrix(c(1, 0), 1, 2)
  Q <- diag(c(q_var, q_var))
  R <- matrix(r_var, 1, 1)

  m_prev <- matrix(state$m, 2, 1)
  P_prev <- state$P

  m_pred <- Fm %*% m_prev
  P_pred <- Fm %*% P_prev %*% t(Fm) + Q

  y_hat <- (Hm %*% m_pred)[1,1]
  innov <- y - y_hat
  S <- (Hm %*% P_pred %*% t(Hm) + R)[1,1]
  if (!is.finite(S) || S <= 0) S <- r_var + 1e-8
  K <- P_pred %*% t(Hm) / S

  m_new <- m_pred + K * innov
  P_new <- (diag(2) - K %*% Hm) %*% P_pred
  P_new <- came_symmetrize(P_new)

  list(
    m = c(level = m_new[1,1], slope = m_new[2,1]),
    P = P_new,
    innov = innov,
    S = S
  )
}

came_signals_kalman <- function(log_prices_last, kalman_prev, q_var, r_var) {
  syms <- names(log_prices_last)
  init_fn <- function() NULL
  kalman_prev <- came_pi_list(kalman_prev, syms, init_fn = init_fn)

  out_slope <- setNames(rep(0, length(syms)), syms)
  out_z <- setNames(rep(0, length(syms)), syms)
  out_innov_z <- setNames(rep(0, length(syms)), syms)
  states_out <- setNames(vector("list", length(syms)), syms)

  for (nm in syms) {
    y <- log_prices_last[nm]
    if (!is.finite(y)) {
      states_out[[nm]] <- kalman_prev[[nm]]
      next
    }
    st <- kalman_prev[[nm]]
    if (is.null(st)) st <- came_kalman_init(y, q_var, r_var)
    st2 <- came_kalman_step(st, y, q_var, r_var)

    slope <- st2$m["slope"]
    slope_var <- st2$P[2,2]
    slope_sd <- sqrt(max(slope_var, 1e-10))
    out_slope[nm] <- slope
    out_z[nm] <- slope / slope_sd

    innov_z <- st2$innov / sqrt(max(st2$S, 1e-10))
    out_innov_z[nm] <- innov_z

    states_out[[nm]] <- st2
  }

  list(slope = out_slope, slope_z = out_z, innov_z = out_innov_z, state = states_out)
}

# ---- Multiscale momentum ----

came_signals_mom_multiscale <- function(R_window, sigma_daily, horizons, scale) {
  syms <- colnames(R_window); Tn <- nrow(R_window)
  Hs <- as.integer(horizons); Hs <- Hs[Hs >= 2]
  out <- matrix(0, length(syms), length(Hs), dimnames = list(syms, paste0("mom_h", Hs)))
  for (h in seq_along(Hs)) {
    hh <- min(Hs[h], Tn)
    rc <- colSums(tail(R_window, hh), na.rm = TRUE)
    denom <- sigma_daily[syms] * sqrt(hh)
    denom[!is.finite(denom) | denom <= 0] <- median(denom[is.finite(denom) & denom > 0], na.rm = TRUE) %||% 1e-4
    out[, h] <- .tanh(rc / (denom + 1e-8), scale)
  }
  out
}

came_signals_factor_trends <- function(F_window, horizons) {
  if (is.null(F_window) || nrow(F_window) < 10) return(NULL)
  k <- ncol(F_window); Tn <- nrow(F_window)
  Hs <- as.integer(horizons); Hs <- Hs[Hs >= 2]
  out <- matrix(0, k, length(Hs), dimnames = list(colnames(F_window), paste0("fac_h", Hs)))
  for (q in seq_len(k)) {
    f <- F_window[, q]
    f_sd <- sd(f, na.rm = TRUE); if (!is.finite(f_sd) || f_sd <= 0) f_sd <- 1
    for (h in seq_along(Hs)) {
      hh <- min(Hs[h], Tn)
      out[q, h] <- sum(tail(f, hh), na.rm = TRUE) / (f_sd * sqrt(hh) + 1e-8)
    }
  }
  out
}

# ---- Scalarization (trimmed, but real) ----
# Map multiscale vectors to scalar using omega; update omega via ridge on 1-day ahead returns (causal, operational trim).

.came_ridge <- function(X, y, lambda) {
  X <- as.matrix(X); y <- as.numeric(y)
  X[!is.finite(X)] <- 0; y[!is.finite(y)] <- 0
  p <- ncol(X)
  XtX <- crossprod(X) + lambda * diag(p)
  Xty <- crossprod(X, y)
  b <- tryCatch(solve(XtX, Xty), error = function(e) rep(0, p))
  as.numeric(b)
}

came_scalarize_family <- function(U_mat, y_cs, omega_prev, lambda_omega, ridge_lambda) {
  # U_mat: n_assets x d
  syms <- rownames(U_mat); d <- ncol(U_mat)
  if (d == 0) return(list(s = setNames(rep(0, length(syms)), syms), omega = numeric(0)))
  # standardize features cross-sectionally
  mu <- colMeans(U_mat); sdv <- apply(U_mat, 2, sd); sdv[sdv <= 1e-8 | !is.finite(sdv)] <- 1
  U <- scale(U_mat, center = mu, scale = sdv)
  U[!is.finite(U)] <- 0

  omega_new <- .came_ridge(U, y_cs, ridge_lambda)
  # stabilize
  if (sum(abs(omega_new)) > 0) omega_new <- omega_new / sum(abs(omega_new))
  if (!is.null(omega_prev) && length(omega_prev) == d) {
    omega_new <- lambda_omega * omega_prev + (1 - lambda_omega) * omega_new
    if (sum(abs(omega_new)) > 0) omega_new <- omega_new / sum(abs(omega_new))
  }
  s <- as.vector(U %*% omega_new)
  names(s) <- syms
  list(s = s, omega = omega_new, mu = mu, sd = sdv)
}

came_signals_update <- function(P_last, R_window, risk_art, struct_art, state, spec) {
  syms <- colnames(R_window)
  # daily sigma (from EWMA var state) for mom normalization
  sigma_daily <- sqrt(came_pi_vector(state$risk$sigma2, syms, init_val = 1e-4))
  sigma_daily[!is.finite(sigma_daily) | sigma_daily <= 0] <- 1e-4

  # 1) momentum multiscale raw
  mom_h <- spec$signals$mom_horizons
  mom_multi <- came_signals_mom_multiscale(R_window, sigma_daily, mom_h, spec$signals$mom_scale %||% 2.0)

  # 2) residual momentum multiscale: r_perp = D * e_std (use debug retention or recompute approximations)
  resid_multi <- NULL
  if (!is.null(risk_art$E_std) && !is.null(risk_art$D_series)) {
    # D_series: T x p, E_std: T x p => r_perp = D * e
    D <- risk_art$D_series[, syms, drop=FALSE]
    E <- risk_art$E_std[, syms, drop=FALSE]
    R_perp <- E * D
    resid_multi <- came_signals_mom_multiscale(R_perp, sigma_daily, mom_h, spec$signals$mom_scale %||% 2.0)
    colnames(resid_multi) <- paste0("resid_", colnames(resid_multi))
  }

  # 3) Kalman trend slope (stateful)
  qv <- spec$signals$kalman$q_var %||% 1e-5
  rv <- spec$signals$kalman$r_var %||% 1e-3
  logp <- log(pmax(P_last[syms], 1e-12))
  kal_prev <- state$signals$kalman
  kal <- came_signals_kalman(logp, kal_prev, qv, rv)

  # 4) factor trends + projection
  fac_tr <- came_signals_factor_trends(risk_art$F, spec$signals$factor_horizons)
  fac_proj <- setNames(rep(0, length(syms)), syms)
  if (!is.null(fac_tr)) {
    g_bar <- fac_tr[, 1, drop=TRUE]  # shortest horizon
    g_bar[!is.finite(g_bar)] <- 0
    B <- risk_art$B[syms, , drop=FALSE]
    fac_proj <- as.vector(B %*% g_bar)
    names(fac_proj) <- syms
  }

  # 5) scalarization (operational trim; still causal and recursive)
  scalar_enabled <- isTRUE(spec$signals$scalarization$enabled %||% TRUE)
  omega_prev <- state$signals$omega %||% list()
  omega_cfg <- spec$signals$scalarization
  lambda_omega <- omega_cfg$lambda_omega %||% 0.95
  ridge_lambda <- omega_cfg$ridge_lambda %||% 0.05

  # target y_cs: next-day return proxy = last day return cross-sectional rank (no lookahead)
  r1 <- R_window[nrow(R_window), ]
  r1[!is.finite(r1)] <- 0
  y_cs <- rank(r1) / (length(r1) + 1) - 0.5

  s_mom <- setNames(rep(0, length(syms)), syms)
  s_kal <- setNames(rep(0, length(syms)), syms)
  s_fac <- setNames(rep(0, length(syms)), syms)

  omega_out <- list()

  if (scalar_enabled) {
    U_mom <- mom_multi[, , drop=FALSE]
    sres <- came_scalarize_family(U_mom, y_cs, omega_prev$mom, lambda_omega, ridge_lambda)
    s_mom <- sres$s; omega_out$mom <- sres$omega

    U_kal <- cbind(kal$slope_z, kal$innov_z)
    colnames(U_kal) <- c("kal_slope_z","kal_innov_z")
    rownames(U_kal) <- syms
    sres <- came_scalarize_family(U_kal, y_cs, omega_prev$kal, lambda_omega, ridge_lambda)
    s_kal <- sres$s; omega_out$kal <- sres$omega

    U_fac <- cbind(fac_proj)
    colnames(U_fac) <- "fac_proj"
    rownames(U_fac) <- syms
    sres <- came_scalarize_family(U_fac, y_cs, omega_prev$fac, lambda_omega, ridge_lambda)
    s_fac <- sres$s; omega_out$fac <- sres$omega
  } else {
    s_mom <- setNames(mom_multi[,1], syms)
    s_kal <- kal$slope_z
    s_fac <- fac_proj
  }

  st_out <- state
  st_out$signals$kalman <- kal$state
  st_out$signals$omega <- omega_out

  list(
    signals = list(
      mom_multi = mom_multi,
      resid_mom_multi = resid_multi,
      kalman = kal,
      factor_trends = fac_tr,
      fac_proj = fac_proj,
      s_mom = s_mom,
      s_kal = s_kal,
      s_fac = s_fac
    ),
    state_out = st_out
  )
}
