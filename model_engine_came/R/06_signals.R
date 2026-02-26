# 06_signals.R — Signal primitives + scalarization (architecture §7)
# Provides:
#   - multiscale raw momentum m_raw(h)
#   - multiscale residual momentum m_res(h) using r_perp = D * e
#   - per-asset Kalman local-linear-trend features
#   - factor trend features g_q(h)
#   - scalar signals s^(mom), s^(kal), s^(fac) with recursive omega updates

.tanh_scaled <- function(x, scale) tanh(x / (scale %||% 1.0))

# ---- Kalman local linear trend (architecture §7.2) ----
came_kalman_init <- function(y0, q_var = NULL, r_var = NULL) {
  # q_var / r_var are accepted for compatibility with callers, but init does not need them.
  list(m = c(level = y0, slope = 0), P = diag(c(1, 1)))
}

came_kalman_step <- function(state, y, q_var, r_var) {
  Fm <- matrix(c(1, 1, 0, 1), 2, 2, byrow = TRUE)
  Hm <- matrix(c(1, 0), 1, 2)
  Q <- diag(c(q_var, q_var))
  R <- matrix(r_var, 1, 1)

  m_prev <- matrix(state$m, 2, 1)
  P_prev <- state$P

  m_pred <- Fm %*% m_prev
  P_pred <- Fm %*% P_prev %*% t(Fm) + Q

  y_hat <- (Hm %*% m_pred)[1, 1]
  innov <- y - y_hat
  S <- (Hm %*% P_pred %*% t(Hm) + R)[1, 1]
  if (!is.finite(S) || S <= 0) S <- r_var + 1e-8
  K <- P_pred %*% t(Hm) / S

  m_new <- m_pred + K * innov
  P_new <- (diag(2) - K %*% Hm) %*% P_pred
  P_new <- came_symmetrize(P_new)

  list(
    m = c(level = m_new[1, 1], slope = m_new[2, 1]),
    P = P_new,
    innov = innov,
    S = S
  )
}

came_signals_kalman <- function(log_prices_last, kalman_prev, q_var, r_var) {
  syms <- names(log_prices_last)
  kalman_prev <- came_pi_list(kalman_prev, syms, init_fn = function() NULL)

  out_slope <- setNames(rep(0, length(syms)), syms)
  out_slope_var <- setNames(rep(1, length(syms)), syms)
  out_slope_z <- setNames(rep(0, length(syms)), syms)
  out_innov <- setNames(rep(0, length(syms)), syms)
  out_innov_z <- setNames(rep(0, length(syms)), syms)
  out_S <- setNames(rep(0, length(syms)), syms)

  states_out <- setNames(vector("list", length(syms)), syms)

  for (nm in syms) {
    y <- log_prices_last[nm]
    if (!is.finite(y)) {
      states_out[[nm]] <- kalman_prev[[nm]]
      next
    }

    st <- kalman_prev[[nm]]
    if (is.null(st)) st <- came_kalman_init(y)

    st2 <- came_kalman_step(st, y, q_var, r_var)

    slope <- st2$m["slope"]
    slope_var <- st2$P[2, 2]
    slope_var <- if (!is.finite(slope_var) || slope_var <= 1e-12) 1e-12 else slope_var
    slope_sd <- sqrt(slope_var)

    out_slope[nm] <- slope
    out_slope_var[nm] <- slope_var
    out_slope_z[nm] <- slope / slope_sd

    out_innov[nm] <- st2$innov
    out_innov_z[nm] <- st2$innov / sqrt(max(st2$S, 1e-12))
    out_S[nm] <- st2$S

    states_out[[nm]] <- st2
  }

  list(
    slope = out_slope,
    slope_var = out_slope_var,
    slope_z = out_slope_z,
    innov = out_innov,
    S = out_S,
    innov_z = out_innov_z,
    state = states_out
  )
}

# ---- multiscale momentum (architecture §7.1) ----
came_signals_mom_multiscale <- function(R_window, sigma_daily, horizons, scale, prefix = "mom_h") {
  syms <- colnames(R_window)
  Tn <- nrow(R_window)
  Hs <- as.integer(horizons)
  Hs <- Hs[Hs >= 2]
  came_assert(length(Hs) > 0, "signals_mom_horizons", "signals$mom_horizons must contain values >= 2")

  out <- matrix(0, length(syms), length(Hs), dimnames = list(syms, paste0(prefix, Hs)))

  for (h in seq_along(Hs)) {
    hh <- min(Hs[h], Tn)
    rc <- colSums(tail(R_window, hh), na.rm = TRUE)
    denom <- sigma_daily[syms] * sqrt(hh)
    denom[!is.finite(denom) | denom <= 0] <- stats::median(denom[is.finite(denom) & denom > 0], na.rm = TRUE) %||% 1e-4
    out[, h] <- .tanh_scaled(rc / (denom + 1e-8), scale)
  }
  out
}

# ---- factor trends (architecture §7.3) ----
came_signals_factor_trends <- function(F_window, horizons) {
  if (is.null(F_window) || nrow(F_window) < 10) {
    return(NULL)
  }

  k <- ncol(F_window)
  Tn <- nrow(F_window)
  Hs <- as.integer(horizons)
  Hs <- Hs[Hs >= 2]
  if (length(Hs) == 0) {
    return(NULL)
  }

  out <- matrix(0, k, length(Hs), dimnames = list(colnames(F_window), paste0("fac_h", Hs)))

  for (q in seq_len(k)) {
    f <- F_window[, q]
    f_sd <- stats::sd(f, na.rm = TRUE)
    if (!is.finite(f_sd) || f_sd <= 0) f_sd <- 1
    for (h in seq_along(Hs)) {
      hh <- min(Hs[h], Tn)
      out[q, h] <- sum(tail(f, hh), na.rm = TRUE) / (f_sd * sqrt(hh) + 1e-8)
    }
  }
  out
}

# ---- ridge helper (deterministic) ----
.came_ridge <- function(X, y, lambda) {
  X <- as.matrix(X)
  y <- as.numeric(y)
  X[!is.finite(X)] <- 0
  y[!is.finite(y)] <- 0

  p <- ncol(X)
  XtX <- crossprod(X) + lambda * diag(p)
  Xty <- crossprod(X, y)

  b <- tryCatch(solve(XtX, Xty), error = function(e) rep(0, p))
  as.numeric(b)
}

# ---- scalarize a family u_{i} in R^{d} to s_i with recursive omega (architecture §7.4) ----
came_scalarize_family <- function(U_mat, y_cs, omega_prev, lambda_omega, ridge_lambda) {
  syms <- rownames(U_mat)
  d <- ncol(U_mat)
  if (d == 0) {
    return(list(s = setNames(rep(0, length(syms)), syms), omega = numeric(0)))
  }

  # cross-sectional standardization of columns
  mu <- colMeans(U_mat)
  sdv <- apply(U_mat, 2, stats::sd)
  sdv[!is.finite(sdv) | sdv <= 1e-8] <- 1

  U <- scale(U_mat, center = mu, scale = sdv)
  U[!is.finite(U)] <- 0

  omega_new <- .came_ridge(U, y_cs, ridge_lambda)
  if (sum(abs(omega_new)) > 0) omega_new <- omega_new / sum(abs(omega_new))

  if (!is.null(omega_prev) && length(omega_prev) == d) {
    omega_new <- lambda_omega * omega_prev + (1 - lambda_omega) * omega_new
    if (sum(abs(omega_new)) > 0) omega_new <- omega_new / sum(abs(omega_new))
  }

  s <- as.vector(U %*% omega_new)
  names(s) <- syms
  list(s = s, omega = omega_new)
}

# ---- scalarize factor horizon vectors across factors (architecture §7.3) ----
.came_scalarize_factor_horizons <- function(g_mat, f_t, omega_prev, lambda_omega, ridge_lambda) {
  # g_mat: k x m horizons (rows=factors), f_t: k vector target (observed after close)
  k <- nrow(g_mat)
  m <- ncol(g_mat)
  if (k < 1 || m < 1) {
    return(list(g_scalar = rep(0, k), omega = omega_prev))
  }

  X <- g_mat
  X[!is.finite(X)] <- 0

  # standardize columns across factors (pooling)
  mu <- colMeans(X)
  sdv <- apply(X, 2, stats::sd)
  sdv[!is.finite(sdv) | sdv <= 1e-8] <- 1
  Xs <- scale(X, center = mu, scale = sdv)
  Xs[!is.finite(Xs)] <- 0

  y <- as.numeric(f_t)
  y[!is.finite(y)] <- 0

  omega_new <- .came_ridge(Xs, y, ridge_lambda)
  if (sum(abs(omega_new)) > 0) omega_new <- omega_new / sum(abs(omega_new))

  if (!is.null(omega_prev) && length(omega_prev) == m) {
    omega_new <- lambda_omega * omega_prev + (1 - lambda_omega) * omega_new
    if (sum(abs(omega_new)) > 0) omega_new <- omega_new / sum(abs(omega_new))
  }

  g_scalar <- as.vector(Xs %*% omega_new)
  list(g_scalar = g_scalar, omega = omega_new)
}

# ---- main signals update ----
came_signals_update <- function(P_last, R_window, risk_art, struct_art, state, spec) {
  came_assert(is.matrix(R_window) && nrow(R_window) >= 5, "signals_R_window", "R_window must be matrix with >=5 rows")
  syms <- colnames(R_window)
  came_assert(!is.null(syms) && length(syms) >= 3, "signals_syms", "R_window must have >=3 symbols")

  # daily sigma for normalization (use EWMA state)
  sigma_daily <- sqrt(came_pi_vector(state$risk$sigma2, syms, init_val = 1e-4))
  sigma_daily[!is.finite(sigma_daily) | sigma_daily <= 0] <- 1e-4

  # (1) raw multiscale momentum
  mom_h <- spec$signals$mom_horizons
  mom_multi <- came_signals_mom_multiscale(
    R_window = R_window,
    sigma_daily = sigma_daily,
    horizons = mom_h,
    scale = spec$signals$mom_scale %||% 2.0,
    prefix = "mom_h"
  )

  # (2) residual multiscale momentum (architecture §7.1): r_perp = D * e
  came_assert(
    !is.null(risk_art$E_std) && !is.null(risk_art$D_series),
    "signals_missing_resid_inputs",
    "risk_art must provide E_std and D_series (required for residual momentum)."
  )

  E <- risk_art$E_std
  D <- risk_art$D_series

  # align residual series to current symbols
  syms2 <- intersect(syms, colnames(E))
  came_assert(length(syms2) >= 3, "signals_resid_common", "Residual series must cover >=3 symbols")

  E <- E[, syms2, drop = FALSE]
  D <- D[, syms2, drop = FALSE]

  R_perp <- E * D
  R_perp[!is.finite(R_perp)] <- 0

  resid_multi <- came_signals_mom_multiscale(
    R_window = R_perp,
    sigma_daily = sigma_daily,
    horizons = mom_h,
    scale = spec$signals$mom_scale %||% 2.0,
    prefix = "resid_mom_h"
  )

  # (3) Kalman features (architecture §7.2)
  qv <- spec$signals$kalman$q_var %||% 1e-5
  rv <- spec$signals$kalman$r_var %||% 1e-3
  if (!is.finite(qv) || qv <= 0) came_stop("signals_kalman_q", "signals$kalman$q_var must be > 0")
  if (!is.finite(rv) || rv <= 0) came_stop("signals_kalman_r", "signals$kalman$r_var must be > 0")

  logp <- log(pmax(P_last[syms], 1e-12))
  kal_prev <- state$signals$kalman
  kal <- came_signals_kalman(logp, kal_prev, q_var = qv, r_var = rv)

  # (4) factor trends (architecture §7.3) and factor-projected continuation via horizon scalarization
  fac_tr <- came_signals_factor_trends(risk_art$F, spec$signals$factor_horizons)
  fac_proj <- setNames(rep(0, length(syms)), syms)
  g_scalar <- NULL

  omega_prev <- state$signals$omega %||% list()
  omega_cfg <- spec$signals$scalarization
  lambda_omega <- omega_cfg$lambda_omega %||% 0.95
  ridge_lambda <- omega_cfg$ridge_lambda %||% 0.05

  if (!is.null(fac_tr)) {
    f_t <- as.numeric(risk_art$F[nrow(risk_art$F), ])
    f_t[!is.finite(f_t)] <- 0

    # scalarize horizons pooled across factors (causal target: observed f_t)
    sc <- .came_scalarize_factor_horizons(
      g_mat = fac_tr,
      f_t = f_t,
      omega_prev = omega_prev$fac_h,
      lambda_omega = lambda_omega,
      ridge_lambda = ridge_lambda
    )
    g_scalar <- sc$g_scalar
    omega_prev$fac_h <- sc$omega

    # project to assets: phi_facproj = B * g_scalar
    B <- risk_art$B[syms, , drop = FALSE]
    fac_proj <- as.vector(B %*% g_scalar)
    names(fac_proj) <- syms
  }

  # (5) scalar signals s^(mom), s^(kal), s^(fac) with recursive omega (architecture §7.4)
  scalar_enabled <- isTRUE(omega_cfg$enabled %||% TRUE)

  # causal cross-sectional target for omega updates:
  # use realized r_{i,t} (last row of R_window), observed after close.
  r_t <- R_window[nrow(R_window), ]
  r_t[!is.finite(r_t)] <- 0
  y_cs <- came_rank01(r_t) - 0.5
  y_cs[!is.finite(y_cs)] <- 0

  s_mom <- setNames(rep(0, length(syms)), syms)
  s_kal <- setNames(rep(0, length(syms)), syms)
  s_fac <- setNames(rep(0, length(syms)), syms)

  omega_out <- list()

  if (scalar_enabled) {
    # momentum family: multiscale features
    U_mom <- mom_multi
    rownames(U_mom) <- syms
    sres <- came_scalarize_family(U_mom, y_cs, omega_prev$mom, lambda_omega, ridge_lambda)
    s_mom <- sres$s
    omega_out$mom <- sres$omega

    # kalman family: slope_z + innov_z + slope_var (variance is informative)
    U_kal <- cbind(
      kal_slope_z = kal$slope_z,
      kal_innov_z = kal$innov_z,
      kal_slope_var = log1p(kal$slope_var)
    )
    rownames(U_kal) <- syms
    sres <- came_scalarize_family(U_kal, y_cs, omega_prev$kal, lambda_omega, ridge_lambda)
    s_kal <- sres$s
    omega_out$kal <- sres$omega

    # factor family: projected continuation (1-d) -> omega is trivial but we keep the same mechanism
    U_fac <- cbind(fac_proj = fac_proj)
    rownames(U_fac) <- syms
    sres <- came_scalarize_family(U_fac, y_cs, omega_prev$fac, lambda_omega, ridge_lambda)
    s_fac <- sres$s
    omega_out$fac <- sres$omega

    # store factor-horizon omega too (pooled across factors)
    if (!is.null(omega_prev$fac_h)) omega_out$fac_h <- omega_prev$fac_h
  } else {
    # if scalarization disabled, expose simple deterministic scalars
    s_mom <- setNames(mom_multi[, 1], syms)
    s_kal <- kal$slope_z
    s_fac <- fac_proj
  }

  # update state
  st_out <- state
  st_out$signals$kalman <- kal$state
  st_out$signals$omega <- omega_out

  list(
    signals = list(
      mom_multi = mom_multi,
      resid_mom_multi = resid_multi,
      kalman = kal,
      factor_trends = fac_tr,
      factor_g_scalar = g_scalar,
      fac_proj = fac_proj,
      s_mom = s_mom,
      s_kal = s_kal,
      s_fac = s_fac
    ),
    state_out = st_out
  )
}
