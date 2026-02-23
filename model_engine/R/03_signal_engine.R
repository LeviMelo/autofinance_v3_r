#' @title Signal Engine
#' @description Implement trend experts and score normalization in one place.

.tanh_scale <- function(x, scale = 1.0) {
  tanh(x / scale)
}

#' @export
me_signal_kalman <- function(prices_window, sigma_t, spec_kalman) {
  # 2-state local linear trend Kalman filter on log-prices
  # State x_t = [level_t, slope_t]'
  # level_t = level_{t-1} + slope_{t-1} + w1_t
  # slope_t = slope_{t-1} + w2_t
  # y_t = level_t + v_t

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

  # Correct local linear trend transition / observation matrices
  F_mat <- matrix(c(
    1, 1,
    0, 1
  ), nrow = 2, byrow = TRUE)
  H_mat <- matrix(c(1, 0), nrow = 1)

  Q <- diag(c(q_var, q_var))
  R_obs <- r_var

  I2 <- diag(2)

  for (j in seq_len(n_assets)) {
    y_raw <- prices_window[, j]

    # Require finite positive prices before log
    valid <- is.finite(y_raw) & !is.na(y_raw) & (y_raw > 0)
    if (sum(valid) < 10) next

    y <- log(y_raw[valid])

    # Initialize state
    x_hat <- matrix(c(y[1], 0), nrow = 2)
    P <- diag(c(1, 1))

    for (i in 2:length(y)) {
      # Predict
      x_pred <- F_mat %*% x_hat
      P_pred <- F_mat %*% P %*% t(F_mat) + Q

      # Update
      y_hat <- (H_mat %*% x_pred)[1, 1]
      err <- y[i] - y_hat
      S <- (H_mat %*% P_pred %*% t(H_mat))[1, 1] + R_obs

      if (!is.finite(S) || S <= 0) next

      K <- P_pred %*% t(H_mat) / S
      x_hat <- x_pred + K * err

      # Joseph-form covariance update (more numerically stable)
      KH <- K %*% H_mat
      P <- (I2 - KH) %*% P_pred %*% t(I2 - KH) + K %*% matrix(R_obs, 1, 1) %*% t(K)
    }

    # Final latent slope is a daily log-return-like increment
    slope_daily <- x_hat[2, 1]
    if (!is.finite(slope_daily)) next

    # Convert to annualized slope to match annualized sigma_t
    slope_ann <- slope_daily * 252

    sym_j <- colnames(prices_window)[j]
    vol_j <- sigma_t[sym_j]
    if (length(vol_j) != 1 || !is.finite(vol_j) || vol_j <= 0) {
      vol_j <- median(sigma_t[is.finite(sigma_t) & sigma_t > 0], na.rm = TRUE)
      if (!is.finite(vol_j) || vol_j <= 0) vol_j <- 0.2
    }

    scores[j] <- slope_ann / vol_j
  }

  .tanh_scale(scores, out_scale)
}

#' @export
me_signal_tsmom <- function(R_window, sigma_t, spec_tsmom) {
  # Defensive checks
  if (is.null(dim(R_window)) || ncol(R_window) == 0) {
    return(setNames(numeric(0), character(0)))
  }
  if (is.null(colnames(R_window))) {
    stop("me_signal_tsmom: R_window must have colnames for symbol alignment.")
  }
  if (is.null(names(sigma_t))) {
    stop("me_signal_tsmom: sigma_t must be a named numeric vector.")
  }

  h <- spec_tsmom$horizon %||% 252L
  h <- min(h, nrow(R_window))

  if (h < 5) {
    v <- rep(0, ncol(R_window))
    names(v) <- colnames(R_window)
    return(v)
  }

  # ---- CRITICAL FIX: align sigma_t to returns columns by symbol ----
  syms <- colnames(R_window)
  sigma_aligned <- sigma_t[syms]

  # Replace missing / invalid vols with robust fallback (median positive vol)
  good_sigma <- sigma_t[is.finite(sigma_t) & !is.na(sigma_t) & sigma_t > 0]
  sigma_fallback <- stats::median(good_sigma, na.rm = TRUE)
  if (!is.finite(sigma_fallback) || sigma_fallback <= 0) sigma_fallback <- 0.2

  bad_sigma <- !is.finite(sigma_aligned) | is.na(sigma_aligned) | sigma_aligned <= 0
  if (any(bad_sigma)) {
    sigma_aligned[bad_sigma] <- sigma_fallback
    warning(sprintf(
      "TSMOM sigma alignment: replaced %d invalid/missing sigma values with fallback %.6f.",
      sum(bad_sigma), sigma_fallback
    ), call. = FALSE)
  }

  # Use colSums to preserve exact 1:1 vector length with columns
  ret_cum <- colSums(tail(R_window, h), na.rm = TRUE)

  denom <- sigma_aligned * sqrt(h / 252)
  bad_denom <- !is.finite(denom) | is.na(denom) | denom <= 0
  if (any(bad_denom)) {
    denom[bad_denom] <- sigma_fallback * sqrt(h / 252)
    warning(sprintf(
      "TSMOM denominator: replaced %d invalid denominators.",
      sum(bad_denom)
    ), call. = FALSE)
  }

  # Now lengths and names are guaranteed aligned (no vector recycling)
  scores_raw <- ret_cum / denom
  scores_raw[!is.finite(scores_raw) | is.na(scores_raw)] <- 0

  .tanh_scale(scores_raw, spec_tsmom$scale %||% 2.0)
}

#' @export
me_align_signal_vectors <- function(...) {
  lst <- list(...)
  syms <- unique(unlist(lapply(lst, names)))

  aligned <- lapply(lst, function(v) {
    res <- rep(0, length(syms))
    names(res) <- syms
    idx <- intersect(names(v), syms)
    res[idx] <- v[idx]
    res
  })
  aligned
}

#' @export
me_run_signal_engine <- function(prices_window, R_window, sigma_t, spec_signals) {
  # Defensive checks
  if (is.null(dim(prices_window)) || is.null(dim(R_window))) {
    stop("me_run_signal_engine: prices_window and R_window must be matrices.")
  }
  if (is.null(colnames(prices_window)) || is.null(colnames(R_window))) {
    stop("me_run_signal_engine: input matrices must have colnames.")
  }
  if (is.null(names(sigma_t))) {
    stop("me_run_signal_engine: sigma_t must be a named numeric vector.")
  }

  # ---- CRITICAL FIX: canonical signal universe alignment ----
  common_syms <- Reduce(intersect, list(colnames(prices_window), colnames(R_window), names(sigma_t)))
  common_syms <- unique(common_syms)

  if (length(common_syms) == 0) {
    stop("Signal engine has zero common symbols across prices_window, R_window, and sigma_t.")
  }

  n_price_only <- length(setdiff(colnames(prices_window), common_syms))
  n_ret_only <- length(setdiff(colnames(R_window), common_syms))
  n_sigma_only <- length(setdiff(names(sigma_t), common_syms))

  if (n_price_only > 0 || n_ret_only > 0 || n_sigma_only > 0) {
    warning(sprintf(
      paste0(
        "Signal universe alignment dropped symbols: ",
        "prices_only=%d, returns_only=%d, sigma_only=%d; using common=%d."
      ),
      n_price_only, n_ret_only, n_sigma_only, length(common_syms)
    ), call. = FALSE)
  }

  P_use <- prices_window[, common_syms, drop = FALSE]
  R_use <- R_window[, common_syms, drop = FALSE]
  sigma_use <- sigma_t[common_syms]

  kalman_scores <- me_signal_kalman(P_use, sigma_use, spec_signals$kalman)
  tsmom_scores <- me_signal_tsmom(R_use, sigma_use, spec_signals$tsmom)

  aligned <- me_align_signal_vectors(kalman = kalman_scores, tsmom = tsmom_scores)

  list(
    kalman = aligned$kalman,
    tsmom = aligned$tsmom,
    diag = list(
      kalman_coverage = sum(aligned$kalman != 0),
      tsmom_coverage = sum(aligned$tsmom != 0),
      n_common_assets = length(common_syms),
      n_price_assets = ncol(prices_window),
      n_return_assets = ncol(R_window),
      n_sigma_assets = length(sigma_t)
    )
  )
}
