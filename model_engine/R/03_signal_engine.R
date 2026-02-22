#' @title Signal Engine
#' @description Implement trend experts and score normalization in one place.

.tanh_scale <- function(x, scale = 1.0) {
  tanh(x / scale)
}

#' @export
me_signal_kalman <- function(prices_window, sigma_t, spec_kalman) {
  # 2-state Kalman filter (Level and Slope)
  # State x_t = [l_t, b_t]'
  # l_t = l_{t-1} + b_{t-1} + w_{l,t}
  # b_t = b_{t-1} + w_{b,t}
  # y_t = l_t + v_t

  n_assets <- ncol(prices_window)
  T <- nrow(prices_window)
  scores <- rep(0, n_assets)
  names(scores) <- colnames(prices_window)

  if (T < 10) {
    return(scores)
  }

  q_var <- spec_kalman$q_var %||% 1e-4
  r_var <- spec_kalman$r_var %||% 1e-2

  # Transition Matrix
  F_mat <- matrix(c(1, 0, 1, 1), nrow = 2, byrow = TRUE)

  # Observation Matrix
  H_mat <- matrix(c(1, 0), nrow = 1)

  for (j in seq_len(n_assets)) {
    y <- log(prices_window[, j]) # Use log-prices
    valid <- !is.na(y)
    if (sum(valid) < 10) next
    y <- y[valid]

    # Init state
    x_hat <- matrix(c(y[1], 0), nrow = 2)
    P <- diag(c(1, 1))

    # Process Noise
    Q <- diag(c(q_var, q_var))
    # Obs Noise
    R <- r_var

    for (i in 2:length(y)) {
      # Predict
      x_pred <- F_mat %*% x_hat
      P_pred <- F_mat %*% P %*% t(F_mat) + Q

      # Update
      err <- y[i] - (H_mat %*% x_pred)[1, 1]
      S <- (H_mat %*% P_pred %*% t(H_mat))[1, 1] + R
      K <- P_pred %*% t(H_mat) / S

      x_hat <- x_pred + K * err
      P <- (diag(2) - K %*% H_mat) %*% P_pred
    }

    # Extract final slope estimate
    slope <- x_hat[2, 1]

    # Convert slope to daily return scale conceptually, then scale by asset vol
    # vol is annualized. To normalize, compare annualized slope to annual vol
    slope_ann <- slope * 252
    vol_j <- sigma_t[names(sigma_t) == colnames(prices_window)[j]] %||% 0.2
    if (vol_j == 0) vol_j <- 0.2

    scores[j] <- slope_ann / vol_j
  }

  .tanh_scale(scores, spec_kalman$scale %||% 1.0)
}

#' @export
me_signal_tsmom <- function(R_window, sigma_t, spec_tsmom) {
  h <- spec_tsmom$horizon %||% 252
  h <- min(h, nrow(R_window))

  if (h < 5) {
    return(rep(0, ncol(R_window)))
  }

  ret_cum <- apply(tail(R_window, h), 2, sum, na.rm = TRUE)

  scores_raw <- ret_cum / (sigma_t * sqrt(h / 252))
  scores_raw[is.na(scores_raw) | is.infinite(scores_raw)] <- 0

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
  kalman_scores <- me_signal_kalman(prices_window, sigma_t, spec_signals$kalman)
  tsmom_scores <- me_signal_tsmom(R_window, sigma_t, spec_signals$tsmom)

  aligned <- me_align_signal_vectors(kalman = kalman_scores, tsmom = tsmom_scores)

  list(
    kalman = aligned$kalman, tsmom = aligned$tsmom,
    diag = list(
      kalman_coverage = sum(aligned$kalman != 0),
      tsmom_coverage = sum(aligned$tsmom != 0)
    )
  )
}
