#' @title Model Engine — Signal Engine
#' @description Trend expert signals: Kalman filter, TSMOM (raw + residual).

# ── Internal helpers ──────────────────────────────────────────────────────────

.tanh_scale <- function(x, scale = 1.0) tanh(x / scale)

# ── Kalman filter signal ──────────────────────────────────────────────────────

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

  # 2-state local linear trend: [level, slope]
  F_mat <- matrix(c(1, 0, 1, 1), nrow = 2, byrow = FALSE)
  H_mat <- matrix(c(1, 0), nrow = 1)
  Q <- diag(c(q_var, q_var))
  I2 <- diag(2)

  for (j in seq_len(n_assets)) {
    y_raw <- prices_window[, j]
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
      S <- (H_mat %*% P_pred %*% t(H_mat))[1, 1] + r_var
      if (!is.finite(S) || S <= 0) next

      K <- P_pred %*% t(H_mat) / S
      x_hat <- x_pred + K * err

      # Joseph-form covariance update
      KH <- K %*% H_mat
      P <- (I2 - KH) %*% P_pred %*% t(I2 - KH) + K %*% matrix(r_var, 1, 1) %*% t(K)
    }

    slope_daily <- x_hat[2, 1]
    if (!is.finite(slope_daily)) next

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

# ── TSMOM signal (raw returns) ────────────────────────────────────────────────

#' @export
me_signal_tsmom <- function(R_window, sigma_t, spec_tsmom) {
  if (is.null(dim(R_window)) || ncol(R_window) == 0) {
    return(setNames(numeric(0), character(0)))
  }
  if (is.null(colnames(R_window))) {
    stop("me_signal_tsmom: R_window must have colnames")
  }
  if (is.null(names(sigma_t))) {
    stop("me_signal_tsmom: sigma_t must be named")
  }

  h <- spec_tsmom$horizon %||% 252L
  h <- min(h, nrow(R_window))
  out_scale <- spec_tsmom$scale %||% 2.0

  if (h < 5) {
    v <- rep(0, ncol(R_window))
    names(v) <- colnames(R_window)
    return(v)
  }

  # Align sigma to returns columns
  syms <- colnames(R_window)
  sigma_aligned <- sigma_t[syms]

  good_sigma <- sigma_t[is.finite(sigma_t) & !is.na(sigma_t) & sigma_t > 0]
  sigma_fb <- stats::median(good_sigma, na.rm = TRUE)
  if (!is.finite(sigma_fb) || sigma_fb <= 0) sigma_fb <- 0.2

  bad <- !is.finite(sigma_aligned) | is.na(sigma_aligned) | sigma_aligned <= 0
  if (any(bad)) sigma_aligned[bad] <- sigma_fb

  ret_cum <- colSums(tail(R_window, h), na.rm = TRUE)
  denom <- sigma_aligned * sqrt(h / 252)

  bad_d <- !is.finite(denom) | denom <= 0
  if (any(bad_d)) denom[bad_d] <- sigma_fb * sqrt(h / 252)

  scores <- ret_cum / denom
  scores[!is.finite(scores)] <- 0

  .tanh_scale(scores, out_scale)
}

# ── Residual TSMOM signal ─────────────────────────────────────────────────────

#' @export
me_signal_residual_tsmom <- function(E_window, sigma_t, spec_tsmom) {
  # Same logic as raw TSMOM but applied to PCA residuals
  me_signal_tsmom(E_window, sigma_t, spec_tsmom)
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
    res <- rep(0, length(syms))
    names(res) <- syms
    idx <- intersect(names(v), syms)
    res[idx] <- v[idx]
    res
  })
  aligned
}

# ── Full signal engine orchestrator ───────────────────────────────────────────

#' @export
me_run_signal_engine <- function(prices_window, R_window, sigma_t,
                                 spec_signals, E_window = NULL) {
  if (is.null(dim(prices_window)) || is.null(dim(R_window))) {
    stop("prices_window and R_window must be matrices")
  }
  if (is.null(colnames(prices_window)) || is.null(colnames(R_window))) {
    stop("Input matrices must have colnames")
  }
  if (is.null(names(sigma_t))) {
    stop("sigma_t must be named")
  }

  # Canonical signal universe = intersection
  common <- Reduce(intersect, list(
    colnames(prices_window),
    colnames(R_window),
    names(sigma_t)
  ))
  if (length(common) == 0) stop("Signal engine: zero common symbols")

  P_use <- prices_window[, common, drop = FALSE]
  R_use <- R_window[, common, drop = FALSE]
  sigma_use <- sigma_t[common]

  kalman_scores <- me_signal_kalman(P_use, sigma_use, spec_signals$kalman)
  tsmom_scores <- me_signal_tsmom(R_use, sigma_use, spec_signals$tsmom)

  # Residual TSMOM if residuals provided
  res_tsmom_scores <- NULL
  if (!is.null(E_window) && ncol(E_window) > 0) {
    e_common <- intersect(common, colnames(E_window))
    if (length(e_common) > 0) {
      res_tsmom_scores <- me_signal_residual_tsmom(
        E_window[, e_common, drop = FALSE],
        sigma_use[e_common],
        spec_signals$tsmom
      )
    }
  }

  # Align all signals to union universe
  sig_list <- list(kalman = kalman_scores, tsmom = tsmom_scores)
  if (!is.null(res_tsmom_scores)) sig_list$res_tsmom <- res_tsmom_scores
  aligned <- do.call(me_align_signal_vectors, sig_list)

  result <- list(
    kalman = aligned$kalman,
    tsmom = aligned$tsmom,
    diag = list(
      kalman_coverage   = sum(aligned$kalman != 0),
      tsmom_coverage    = sum(aligned$tsmom != 0),
      n_common          = length(common),
      n_price_assets    = ncol(prices_window),
      n_return_assets   = ncol(R_window),
      n_sigma_assets    = length(sigma_t),
      kalman_all_zero   = all(aligned$kalman == 0),
      tsmom_all_zero    = all(aligned$tsmom == 0)
    )
  )

  if (!is.null(res_tsmom_scores)) {
    result$res_tsmom <- aligned$res_tsmom
    result$diag$res_tsmom_coverage <- sum(aligned$res_tsmom != 0)
  }

  result
}
