# 09_optimizer.R — continuous long-only QP with turnover auxiliaries, post-shaping

came_optimizer_controls <- function(m_t, spec) {
  # trimmed deterministic maps (architecture-compatible)
  vov <- m_t["VoV"] %||% 0
  disp <- m_t["disp"] %||% 0

  gamma <- (spec$optimizer$gamma_base %||% 1.0) * exp((spec$optimizer$gamma_vov_scale %||% 1.0) * vov)
  gamma <- pmin(20, pmax(0.1, gamma))

  # turnover cost scalar (linear)
  tau <- (spec$optimizer$turnover_cost_base %||% 5.0) * (1 + (spec$optimizer$tau_disp_scale %||% 0.5) * abs(disp))
  tau <- pmax(0, tau)

  rho_gross <- spec$optimizer$gross_base %||% 0.95
  rho_gross <- pmin(0.99, pmax(0.10, rho_gross))

  list(gamma = gamma, tau = tau, rho_gross = rho_gross)
}

.came_project_simplex_capped <- function(w, target_sum, cap) {
  # simple deterministic projection: clip then renormalize iteratively
  w <- pmax(0, w); w[!is.finite(w)] <- 0
  if (target_sum <= 0) return(w*0)
  cap <- max(cap, 0)
  w <- pmin(w, cap)
  s <- sum(w)
  if (s <= 0) {
    # spread evenly
    n <- length(w)
    w[] <- min(cap, target_sum / n)
    w <- w * (target_sum / sum(w))
    return(w)
  }
  w <- w * (target_sum / s)
  # enforce cap again
  for (iter in 1:50) {
    over <- which(w > cap + 1e-12)
    if (length(over) == 0) break
    excess <- sum(w[over] - cap)
    w[over] <- cap
    under <- which(w < cap - 1e-12)
    if (length(under) == 0) break
    add <- excess * w[under] / sum(w[under])
    w[under] <- w[under] + add
    w <- pmax(0, w)
    w <- w * (target_sum / sum(w))
  }
  w
}

came_optimize_qp <- function(mu_eff, Sigma_H, prev_w, caps, rho_gross, gamma, tau, illiq_z = NULL, strict = TRUE) {
  came_require("quadprog")
  syms <- colnames(Sigma_H)
  n <- length(syms)

  mu <- mu_eff[syms]; mu[!is.finite(mu)] <- 0
  prev <- prev_w[syms]; prev[!is.finite(prev)] <- 0
  cap <- caps[syms]; cap[!is.finite(cap) | cap < 0] <- 0

  # turnover cost per asset: tau * (1 + scale*illiq_z)
  k_turn <- rep(tau, n)
  if (!is.null(illiq_z)) {
    z <- illiq_z[syms]; z[!is.finite(z)] <- 0
    k_turn <- tau * (1 + 0.5 * pmax(z, 0))
  }

  # Variables x = [w (n), d (n)]
  # Objective: maximize mu'w - (gamma/2) w'Sigma w - k_turn' d
  # quadprog solves min 1/2 x'D x - dvec'x
  eps_d <- 1e-8
  Dmat <- matrix(0, 2*n, 2*n)
  Dmat[1:n, 1:n] <- gamma * came_symmetrize(Sigma_H)
  diag(Dmat[(n+1):(2*n), (n+1):(2*n)]) <- eps_d  # make PD

  dvec <- c(mu, -k_turn)

  # Constraints Amat^T x >= bvec
  # Equality: sum(w) = rho_gross => two inequalities: sum(w) >= rho_gross and -sum(w) >= -rho_gross with meq=0 is messy.
  # quadprog supports meq; we set first constraint as equality.
  Amat <- NULL; bvec <- NULL

  # 1) equality sum(w)=rho_gross
  Amat <- cbind(c(rep(1, n), rep(0, n)))
  bvec <- c(rho_gross)

  # 2) w >= 0
  Amat <- cbind(Amat, rbind(diag(n), matrix(0, n, n)))
  bvec <- c(bvec, rep(0, n))

  # 3) w <= cap => -w >= -cap
  Amat <- cbind(Amat, rbind(-diag(n), matrix(0, n, n)))
  bvec <- c(bvec, -cap)

  # 4) d >= 0
  Amat <- cbind(Amat, rbind(matrix(0, n, n), diag(n)))
  bvec <- c(bvec, rep(0, n))

  # 5) d - w >= -prev  (d >= w - prev)
  Amat <- cbind(Amat, rbind(-diag(n), diag(n)))
  bvec <- c(bvec, -prev)

  # 6) d + w >= prev (d >= prev - w)
  Amat <- cbind(Amat, rbind(diag(n), diag(n)))
  bvec <- c(bvec, prev)

  sol <- tryCatch(
    quadprog::solve.QP(Dmat, dvec, Amat, bvec, meq = 1L),
    error = function(e) e
  )
  if (inherits(sol, "error")) {
    if (isTRUE(strict)) stop(came_error("opt_qp_failed", sol$message))
    # minimal legitimate fallback: capped simplex projection of prev weights to rho_gross
    w <- prev
    w <- .came_project_simplex_capped(w, rho_gross, max(cap))
    names(w) <- syms
    return(list(w = w, method = "fallback_prev_projection", obj = NA_real_))
  }

  x <- sol$solution
  w <- x[1:n]
  w[!is.finite(w)] <- 0
  w <- pmax(0, pmin(w, cap))
  w <- .came_project_simplex_capped(w, rho_gross, max(cap))
  names(w) <- syms

  list(w = w, method = "qp", obj = sol$value)
}

came_post_shape <- function(w, rho_gross, cap, drop_thr = 1e-6) {
  w[!is.finite(w)] <- 0
  w[w < drop_thr] <- 0
  w <- pmin(pmax(w, 0), cap)
  if (sum(w) > 0) w <- w * (rho_gross / sum(w))
  w
}
