# 09_optimizer.R — continuous long-only QP + portfolio wrapper with tradability/frozen carry

came_optimizer_controls <- function(m_t, spec) {
  vov <- m_t["VoV"] %||% 0
  disp <- m_t["disp"] %||% 0

  gamma <- (spec$optimizer$gamma_base %||% 1.0) * exp((spec$optimizer$gamma_vov_scale %||% 1.0) * vov)
  gamma <- pmin(20, pmax(0.1, gamma))

  tau <- (spec$optimizer$turnover_cost_base %||% 5.0) * (1 + (spec$optimizer$tau_disp_scale %||% 0.5) * abs(disp))
  tau <- pmax(0, tau)

  rho_gross <- spec$optimizer$gross_base %||% 0.95
  rho_gross <- pmin(0.99, pmax(0.10, rho_gross))

  list(gamma = gamma, tau = tau, rho_gross = rho_gross)
}

.came_project_simplex_capped <- function(w, target_sum, cap) {
  w <- pmax(0, w)
  w[!is.finite(w)] <- 0
  if (!is.finite(target_sum) || target_sum <= 0) {
    return(w * 0)
  }

  cap <- max(cap, 0)
  w <- pmin(w, cap)

  s <- sum(w)
  if (s <= 0) {
    n <- length(w)
    w[] <- min(cap, target_sum / n)
    if (sum(w) > 0) w <- w * (target_sum / sum(w))
    return(w)
  }

  w <- w * (target_sum / s)

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
    if (sum(w) > 0) w <- w * (target_sum / sum(w))
  }
  w
}

came_post_shape <- function(w, target_sum, cap, drop_thr = 1e-6) {
  w[!is.finite(w)] <- 0
  w[w < drop_thr] <- 0
  w <- pmin(pmax(w, 0), cap)
  if (sum(w) > 0 && target_sum > 0) w <- w * (target_sum / sum(w))
  w
}

came_optimize_qp <- function(mu_eff, Sigma_H, prev_w, caps, rho_gross, gamma, tau,
                             illiq_z = NULL, turnover_illiq_scale = 2.0, strict = TRUE) {
  came_require("quadprog")
  came_assert(is.matrix(Sigma_H), "opt_Sigma_type", "Sigma_H must be a matrix")
  came_assert(nrow(Sigma_H) == ncol(Sigma_H), "opt_Sigma_square", "Sigma_H must be square")

  syms <- colnames(Sigma_H)
  came_assert(
    !is.null(syms) && identical(syms, rownames(Sigma_H)),
    "opt_Sigma_names", "Sigma_H must have identical row/col names"
  )

  n <- length(syms)

  if (!is.finite(rho_gross) || rho_gross < 0) rho_gross <- 0
  if (rho_gross == 0 || n == 0) {
    return(list(w = setNames(rep(0, n), syms), method = "trivial_zero", obj = NA_real_))
  }

  mu <- mu_eff[syms]
  mu[!is.finite(mu)] <- 0
  prev <- prev_w[syms]
  prev[!is.finite(prev)] <- 0
  cap <- caps[syms]
  cap[!is.finite(cap) | cap < 0] <- 0

  # turnover cost per asset
  k_turn <- rep(tau, n)
  if (!is.null(illiq_z)) {
    z <- illiq_z[syms]
    z[!is.finite(z)] <- 0
    scale_illiq <- turnover_illiq_scale
    if (!is.finite(scale_illiq) || scale_illiq < 0) scale_illiq <- 2.0
    k_turn <- tau * (1 + 0.5 * scale_illiq * pmax(z, 0))
  }

  # Variables x = [w (n), d (n)]
  eps_d <- 1e-8
  eps_w <- 1e-10

  Dmat <- matrix(0, 2 * n, 2 * n)
  Ssym <- came_symmetrize(Sigma_H)
  Dmat[1:n, 1:n] <- gamma * Ssym + eps_w * diag(n)

  idx <- (n + 1):(2 * n)
  Dmat[cbind(idx, idx)] <- rep(eps_d, n)

  dvec <- c(mu, -k_turn)

  # Constraints Amat^T x >= bvec
  # meq=1 => first constraint equality: sum(w)=rho_gross
  Amat <- cbind(c(rep(1, n), rep(0, n)))
  bvec <- c(rho_gross)

  # w >= 0
  Amat <- cbind(Amat, rbind(diag(n), matrix(0, n, n)))
  bvec <- c(bvec, rep(0, n))

  # w <= cap  => -w >= -cap
  Amat <- cbind(Amat, rbind(-diag(n), matrix(0, n, n)))
  bvec <- c(bvec, -cap)

  # d >= 0
  Amat <- cbind(Amat, rbind(matrix(0, n, n), diag(n)))
  bvec <- c(bvec, rep(0, n))

  # d >= w - prev  => d - w >= -prev
  Amat <- cbind(Amat, rbind(-diag(n), diag(n)))
  bvec <- c(bvec, -prev)

  # d >= prev - w  => d + w >= prev
  Amat <- cbind(Amat, rbind(diag(n), diag(n)))
  bvec <- c(bvec, prev)

  sol <- tryCatch(
    quadprog::solve.QP(Dmat, dvec, Amat, bvec, meq = 1L),
    error = function(e) e
  )

  if (inherits(sol, "error")) {
    if (isTRUE(strict)) stop(came_error("opt_qp_failed", sol$message))
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

came_optimize_portfolio <- function(mu_eff, Sigma_H, prev_w, caps, a_t,
                                    rho_gross, gamma, tau,
                                    illiq_z = NULL, turnover_illiq_scale = 2.0,
                                    eps_hold = 1e-8, drop_thr = 1e-6,
                                    strict = TRUE) {
  came_assert(is.matrix(Sigma_H), "opt_port_Sigma", "Sigma_H must be matrix")
  syms <- colnames(Sigma_H)
  came_assert(
    !is.null(syms) && identical(syms, rownames(Sigma_H)),
    "opt_port_Sigma_names", "Sigma_H must have identical row/col names"
  )

  # align vectors
  mu <- mu_eff[syms]
  mu[!is.finite(mu)] <- 0
  cap <- caps[syms]
  cap[!is.finite(cap) | cap < 0] <- 0
  prev <- prev_w[syms]
  prev[!is.finite(prev)] <- 0
  a <- a_t[syms]
  came_assert(!any(is.na(a)), "opt_port_a_t", "a_t must be named logical for all symbols")

  frozen <- syms[(!a) & (prev > (eps_hold %||% 1e-8))]
  fixed_w <- prev[frozen]
  fixed_w[!is.finite(fixed_w)] <- 0

  if (isTRUE(strict)) {
    if (length(frozen) > 0) {
      overcap <- frozen[fixed_w > cap[frozen] + 1e-12]
      came_assert(
        length(overcap) == 0, "opt_port_frozen_overcap",
        paste("Frozen names exceed cap:", paste(overcap, collapse = ", "))
      )
    }
  }

  fixed_sum <- sum(fixed_w)

  # If frozen carry already exceeds rho_gross, strict must error.
  # Non-strict: accept reality; set effective gross to fixed_sum.
  rho_eff <- rho_gross
  if (!is.finite(rho_eff) || rho_eff < 0) rho_eff <- 0

  if (fixed_sum > rho_eff + 1e-12) {
    if (isTRUE(strict)) {
      stop(came_error(
        "opt_port_fixed_exceeds_gross",
        sprintf("Frozen carry sum %.6f exceeds rho_gross %.6f", fixed_sum, rho_eff)
      ))
    } else {
      rho_eff <- fixed_sum
    }
  }

  free <- syms[a]
  rho_free <- max(0, rho_eff - fixed_sum)

  w_all <- setNames(rep(0, length(syms)), syms)
  if (length(frozen) > 0) w_all[frozen] <- fixed_w

  method <- "frozen_only"
  qp_diag <- NULL

  if (rho_free > 0) {
    came_assert(
      length(free) >= 1, "opt_port_no_tradables",
      "rho_free > 0 but no tradable assets available"
    )

    # Incorporate cross-covariance with fixed weights:
    # objective includes - (gamma) w_free' Sigma_fF w_fixed as linear term
    mu_adj <- mu[free]
    if (length(frozen) > 0) {
      cross <- Sigma_H[free, frozen, drop = FALSE] %*% fixed_w
      cross <- as.numeric(cross)
      names(cross) <- free
      mu_adj <- mu_adj - gamma * cross
    }

    Sigma_ff <- Sigma_H[free, free, drop = FALSE]
    prev_free <- prev[free]
    cap_free <- cap[free]
    illiq_free <- if (!is.null(illiq_z)) illiq_z[free] else NULL

    sol <- came_optimize_qp(
      mu_eff = mu_adj,
      Sigma_H = Sigma_ff,
      prev_w = prev_free,
      caps = cap_free,
      rho_gross = rho_free,
      gamma = gamma,
      tau = tau,
      illiq_z = illiq_free,
      turnover_illiq_scale = turnover_illiq_scale,
      strict = strict
    )

    w_free <- sol$w
    w_free <- came_post_shape(w_free, rho_free, cap_free, drop_thr = drop_thr)
    w_all[free] <- w_free

    method <- paste0("portfolio_", sol$method)
    qp_diag <- list(
      rho_eff = rho_eff, rho_free = rho_free, fixed_sum = fixed_sum,
      n_free = length(free), n_frozen = length(frozen)
    )
  }

  risky_sum <- sum(w_all)
  cash <- 1 - risky_sum
  if (!is.finite(cash)) cash <- 0
  if (cash < -1e-8 && isTRUE(strict)) {
    stop(came_error("opt_port_negative_cash", sprintf("cash=%.6f (sum weights=%.6f)", cash, risky_sum)))
  }
  cash <- max(0, cash)

  list(
    w = w_all,
    cash = cash,
    frozen = frozen,
    free = free,
    method = method,
    diag = qp_diag
  )
}
