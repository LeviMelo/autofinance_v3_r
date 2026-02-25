# 01_contracts.R — spec, artifact contracts, state validation

came_spec_default <- function() {
  # Global state vector m_t dimensions (trimmed but architecture-consistent)
  # [disp, eta_mode, VoV, dens, eto, chi, liq_med_logv, liq_med_logntr, liq_frac_active]
  d_m <- 9L

  list(
    data = list(
      min_coverage_ratio = 0.90,
      min_median_traded_value = 1e5,
      allowed_types = c("equity", "fii", "etf", "bdr"),
      eps_hold = 1e-8
    ),
    risk = list(
      lookback = 252L,
      k = 5L,
      lambda_sigma = 0.94,
      lambda_f = 0.97,
      lambda_e = 0.97,
      glasso_lambda = 0.10,
      psd_eps = 1e-8,
      align_factors = TRUE
    ),
    structure = list(
      # smoothing
      lambda_edge = 0.95,
      theta_alpha = c(intercept = -0.2, edge_stab = 1.0, chi = -1.0, xi = -0.2),
      # activation
      k_min = 2L, k_max = 10L,
      beta_w = 1.0,
      q_on_base = 0.90,
      q_on_chi_scale = 0.10,
      hysteresis_ratio = 0.7,
      q_global = 0.995,
      K_min = 2L, K_max = 8L
    ),
    signals = list(
      mom_horizons = c(21L, 63L, 126L, 252L),
      mom_scale = 2.0,
      kalman = list(q_var = 1e-5, r_var = 1e-3),
      factor_horizons = c(21L, 63L),
      scalarization = list(
        enabled = TRUE,
        lambda_omega = 0.95,
        ridge_lambda = 0.05,
        # trimmed change: scalarization target horizon for omega update
        # (architecture suggests matured H; operationally we use 1-day ahead for stability)
        omega_target_horizon = 1L
      )
    ),
    forecast = list(
      H = 21L,
      n_components = 5L,
      ridge_lambda = 0.10,
      ew_lambda = 0.99,
      refit_every = 5L,
      kappa_min = 0.7,
      kappa_max = 1.3,
      lambda_err = 0.97,
      history_keep = 300L
    ),
    gating = list(
      # A_pi: C x d_m, b_pi: C
      A_pi = matrix(0, nrow = 5, ncol = d_m),
      b_pi = rep(0, 5),
      temperature = 1.0
    ),
    reliability = list(
      # deterministic initial coefficients (can be learned later)
      # rho_rel = sigmoid(a0 + a1*liq_z - a2*illiq_z + a3*node_stab - a4*eto - a5*chi)
      a = c(intercept = 0.5, liq_z = 0.6, illiq_z = -0.7, node_stab = 0.5, eto = -0.5, chi = -0.3),
      eps = 1e-6
    ),
    optimizer = list(
      max_weight_base = 0.15,
      max_weight_min = 0.01,
      turnover_cost_base = 5.0,    # linear cost coefficient
      turnover_cost_illiq_scale = 2.0,
      gamma_base = 1.0,
      # state maps (trimmed initial)
      gamma_vov_scale = 1.0,
      tau_disp_scale = 0.5,
      gross_base = 0.95
    ),
    meta = list(
      strict = TRUE,
      retain_debug = FALSE
    )
  )
}

came_spec_validate <- function(spec) {
  req <- c("data","risk","structure","signals","forecast","gating","optimizer","meta","reliability")
  miss <- setdiff(req, names(spec))
  came_assert(length(miss) == 0, "spec_missing_sections",
              paste("Missing sections:", paste(miss, collapse=", ")))

  came_assert(is.matrix(spec$gating$A_pi), "spec_gating_A_pi", "gating$A_pi must be a matrix")
  came_assert(length(spec$gating$b_pi) == nrow(spec$gating$A_pi), "spec_gating_b_pi",
              "gating$b_pi length must match nrow(A_pi)")
  came_assert(nrow(spec$gating$A_pi) == spec$forecast$n_components, "spec_components",
              "n_components must match gating matrix rows")

  invisible(spec)
}

came_state_validate <- function(state) {
  if (is.null(state)) return(invisible(NULL))
  came_assert(is.list(state), "state_type", "state must be a list or NULL")

  if (!is.null(state$prev_target)) {
    pt <- state$prev_target
    came_assert(is.data.frame(pt) && all(c("symbol","weight_target") %in% names(pt)),
                "state_prev_target", "prev_target must be data.frame(symbol, weight_target)")
  }

  # key recursive fields (optional)
  if (!is.null(state$risk$sigma2)) came_assert(is.numeric(state$risk$sigma2), "state_sigma2", "risk$sigma2 must be numeric")
  if (!is.null(state$risk$Sigma_f)) came_assert(is.matrix(state$risk$Sigma_f), "state_Sigma_f", "risk$Sigma_f must be matrix")
  if (!is.null(state$risk$S_e)) came_assert(is.matrix(state$risk$S_e), "state_S_e", "risk$S_e must be matrix")
  if (!is.null(state$risk$B_prev)) came_assert(is.matrix(state$risk$B_prev), "state_B_prev", "risk$B_prev must be matrix")

  if (!is.null(state$structure$P_bar)) came_assert(is.matrix(state$structure$P_bar), "state_P_bar", "structure$P_bar must be matrix")
  if (!is.null(state$structure$M_prev)) came_assert(is.matrix(state$structure$M_prev), "state_M_prev", "structure$M_prev must be matrix")
  if (!is.null(state$structure$edge_stab)) came_assert(is.matrix(state$structure$edge_stab), "state_edge_stab", "structure$edge_stab must be matrix")
  if (!is.null(state$structure$labels)) came_assert(is.integer(state$structure$labels) || is.numeric(state$structure$labels),
                                                    "state_labels", "structure$labels must be integer-like")

  if (!is.null(state$signals$kalman)) came_assert(is.list(state$signals$kalman), "state_kalman", "signals$kalman must be list")
  if (!is.null(state$signals$omega)) came_assert(is.list(state$signals$omega), "state_omega", "signals$omega must be list")

  if (!is.null(state$forecast$models)) came_assert(is.list(state$forecast$models), "state_models", "forecast$models must be list")
  if (!is.null(state$forecast$error_buckets)) came_assert(is.list(state$forecast$error_buckets), "state_err", "forecast$error_buckets must be list")
  if (!is.null(state$forecast$hist)) came_assert(is.list(state$forecast$hist), "state_fhist", "forecast$hist must be list")

  if (!is.null(state$history)) {
    h <- state$history
    came_assert(is.list(h$snapshots), "state_hist_snapshots", "history$snapshots must be list")
  }

  invisible(state)
}

came_snapshot_validate <- function(x) {
  req <- c("as_of_date","universe","weights","cash_weight","risk","structure","signals","features","forecast","optimizer","meta","state_out")
  miss <- setdiff(req, names(x))
  came_assert(length(miss) == 0, "snapshot_missing", paste("Missing fields:", paste(miss, collapse=", ")))

  wdf <- x$weights
  came_assert(is.data.frame(wdf) && all(c("symbol","weight_target") %in% names(wdf)),
              "snapshot_weights", "weights must be data.frame(symbol, weight_target)")

  tot <- sum(wdf$weight_target, na.rm = TRUE) + x$cash_weight
  came_assert(is.finite(tot) && abs(tot - 1.0) <= 1e-6, "snapshot_budget", "weights must sum to 1")

  Sigma <- x$risk$Sigma_H
  came_assert_square_named_matrix(Sigma, "snapshot_sigma", "risk$Sigma_H must be named square covariance")

  invisible(x)
}
