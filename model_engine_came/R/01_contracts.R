# 01_contracts.R — spec defaults + validation, state/snapshot contracts
# Aligned to architecture.md: strict causality, Glasso-only residual precision,
# graph operators distinct, component mixture + kappa + reliability + QP optimizer.

came_spec_default <- function() {
  # Global state vector m_t dimension (architecture §11.5.5)
  # [disp, eta_mode, VoV, dens, eto, chi, liq_med_logv, liq_med_logntr, liq_frac_active]
  d_m <- 9L

  list(
    data = list(
      min_coverage_ratio = 0.90,
      min_median_traded_value = 1e5,
      allowed_types = c("equity", "fii", "etf", "bdr"),
      eps_hold = 1e-8
    ),

    # ---- risk (architecture §5) ----
    risk = list(
      lookback = 252L,
      k = 5L,
      lambda_sigma = 0.94,
      lambda_f = 0.97,
      lambda_e = 0.97,
      vov_lambda = 0.97,
      glasso_lambda = 0.10,
      psd_eps = 1e-8,
      align_factors = TRUE
    ),

    # ---- structure (architecture §6 + §9 + §10) ----
    structure = list(
      # §6.2 smoothing
      lambda_edge = 0.95,
      theta_alpha = c(intercept = -0.2, edge_stab = 1.0, chi = -1.0, xi = -0.2),

      # §6.3 activation / persistence / degree targeting (operationally trimmed but real)
      k_min = 2L,
      k_max = 10L,
      beta_w = 1.0,
      q_on_base = 0.90,
      q_on_chi_scale = 0.10,
      hysteresis_ratio = 0.7,
      q_global = 0.995,

      # optional density target (for diagnostics; may be used later for delta_t)
      dens_target = 0.05,

      # clustering bounds
      K_min = 2L,
      K_max = 8L
    ),

    # ---- signals (architecture §7) ----
    signals = list(
      mom_horizons = c(21L, 63L, 126L, 252L),
      mom_scale = 2.0,
      kalman = list(
        q_var = 1e-5,
        r_var = 1e-3
      ),
      factor_horizons = c(21L, 63L),
      scalarization = list(
        enabled = TRUE,
        lambda_omega = 0.95,
        ridge_lambda = 0.05,
        # architecture suggests matured H; operational trim allowed but must stay causal
        omega_target_horizon = 1L
      )
    ),

    # ---- forecast (architecture §12) ----
    forecast = list(
      H = 21L,
      n_components = 5L,
      ridge_lambda = 0.10,
      ew_lambda = 0.99,
      refit_every = 5L,
      kappa_min = 0.7,
      kappa_max = 1.3,
      lambda_err = 0.97,
      history_keep = 300L,

      # strict cold-start policy: "error" or "skip"
      # "skip" must be handled explicitly by runner (not silently)
      cold_start_policy = "error"
    ),

    # ---- gating (architecture §12.4) ----
    gating = list(
      A_pi = matrix(0, nrow = 5, ncol = d_m),
      b_pi = rep(0, 5),
      temperature = 1.0
    ),

    # ---- reliability (architecture §12.8) ----
    reliability = list(
      # rho_rel = sigmoid(a0 + a1*liq_z + a2*illiq_z + a3*node_stab + a4*eto + a5*chi)
      # (signs are up to calibration; keep deterministic initial)
      a = c(intercept = 0.5, liq_z = 0.6, illiq_z = -0.7, node_stab = 0.5, eto = -0.5, chi = -0.3),
      eps = 1e-6
    ),

    # ---- optimizer controls (architecture §13) + core QP (architecture §14) ----
    optimizer = list(
      max_weight_base = 0.15,
      max_weight_min = 0.01,
      turnover_cost_base = 5.0,
      turnover_cost_illiq_scale = 2.0,
      gamma_base = 1.0,
      gamma_vov_scale = 1.0,
      tau_disp_scale = 0.5,
      gross_base = 0.95,

      # post-shaping (architecture §14.3)
      drop_thr = 1e-6
    ),
    meta = list(
      strict = TRUE,
      retain_debug = FALSE
    )
  )
}

came_spec_validate <- function(spec) {
  req <- c("data", "risk", "structure", "signals", "forecast", "gating", "optimizer", "meta", "reliability")
  miss <- setdiff(req, names(spec))
  came_assert(
    length(miss) == 0, "spec_missing_sections",
    paste("Missing sections:", paste(miss, collapse = ", "))
  )

  # gating dims
  came_assert(is.matrix(spec$gating$A_pi), "spec_gating_A_pi", "gating$A_pi must be a matrix")
  came_assert(
    length(spec$gating$b_pi) == nrow(spec$gating$A_pi), "spec_gating_b_pi",
    "gating$b_pi length must match nrow(A_pi)"
  )
  came_assert(
    nrow(spec$gating$A_pi) == as.integer(spec$forecast$n_components), "spec_components",
    "forecast$n_components must match gating$A_pi rows"
  )

  # basic numeric ranges
  for (nm in c("lambda_sigma", "lambda_f", "lambda_e", "vov_lambda")) {
    v <- spec$risk[[nm]]
    came_assert(
      is.numeric(v) && length(v) == 1 && is.finite(v) && v > 0 && v < 1,
      paste0("spec_risk_", nm), paste0("risk$", nm, " must be in (0,1)")
    )
  }
  came_assert(
    is.numeric(spec$risk$glasso_lambda) && is.finite(spec$risk$glasso_lambda) && spec$risk$glasso_lambda >= 0,
    "spec_risk_glasso_lambda", "risk$glasso_lambda must be >= 0"
  )
  came_assert(
    is.numeric(spec$risk$psd_eps) && is.finite(spec$risk$psd_eps) && spec$risk$psd_eps > 0,
    "spec_risk_psd_eps", "risk$psd_eps must be > 0"
  )

  # structure theta_alpha
  ta <- spec$structure$theta_alpha
  came_assert(is.numeric(ta) && length(ta) >= 4, "spec_theta_alpha", "structure$theta_alpha must be numeric length>=4")
  came_assert(
    all(c("intercept", "edge_stab", "chi", "xi") %in% names(ta)),
    "spec_theta_alpha_names", "structure$theta_alpha must have names: intercept, edge_stab, chi, xi"
  )

  # forecast cold start policy
  csp <- spec$forecast$cold_start_policy %||% "error"
  came_assert(
    csp %in% c("error", "skip"), "spec_cold_start_policy",
    "forecast$cold_start_policy must be 'error' or 'skip'"
  )

  invisible(spec)
}

# ---- state validation (shape/type checks only; values may be NULL on cold start) ----

came_state_validate <- function(state) {
  if (is.null(state)) {
    return(invisible(NULL))
  }
  came_assert(is.list(state), "state_type", "state must be a list or NULL")

  if (!is.null(state$prev_target)) {
    pt <- state$prev_target
    came_assert(
      is.data.frame(pt) && all(c("symbol", "weight_target") %in% names(pt)),
      "state_prev_target", "prev_target must be data.frame(symbol, weight_target)"
    )
  }

  # risk
  if (!is.null(state$risk$sigma2)) came_assert(is.numeric(state$risk$sigma2), "state_sigma2", "risk$sigma2 must be numeric")
  if (!is.null(state$risk$sigma2_hist)) came_assert(is.matrix(state$risk$sigma2_hist), "state_sigma2_hist", "risk$sigma2_hist must be matrix")
  if (!is.null(state$risk$Sigma_f)) came_assert(is.matrix(state$risk$Sigma_f), "state_Sigma_f", "risk$Sigma_f must be matrix")
  if (!is.null(state$risk$S_e)) came_assert(is.matrix(state$risk$S_e), "state_S_e", "risk$S_e must be matrix")
  if (!is.null(state$risk$B_prev)) came_assert(is.matrix(state$risk$B_prev), "state_B_prev", "risk$B_prev must be matrix")
  if (!is.null(state$risk$bar_sigma_prev)) {
    came_assert(
      is.numeric(state$risk$bar_sigma_prev) && length(state$risk$bar_sigma_prev) == 1L,
      "state_bar_sigma_prev", "risk$bar_sigma_prev must be scalar numeric"
    )
  }
  if (!is.null(state$risk$vov2)) {
    came_assert(
      is.numeric(state$risk$vov2) && length(state$risk$vov2) == 1L,
      "state_vov2", "risk$vov2 must be scalar numeric"
    )
  }

  # structure
  if (!is.null(state$structure$P_bar)) came_assert(is.matrix(state$structure$P_bar), "state_P_bar", "structure$P_bar must be matrix")
  if (!is.null(state$structure$M_prev)) came_assert(is.matrix(state$structure$M_prev), "state_M_prev", "structure$M_prev must be matrix")
  if (!is.null(state$structure$edge_stab)) came_assert(is.matrix(state$structure$edge_stab), "state_edge_stab", "structure$edge_stab must be matrix")
  if (!is.null(state$structure$node_stab)) came_assert(is.numeric(state$structure$node_stab), "state_node_stab", "structure$node_stab must be numeric")
  if (!is.null(state$structure$labels)) {
    came_assert(
      is.integer(state$structure$labels) || is.numeric(state$structure$labels),
      "state_labels", "structure$labels must be integer-like"
    )
  }

  # signals
  if (!is.null(state$signals$kalman)) came_assert(is.list(state$signals$kalman), "state_kalman", "signals$kalman must be list")
  if (!is.null(state$signals$omega)) came_assert(is.list(state$signals$omega), "state_omega", "signals$omega must be list")

  # forecast
  if (!is.null(state$forecast$models)) came_assert(is.list(state$forecast$models), "state_models", "forecast$models must be list")
  if (!is.null(state$forecast$error_buckets)) came_assert(is.list(state$forecast$error_buckets), "state_err", "forecast$error_buckets must be list")
  if (!is.null(state$forecast$hist)) came_assert(is.list(state$forecast$hist), "state_fhist", "forecast$hist must be list")

  # history
  if (!is.null(state$history)) {
    h <- state$history
    came_assert(is.list(h$snapshots), "state_hist_snapshots", "history$snapshots must be list")
  }

  invisible(state)
}

# ---- snapshot contract (core invariants) ----

came_snapshot_validate <- function(x) {
  req <- c(
    "as_of_date", "universe", "a_t", "m_t",
    "weights", "cash_weight",
    "risk", "structure", "signals",
    "features", "forecast", "optimizer",
    "meta", "state_out"
  )
  miss <- setdiff(req, names(x))
  came_assert(length(miss) == 0, "snapshot_missing", paste("Missing fields:", paste(miss, collapse = ", ")))

  # weights budget
  wdf <- x$weights
  came_assert(
    is.data.frame(wdf) && all(c("symbol", "weight_target") %in% names(wdf)),
    "snapshot_weights", "weights must be data.frame(symbol, weight_target)"
  )
  tot <- sum(wdf$weight_target, na.rm = TRUE) + x$cash_weight
  came_assert(is.finite(tot) && abs(tot - 1.0) <= 1e-6, "snapshot_budget", "weights must sum to 1")

  # covariance invariants
  Sigma <- x$risk$Sigma_H
  came_assert_square_named_matrix(Sigma, "snapshot_sigma", "risk$Sigma_H must be named square covariance")
  came_assert(max(abs(Sigma - t(Sigma))) < 1e-6, "snapshot_sigma_sym", "risk$Sigma_H must be symmetric")

  # graph invariants
  M <- x$structure$M
  came_assert(is.matrix(M), "snapshot_M_type", "structure$M must be a matrix")
  came_assert(max(abs(M - t(M))) == 0, "snapshot_M_sym", "structure$M must be symmetric")
  came_assert(all(diag(M) == 0), "snapshot_M_diag", "structure$M diagonal must be zero")

  # gating weights
  pi <- x$forecast$pi
  came_assert(is.numeric(pi) && is.finite(sum(pi)) && abs(sum(pi) - 1.0) < 1e-6, "snapshot_pi", "forecast$pi must sum to 1")

  invisible(x)
}
