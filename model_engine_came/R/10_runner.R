# 10_runner.R — end-to-end snapshot runner (hard restart) with tradability-correct optimization

came_run_snapshot <- function(data_bundle_or_panel, as_of_date, spec = NULL, state = NULL, prev_target = NULL) {
  spec <- spec %||% came_spec_default()
  came_spec_validate(spec)
  state <- state %||% came_state_init()
  came_state_validate(state)

  adapter <- came_make_data_adapter(data_bundle_or_panel)

  cal <- adapter$calendar()
  as_of_date <- as.Date(as_of_date)
  if (!(as_of_date %in% cal)) {
    as_of_date <- max(cal[cal <= as_of_date])
    came_assert(is.finite(as_of_date) && !is.na(as_of_date), "runner_no_date", "No calendar date <= as_of_date")
  }

  inv <- adapter$investable_universe(as_of_date, spec$data)
  eps_hold <- spec$data$eps_hold %||% 1e-8

  prev_hold <- character(0)
  if (!is.null(prev_target) && is.data.frame(prev_target)) {
    prev_hold <- prev_target$symbol[is.finite(prev_target$weight_target) & prev_target$weight_target > eps_hold]
    prev_hold <- unique(as.character(prev_hold))
  } else if (!is.null(state$prev_target)) {
    prev_hold <- state$prev_target$symbol[state$prev_target$weight_target > eps_hold]
  }

  univ <- unique(c(inv, prev_hold))
  came_assert(length(univ) >= 3, "runner_universe_small", "Universe too small to run model (need >=3 assets).")

  L <- spec$risk$lookback %||% 252L
  H <- spec$forecast$H %||% 21L
  lookback <- max(L, max(spec$signals$mom_horizons), 63L) + H + 5L

  P_win <- adapter$prices(as_of_date, lookback + 1L, univ, strict = TRUE)
  came_assert(ncol(P_win) >= 3, "runner_price_strict", "Too few assets with complete price history under strict policy.")
  R_win <- diff(log(pmax(P_win, 1e-12)))
  R_win[!is.finite(R_win)] <- 0

  state <- came_state_pi(state, colnames(P_win))

  act <- adapter$activity(as_of_date, min(63L, nrow(P_win)), colnames(P_win), strict = FALSE)

  # Tradability mask a_{i,t}
  tv_t <- act$traded_value[nrow(act$traded_value), ]
  nt_t <- act$n_trades[nrow(act$n_trades), ]
  cl_t <- P_win[nrow(P_win), ]

  a_t <- (is.finite(tv_t) & tv_t > 0) & (is.finite(nt_t) & nt_t > 0) & (is.finite(cl_t) & cl_t > 0)
  a_t <- a_t[colnames(P_win)]

  # ---- risk ----
  risk_res <- came_risk_update(tail(R_win, L), state, spec)
  risk_art <- risk_res$risk
  state1 <- risk_res$state_out

  # ---- structure ----
  activity_last <- act$traded_value[nrow(act$traded_value), ]
  names(activity_last) <- colnames(act$traded_value)

  struct_res <- came_structure_update(risk_art$Theta, activity_last, state1, spec)
  struct_art <- struct_res$structure
  state2 <- struct_res$state_out

  # ---- global state vector m_t ----
  r_t <- R_win[nrow(R_win), ]
  disp <- sd(r_t, na.rm = TRUE)
  if (!is.finite(disp)) disp <- 0

  R_eta <- tail(R_win, min(nrow(R_win), 126L))
  Cmat <- tryCatch(cor(R_eta, use = "pairwise.complete.obs"), error = function(e) NULL)
  if (is.null(Cmat)) {
    eta <- 0
  } else {
    Cmat[!is.finite(Cmat)] <- 0
    diag(Cmat) <- 1
    ev <- tryCatch(eigen(Cmat, symmetric = TRUE, only.values = TRUE)$values, error = function(e) NULL)
    eta <- if (is.null(ev)) 0 else (max(ev) / length(ev))
    if (!is.finite(eta)) eta <- 0
  }

  vov_lambda <- spec$risk$vov_lambda %||% 0.97
  vov_upd <- came_update_vov(state2, risk_art$sigma2, lambda_vov = vov_lambda)
  state2 <- vov_upd$state
  VoV <- vov_upd$VoV
  if (!is.finite(VoV)) VoV <- 0

  dens <- struct_art$diag$density %||% 0
  eto <- struct_art$diag$eto %||% 0
  chi <- struct_art$diag$chi %||% 0

  frac_active <- mean(tv_t > 0 & nt_t > 0, na.rm = TRUE)
  liq_med_logv <- median(log1p(tv_t), na.rm = TRUE)
  liq_med_logntr <- median(log1p(nt_t), na.rm = TRUE)

  m_t <- c(
    disp = disp, eta = eta, VoV = VoV, dens = dens, eto = eto, chi = chi,
    liq_med_logv = liq_med_logv, liq_med_logntr = liq_med_logntr, liq_frac_active = frac_active
  )
  m_t[!is.finite(m_t)] <- 0

  # ---- signals ----
  P_last <- P_win[nrow(P_win), ]
  sig_res <- came_signals_update(P_last, tail(R_win, max(spec$signals$mom_horizons)), risk_art, struct_art, state2, spec)
  sig_art <- sig_res$signals
  state3 <- sig_res$state_out

  # ---- features ----
  feat_res <- came_features_build(risk_art, struct_art, sig_art, act, tail(R_win, 63L), m_t, spec)
  X_now <- feat_res$X

  state4 <- came_history_append(state3, as_of_date, X_now, keep = spec$forecast$history_keep %||% 300L)

  # ---- forecast ----
  R_history <- tail(R_win, min(nrow(R_win), spec$forecast$history_keep %||% 300L))
  rownames(R_history) <- rownames(R_win)[(nrow(R_win) - nrow(R_history) + 1):nrow(R_win)]
  hist_snaps <- state4$history$snapshots

  node_stab <- struct_art$node_stab
  node_stab <- node_stab[colnames(P_win)]
  node_stab[!is.finite(node_stab)] <- 1

  fc_res <- came_forecast_update(X_now, m_t, struct_art$diag, node_stab, hist_snaps, R_history, state4, spec)
  fc_art <- fc_res$forecast
  fc_art$diag <- fc_res$diag
  state5 <- fc_res$state_out

  # ---- optimizer controls ----
  no_trade <- isTRUE(spec$meta$no_trade %||% FALSE)
  ctrl <- if (no_trade) list(gamma = NA_real_, tau = NA_real_, rho_gross = 0) else came_optimizer_controls(m_t, spec)

  # caps (liquidity-aware)
  tv <- tv_t[colnames(P_win)]
  tv[!is.finite(tv) | tv < 0] <- 0
  liq_z <- scale(log1p(tv))[, 1]
  liq_z[!is.finite(liq_z)] <- 0

  max_base <- spec$optimizer$max_weight_base %||% 0.15
  cap <- max_base * (0.5 + 0.5 * (1 / (1 + exp(-(liq_z)))))
  cap <- pmin(max_base, pmax(spec$optimizer$max_weight_min %||% 0.01, cap))
  names(cap) <- names(tv)

  # prev weights vector
  prev_w <- came_extract_prev_weights(prev_target) %||% came_extract_prev_weights(state5$prev_target)
  if (is.null(prev_w)) prev_w <- setNames(rep(0, length(a_t)), names(a_t))
  prev_w <- came_pi_vector(prev_w, names(a_t), init_val = 0)

  # illiquidity z for turnover scaling
  illiq_z <- NULL
  if ("f_illiq" %in% colnames(X_now)) {
    illiq_z <- scale(X_now[, "f_illiq"])[, 1]
    illiq_z[!is.finite(illiq_z)] <- 0
  }

  # portfolio-level optimization with frozen carry handling
  if (no_trade) {
    # No optimizer run; keep portfolio in cash and DO NOT overwrite prev_target in state.
    w_risky <- setNames(rep(0, length(a_t)), names(a_t))
    cash_w <- 1.0
    weights_df <- data.frame(symbol = character(0), weight_target = numeric(0), stringsAsFactors = FALSE)

    # preserve previous target (prefer explicit prev_target argument)
    if (!is.null(prev_target) && is.data.frame(prev_target)) {
      state5$prev_target <- prev_target
    }
  } else {
    # portfolio-level optimization with frozen carry handling
    opt_res <- came_optimize_portfolio(
      mu_eff = fc_art$mu_eff,
      Sigma_H = risk_art$Sigma_H,
      prev_w = prev_w,
      caps = cap,
      a_t = a_t,
      rho_gross = ctrl$rho_gross,
      gamma = ctrl$gamma,
      tau = ctrl$tau,
      illiq_z = illiq_z,
      turnover_illiq_scale = spec$optimizer$turnover_cost_illiq_scale %||% 2.0,
      eps_hold = eps_hold,
      drop_thr = 1e-6,
      strict = isTRUE(spec$meta$strict)
    )

    w_risky <- opt_res$w
    cash_w <- opt_res$cash

    weights_df <- data.frame(symbol = names(w_risky), weight_target = as.numeric(w_risky), stringsAsFactors = FALSE)
    weights_df <- weights_df[weights_df$weight_target > 1e-8, , drop = FALSE]

    state5$prev_target <- weights_df
  }

  out <- list(
    as_of_date = as.Date(as_of_date),
    universe = names(cap),
    a_t = a_t,
    m_t = m_t,
    weights = weights_df,
    cash_weight = cash_w,
    risk = list(Sigma_1 = risk_art$Sigma_1, Sigma_H = risk_art$Sigma_H, Theta = risk_art$Theta),
    structure = list(P = struct_art$P, P_bar = struct_art$P_bar, M = struct_art$M, ops = struct_art$ops, clustering = struct_art$clustering, diag = struct_art$diag),
    signals = sig_art,
    features = list(n_features = ncol(X_now), groups = feat_res$groups),
    forecast = fc_art,
    optimizer = list(
      method = if (no_trade) "no_trade" else opt_res$method,
      gamma = ctrl$gamma,
      tau = ctrl$tau,
      rho_gross = ctrl$rho_gross,
      caps = cap,
      frozen = if (no_trade) character(0) else opt_res$frozen,
      no_trade = no_trade
    ),
    meta = list(spec_hash = came_hash(spec)),
    state_out = state5
  )

  came_snapshot_validate(out)
  out
}
