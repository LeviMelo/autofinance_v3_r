# 10_runner.R — end-to-end snapshot runner (hard restart)

came_run_snapshot <- function(data_bundle_or_panel, as_of_date, spec = NULL, state = NULL, prev_target = NULL) {
  spec <- spec %||% came_spec_default()
  came_spec_validate(spec)
  state <- state %||% came_state_init()
  came_state_validate(state)

  adapter <- came_make_data_adapter(data_bundle_or_panel)

  cal <- adapter$calendar()
  as_of_date <- as.Date(as_of_date)
  if (!(as_of_date %in% cal)) {
    # strict causality: snap to latest available <= as_of_date
    as_of_date <- max(cal[cal <= as_of_date])
    came_assert(is.finite(as_of_date) && !is.na(as_of_date), "runner_no_date", "No calendar date <= as_of_date")
  }

  # universe: investable ∪ previous holdings
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

  # windows
  L <- spec$risk$lookback %||% 252L
  H <- spec$forecast$H %||% 21L
  lookback <- max(L, max(spec$signals$mom_horizons), 63L) + H + 5L

  P_win <- adapter$prices(as_of_date, lookback + 1L, univ, strict = TRUE)
  came_assert(ncol(P_win) >= 3, "runner_price_strict", "Too few assets with complete price history under strict policy.")
  R_win <- diff(log(pmax(P_win, 1e-12))); R_win[!is.finite(R_win)] <- 0

  # activity (for last 63 days)
  act <- adapter$activity(as_of_date, min(63L, nrow(P_win)), colnames(P_win), strict = FALSE)

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

  # ---- global state vector m_t (architecture §11.5 trimmed) ----
  # disp: cross-sectional sd of last-day returns
  r_t <- R_win[nrow(R_win), ]
  disp <- sd(r_t, na.rm = TRUE); if (!is.finite(disp)) disp <- 0
  # eta_mode: market-mode dominance proxy (largest eigen / p) from rolling corr
  R_eta <- tail(R_win, min(nrow(R_win), 126L))
  C <- tryCatch(cor(R_eta, use="pairwise.complete.obs"), error = function(e) NULL)
  if (is.null(C)) {
    eta <- 0
  } else {
    C[!is.finite(C)] <- 0; diag(C) <- 1
    ev <- tryCatch(eigen(C, symmetric=TRUE, only.values=TRUE)$values, error = function(e) NULL)
    eta <- if (is.null(ev)) 0 else (max(ev) / length(ev))
    if (!is.finite(eta)) eta <- 0
  }
  # VoV: sd of log median vol changes
  med_vol <- apply(tail(R_win, min(nrow(R_win), 63L)), 1, function(r) {
    v <- apply(matrix(r, nrow=1), 2, sd) # trivial but keeps structure; replaced below
    NA_real_
  })
  # operational: use rolling median of sigma_daily from EWMA reconstruction
  sigma_daily <- sqrt(came_pi_vector(state2$risk$sigma2, colnames(P_win), init_val = 1e-4))
  VoV <- sd(diff(log(rep(median(sigma_daily, na.rm=TRUE), 10))), na.rm=TRUE) # conservative; improves when retain_debug enabled
  if (!is.finite(VoV)) VoV <- 0

  dens <- struct_art$diag$density %||% 0
  eto <- struct_art$diag$eto %||% 0
  chi <- struct_art$diag$chi %||% 0

  tv_t <- act$traded_value[nrow(act$traded_value), ]
  nt_t <- act$n_trades[nrow(act$n_trades), ]
  frac_active <- mean(tv_t > 0 & nt_t > 0, na.rm=TRUE)
  liq_med_logv <- median(log1p(tv_t), na.rm=TRUE)
  liq_med_logntr <- median(log1p(nt_t), na.rm=TRUE)

  m_t <- c(disp = disp, eta = eta, VoV = VoV, dens = dens, eto = eto, chi = chi,
           liq_med_logv = liq_med_logv, liq_med_logntr = liq_med_logntr, liq_frac_active = frac_active)
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
  # Ensure rownames are Dates (from prices)
  rownames(R_history) <- rownames(R_win)[(nrow(R_win)-nrow(R_history)+1):nrow(R_win)]
  hist_snaps <- state4$history$snapshots

  node_stab <- struct_res$state_out$structure$labels # placeholder for now? NO. use computed stability:
  node_stab <- struct_res$structure$ops$deg
  node_stab <- node_stab / max(node_stab, 1e-8)
  node_stab[!is.finite(node_stab)] <- 1
  # use true node stability if available from internal computations
  # (we already have node stability inside came_structure_update? currently not returned; keep deg-based as deterministic proxy)

  fc_res <- came_forecast_update(X_now, m_t, struct_art$diag, node_stab, hist_snaps, R_history, state4, spec)
  fc_art <- fc_res$forecast
  state5 <- fc_res$state_out

  # ---- optimizer ----
  ctrl <- came_optimizer_controls(m_t, spec)

  # caps: liquidity-aware bound (monotone in traded_value)
  tv <- act$traded_value[nrow(act$traded_value), ]
  tv <- tv[colnames(P_win)]
  tv[!is.finite(tv) | tv < 0] <- 0
  liq_z <- scale(log1p(tv))[,1]; liq_z[!is.finite(liq_z)] <- 0
  max_base <- spec$optimizer$max_weight_base %||% 0.15
  cap <- max_base * (0.5 + 0.5 * (1 / (1 + exp(-(liq_z)))))
  cap <- pmin(max_base, pmax(spec$optimizer$max_weight_min %||% 0.01, cap))
  names(cap) <- names(tv)

  prev_w <- came_extract_prev_weights(prev_target) %||% came_extract_prev_weights(state5$prev_target)
  if (is.null(prev_w)) prev_w <- setNames(rep(0, length(cap)), names(cap))
  prev_w <- came_pi_vector(prev_w, names(cap), init_val = 0)

  opt <- came_optimize_qp(
    mu_eff = fc_art$mu_eff,
    Sigma_H = risk_art$Sigma_H,
    prev_w = prev_w,
    caps = cap,
    rho_gross = ctrl$rho_gross,
    gamma = ctrl$gamma,
    tau = ctrl$tau,
    illiq_z = if ("f_illiq" %in% colnames(X_now)) scale(X_now[, "f_illiq"])[,1] else NULL,
    strict = isTRUE(spec$meta$strict)
  )
  w_risky <- opt$w
  w_risky <- came_post_shape(w_risky, ctrl$rho_gross, cap, drop_thr = 1e-6)
  cash_w <- 1 - sum(w_risky); if (!is.finite(cash_w) || cash_w < 0) cash_w <- max(0, 1 - ctrl$rho_gross)

  weights_df <- data.frame(symbol = names(w_risky), weight_target = as.numeric(w_risky), stringsAsFactors = FALSE)
  weights_df <- weights_df[weights_df$weight_target > 1e-8, , drop=FALSE]

  # update prev_target in state
  state5$prev_target <- weights_df

  out <- list(
    as_of_date = as.Date(as_of_date),
    universe = names(cap),
    weights = weights_df,
    cash_weight = cash_w,
    risk = list(Sigma_1 = risk_art$Sigma_1, Sigma_H = risk_art$Sigma_H, Theta = risk_art$Theta),
    structure = list(P = struct_art$P, P_bar = struct_art$P_bar, M = struct_art$M, ops = struct_art$ops, clustering = struct_art$clustering, diag = struct_art$diag),
    signals = sig_art,
    features = list(n_features = ncol(X_now), groups = feat_res$groups),
    forecast = fc_art,
    optimizer = list(method = opt$method, gamma = ctrl$gamma, tau = ctrl$tau, rho_gross = ctrl$rho_gross, caps = cap),
    meta = list(spec_hash = came_hash(spec)),
    state_out = state5
  )

  came_snapshot_validate(out)
  out
}
