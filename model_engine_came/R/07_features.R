# 07_features.R — feature assembly X_{i,t}

.came_graph_mv <- function(M, s) {
  out <- as.vector(M %*% s)
  names(out) <- rownames(M)
  out
}

.came_cluster_z <- function(s, labels, eps=1e-8) {
  out <- s
  for (k in sort(unique(labels))) {
    idx <- names(labels)[labels == k]
    idx <- intersect(idx, names(s))
    if (length(idx) >= 2) {
      mu <- mean(s[idx], na.rm=TRUE)
      sdv <- sd(s[idx], na.rm=TRUE); if (!is.finite(sdv) || sdv < eps) sdv <- eps
      out[idx] <- (s[idx] - mu) / sdv
    } else if (length(idx) == 1) {
      out[idx] <- 0
    }
  }
  out
}

came_liquidity_features <- function(activity_window, R_window) {
  # activity_window fields: traded_value, traded_units, n_trades matrices (T x p)
  tv <- activity_window$traded_value
  tu <- activity_window$traded_units
  nt <- activity_window$n_trades
  syms <- colnames(tv); Tn <- nrow(tv)

  # current day fields (last row)
  tv_t <- tv[Tn, ]; tu_t <- tu[Tn, ]; nt_t <- nt[Tn, ]
  tv_t[!is.finite(tv_t) | tv_t < 0] <- 0
  tu_t[!is.finite(tu_t) | tu_t < 0] <- 0
  nt_t[!is.finite(nt_t) | nt_t < 0] <- 0

  l1 <- log1p(tv_t)
  l2 <- log1p(tu_t)
  l3 <- log1p(nt_t)

  avg_trade_brl <- tv_t / (nt_t + 1e-6)
  avg_trade_units <- tu_t / (nt_t + 1e-6)
  implied_px <- tv_t / (tu_t + 1e-6)

  # Amihud-like illiquidity (rolling mean |r|/tv)
  R_abs <- abs(R_window)
  tv_pos <- tv[-1, , drop=FALSE]
  tv_pos[tv_pos <= 0 | !is.finite(tv_pos)] <- 1
  illiq <- colMeans(R_abs / tv_pos, na.rm=TRUE)
  illiq[!is.finite(illiq)] <- 0

  # rolling z-scores for liquidity logs
  z_roll <- function(x) {
    mu <- colMeans(x, na.rm=TRUE)
    sdv <- apply(x, 2, sd, na.rm=TRUE); sdv[sdv <= 1e-8 | !is.finite(sdv)] <- 1
    (x[Tn, ] - mu) / sdv
  }
  z_tv <- z_roll(log1p(tv))
  z_nt <- z_roll(log1p(nt))

  data.frame(
    f_ltv = l1,
    f_ltu = l2,
    f_lnt = l3,
    f_avg_trade_brl = log1p(avg_trade_brl),
    f_avg_trade_units = log1p(avg_trade_units),
    f_implied_px = log1p(implied_px),
    f_illiq = log1p(illiq),
    f_ltv_z = z_tv,
    f_lnt_z = z_nt,
    row.names = syms,
    stringsAsFactors = FALSE
  )
}

came_features_build <- function(risk_art, struct_art, sig_art, activity_window, R_window, m_t, spec) {
  syms <- colnames(R_window)
  n <- length(syms)

  # temporal: multiscale mom + residual mom + kalman + factor trends projection
  mom_multi <- sig_art$mom_multi[syms, , drop=FALSE]
  colnames(mom_multi) <- paste0("f_", colnames(mom_multi))

  resid_multi <- sig_art$resid_mom_multi
  if (!is.null(resid_multi)) {
    resid_multi <- resid_multi[syms, , drop=FALSE]
    colnames(resid_multi) <- paste0("f_", colnames(resid_multi))
  } else {
    resid_multi <- matrix(0, n, 0, dimnames=list(syms, character(0)))
  }

  kal <- sig_art$kalman
  f_kal <- cbind(
    f_kal_slope = kal$slope[syms],
    f_kal_slope_z = kal$slope_z[syms],
    f_kal_innov_z = kal$innov_z[syms]
  )
  rownames(f_kal) <- syms
  f_facproj <- cbind(f_facproj = sig_art$fac_proj[syms])
  rownames(f_facproj) <- syms

  # scalar signals
  f_scalar <- cbind(
    f_s_mom = sig_art$s_mom[syms],
    f_s_kal = sig_art$s_kal[syms],
    f_s_fac = sig_art$s_fac[syms]
  )
  rownames(f_scalar) <- syms

  # structural / graph transforms (on scalar signals)
  ops <- struct_art$ops
  A <- ops$A; A_sgn <- ops$A_sgn; L <- ops$L; L_norm <- ops$L_norm
  labels <- struct_art$clustering$labels
  labels <- labels[syms]
  s_mom <- sig_art$s_mom[syms]; s_kal <- sig_art$s_kal[syms]; s_fac <- sig_art$s_fac[syms]

  peer_mom <- .came_graph_mv(A, s_mom)
  rel_mom <- s_mom - peer_mom
  sgn_mom <- .came_graph_mv(A_sgn, s_mom)
  ten_mom <- .came_graph_mv(L, s_mom)
  shr_mom <- tryCatch(as.vector(solve(diag(n) + 0.1 * L_norm, s_mom)), error = function(e) s_mom)
  names(shr_mom) <- syms

  clz_mom <- .came_cluster_z(s_mom, labels)

  f_graph <- cbind(
    f_peer_mom = peer_mom,
    f_rel_mom = rel_mom,
    f_sgn_mom = sgn_mom,
    f_ten_mom = ten_mom,
    f_shr_mom = shr_mom,
    f_clz_mom = clz_mom,
    f_deg = ops$deg[syms]
  )
  rownames(f_graph) <- syms

  # PCA–graph interaction (architecture φ_cf1, φ_cf2)
  B <- risk_art$B[syms, , drop=FALSE]
  fac_tr <- sig_art$factor_trends
  g_bar <- if (!is.null(fac_tr)) fac_tr[, 1, drop=TRUE] else rep(0, ncol(B))
  g_bar[!is.finite(g_bar)] <- 0
  # cluster centroids of loadings
  phi_cf1 <- as.vector(B %*% g_bar)
  names(phi_cf1) <- syms

  phi_cf2 <- setNames(rep(0, n), syms)
  for (k in sort(unique(labels))) {
    idx <- names(labels)[labels == k]
    idx <- intersect(idx, syms)
    if (length(idx) >= 2) {
      b_bar <- colMeans(B[idx, , drop=FALSE])
      for (nm in idx) {
        phi_cf2[nm] <- sum((B[nm, ] - b_bar) * g_bar)
      }
    }
  }
  f_pca_graph <- cbind(f_cf1 = phi_cf1, f_cf2 = phi_cf2)
  rownames(f_pca_graph) <- syms

  # liquidity/activity
  f_liq <- came_liquidity_features(activity_window, R_window)
  f_liq <- f_liq[syms, , drop=FALSE]

  # global state context (broadcast)
  m <- m_t
  m[!is.finite(m)] <- 0
  f_state <- matrix(rep(as.numeric(m), each = n), nrow = n, byrow = FALSE)
  colnames(f_state) <- paste0("f_state_", names(m))
  rownames(f_state) <- syms

  X <- cbind(mom_multi, resid_multi, f_kal, f_facproj, f_scalar, f_graph, f_pca_graph, f_liq, f_state)
  X[!is.finite(X)] <- 0

  groups <- list(
    temporal = colnames(mom_multi),
    temporal_resid = colnames(resid_multi),
    kalman = colnames(f_kal),
    factor = colnames(f_facproj),
    scalars = colnames(f_scalar),
    graph = colnames(f_graph),
    pca_graph = colnames(f_pca_graph),
    liquidity = colnames(f_liq),
    state = colnames(f_state)
  )

  list(X = X, groups = groups)
}
