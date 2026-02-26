# 07_features.R — Feature engine assembly X_{i,t} (architecture §11)
# Produces a causal feature matrix X_now (rows=assets, cols=features).
# Includes:
#  - temporal block: raw/residual TSMOM multiscale, Kalman, factor-projected continuation
#  - structural context block: graph operator transforms for s_mom/s_kal/s_fac
#  - PCA–graph interaction block: cf1/cf2
#  - liquidity/activity block: explicit + rolling z + interactions + local mismatch vs neighbors
#  - global state context block: broadcast m_t
#
# No placeholder logic; numerical regularization is explicit and minimal.

# --- helpers ---------------------------------------------------------------

.came_graph_mv <- function(M, s) {
  came_assert(is.matrix(M), "feat_graph_M", "Graph operator must be a matrix")
  came_assert(!is.null(rownames(M)) && !is.null(colnames(M)), "feat_graph_dimnames", "Graph operator must have dimnames")
  s <- s[colnames(M)]
  s[!is.finite(s)] <- 0
  out <- as.vector(M %*% s)
  names(out) <- rownames(M)
  out
}

.came_graph_shrink <- function(L_norm, s, lambda_g = 0.1, eps = 1e-8) {
  came_assert(is.matrix(L_norm), "feat_shr_L", "L_norm must be a matrix")
  n <- ncol(L_norm)
  came_assert(n == nrow(L_norm), "feat_shr_square", "L_norm must be square")
  s <- s[colnames(L_norm)]
  s[!is.finite(s)] <- 0

  lam <- lambda_g %||% 0.1
  if (!is.finite(lam) || lam < 0) lam <- 0.1

  A <- diag(n) + lam * L_norm
  # mathematically legitimate numerical ridge if solve is ill-conditioned
  sol <- tryCatch(solve(A, s), error = function(e) NULL)
  if (is.null(sol)) {
    sol <- tryCatch(solve(A + eps * diag(n), s), error = function(e) NULL)
  }
  if (is.null(sol)) stop(came_error("feat_shr_solve_failed", "Failed to compute (I+lambda L_norm)^{-1}s"))
  names(sol) <- colnames(L_norm)
  sol
}

.came_cluster_center <- function(s, labels) {
  s <- s[names(labels)]
  out <- s
  for (k in sort(unique(labels))) {
    idx <- names(labels)[labels == k]
    idx <- intersect(idx, names(s))
    if (length(idx) >= 1) {
      mu <- mean(s[idx], na.rm = TRUE)
      if (!is.finite(mu)) mu <- 0
      out[idx] <- mu
    }
  }
  out
}

.came_cluster_z <- function(s, labels, eps = 1e-8) {
  s <- s[names(labels)]
  out <- s
  for (k in sort(unique(labels))) {
    idx <- names(labels)[labels == k]
    idx <- intersect(idx, names(s))
    if (length(idx) >= 2) {
      mu <- mean(s[idx], na.rm = TRUE)
      sdv <- sd(s[idx], na.rm = TRUE)
      if (!is.finite(mu)) mu <- 0
      if (!is.finite(sdv) || sdv < eps) sdv <- eps
      out[idx] <- (s[idx] - mu) / sdv
    } else if (length(idx) == 1) {
      out[idx] <- 0
    }
  }
  out[!is.finite(out)] <- 0
  out
}

# EWMA z-score of a per-asset series X (T x p), return last z (p)
.came_ewma_z_last <- function(X, lambda = 0.97, eps = 1e-8) {
  came_assert(is.matrix(X) && nrow(X) >= 2, "feat_ewma_z_X", "X must be a matrix with >=2 rows")
  lam <- lambda
  if (!is.finite(lam) || lam <= 0 || lam >= 1) lam <- 0.97

  Tn <- nrow(X)
  p <- ncol(X)
  mu <- X[1, ]
  mu[!is.finite(mu)] <- 0
  v <- rep(0, p)

  for (t in 2:Tn) {
    x <- X[t, ]
    x[!is.finite(x)] <- mu[!is.finite(x)]
    # EWMA variance update around previous mean (stable)
    v <- lam * v + (1 - lam) * (x - mu)^2
    mu <- lam * mu + (1 - lam) * x
  }

  sdv <- sqrt(pmax(v, eps))
  z <- (X[Tn, ] - mu) / sdv
  z[!is.finite(z)] <- 0
  z
}

# --- liquidity / activity block (architecture §8) --------------------------

came_liquidity_features <- function(activity_window, R_window, ew_lambda = 0.97) {
  tv <- activity_window$traded_value
  tu <- activity_window$traded_units
  nt <- activity_window$n_trades

  came_assert(is.matrix(tv) && is.matrix(tu) && is.matrix(nt), "feat_liq_mats", "activity_window must contain matrices")
  syms <- colnames(tv)
  came_assert(identical(colnames(tu), syms) && identical(colnames(nt), syms), "feat_liq_cols", "activity matrices must share identical columns")

  # Align R_window to activity dates if possible
  if (!is.null(rownames(tv)) && !is.null(rownames(R_window))) {
    d_tv <- as.character(rownames(tv))
    d_r <- as.character(rownames(R_window))
    common_dates <- intersect(d_tv, d_r)
    if (length(common_dates) >= 5) {
      Rw <- R_window[common_dates, syms, drop = FALSE]
    } else {
      Rw <- R_window[, syms, drop = FALSE]
    }
  } else {
    Rw <- R_window[, syms, drop = FALSE]
  }

  Tn <- nrow(tv)

  tv_t <- tv[Tn, ]
  tu_t <- tu[Tn, ]
  nt_t <- nt[Tn, ]
  tv_t[!is.finite(tv_t) | tv_t < 0] <- 0
  tu_t[!is.finite(tu_t) | tu_t < 0] <- 0
  nt_t[!is.finite(nt_t) | nt_t < 0] <- 0

  f_active <- as.numeric((tv_t > 0) & (nt_t > 0))

  # core logs
  ltv <- log1p(tv_t)
  ltu <- log1p(tu_t)
  lnt <- log1p(nt_t)

  # avg trade sizes
  avg_trade_brl <- tv_t / (nt_t + 1e-6)
  avg_trade_units <- tu_t / (nt_t + 1e-6)

  # implied price proxy
  implied_px <- tv_t / (tu_t + 1e-6)

  # Amihud-like daily illiquidity series: |r|/(V^BRL + eps), then EWMA z on last
  # use aligned traded_value dates when possible
  # Build tv series for those return dates (shift-safe by direct date match)
  if (!is.null(rownames(tv)) && !is.null(rownames(Rw))) {
    tv_r <- tv[rownames(Rw), syms, drop = FALSE]
  } else {
    # best-effort: use last min rows
    m <- min(nrow(tv), nrow(Rw))
    tv_r <- tail(tv, m)
    Rw <- tail(Rw, m)
  }

  tv_r[tv_r <= 0 | !is.finite(tv_r)] <- 1
  illiq_daily <- abs(Rw) / tv_r
  illiq_daily[!is.finite(illiq_daily)] <- 0

  # rolling mean illiquidity (level) + EWMA z of daily illiq
  illiq_lvl <- colMeans(illiq_daily, na.rm = TRUE)
  illiq_lvl[!is.finite(illiq_lvl)] <- 0

  # EWMA z-scores (per asset) for ltv, lnt, illiq_daily
  ltv_series <- log1p(tv)
  lnt_series <- log1p(nt)
  ltv_z <- .came_ewma_z_last(ltv_series, lambda = ew_lambda)
  lnt_z <- .came_ewma_z_last(lnt_series, lambda = ew_lambda)
  illiq_z <- .came_ewma_z_last(illiq_daily, lambda = ew_lambda)

  out <- data.frame(
    f_active = f_active,
    f_ltv = ltv,
    f_ltu = ltu,
    f_lnt = lnt,
    f_avg_trade_brl = log1p(avg_trade_brl),
    f_avg_trade_units = log1p(avg_trade_units),
    f_implied_px = log1p(implied_px),
    f_illiq = log1p(illiq_lvl),
    f_ltv_z = ltv_z,
    f_lnt_z = lnt_z,
    f_illiq_z = illiq_z,
    row.names = syms,
    stringsAsFactors = FALSE
  )

  # Robust sanitize: data.frame is list-like, so sanitize column-wise
  out[] <- lapply(out, function(v) {
    v <- as.numeric(v)
    v[!is.finite(v)] <- 0
    v
  })

  out
}

# --- main feature assembly --------------------------------------------------

came_features_build <- function(risk_art, struct_art, sig_art, activity_window, R_window, m_t, spec) {
  came_assert(is.matrix(R_window), "feat_R_window", "R_window must be matrix")
  syms <- colnames(R_window)
  came_assert(length(syms) >= 3, "feat_syms", "Need >=3 assets for features")

  # --- Temporal block (§11.1) ---
  mom_multi <- sig_art$mom_multi[syms, , drop = FALSE]
  colnames(mom_multi) <- paste0("f_", colnames(mom_multi))

  resid_multi <- sig_art$resid_mom_multi[syms, , drop = FALSE]
  colnames(resid_multi) <- paste0("f_", colnames(resid_multi))

  kal <- sig_art$kalman
  f_kal <- cbind(
    f_kal_slope = kal$slope[syms],
    f_kal_slope_z = kal$slope_z[syms],
    f_kal_slope_var = log1p(kal$slope_var[syms]),
    f_kal_innov = kal$innov[syms],
    f_kal_innov_z = kal$innov_z[syms]
  )
  rownames(f_kal) <- syms

  f_facproj <- cbind(f_facproj = sig_art$fac_proj[syms])
  rownames(f_facproj) <- syms

  # Scalar signals
  f_scalar <- cbind(
    f_s_mom = sig_art$s_mom[syms],
    f_s_kal = sig_art$s_kal[syms],
    f_s_fac = sig_art$s_fac[syms]
  )
  rownames(f_scalar) <- syms

  # --- Structural context via graph operators (§11.2 + §9) ---
  ops <- struct_art$ops
  A <- ops$A
  A_sgn <- ops$A_sgn
  L <- ops$L
  L_norm <- ops$L_norm
  deg <- ops$deg[syms]
  deg[!is.finite(deg)] <- 0

  labels <- struct_art$clustering$labels
  labels <- labels[syms]
  labels[!is.finite(labels)] <- 1L
  labels <- as.integer(labels)

  node_stab <- struct_art$node_stab
  if (is.null(node_stab)) node_stab <- setNames(rep(1, length(syms)), syms)
  node_stab <- node_stab[syms]
  node_stab[!is.finite(node_stab)] <- 1

  lambda_g <- (spec$features$lambda_g %||% 0.1)
  if (!is.finite(lambda_g) || lambda_g < 0) lambda_g <- 0.1

  build_graph_block <- function(s, tag) {
    s <- s[syms]
    s[!is.finite(s)] <- 0

    peer <- .came_graph_mv(A, s)
    rel <- s - peer
    sgn <- .came_graph_mv(A_sgn, s)
    ten <- .came_graph_mv(L, s)
    shr <- .came_graph_shrink(L_norm, s, lambda_g = lambda_g)

    clctr <- .came_cluster_center(s, labels)
    clz <- .came_cluster_z(s, labels)

    out <- cbind(
      peer = peer,
      rel = rel,
      sgn = sgn,
      ten = ten,
      shr = shr,
      clctr = clctr,
      clz = clz
    )
    colnames(out) <- paste0("f_", colnames(out), "_", tag)
    rownames(out) <- syms
    out
  }

  g_mom <- build_graph_block(sig_art$s_mom, "mom")
  g_kal <- build_graph_block(sig_art$s_kal, "kal")
  g_fac <- build_graph_block(sig_art$s_fac, "fac")

  f_graph_meta <- cbind(
    f_deg = deg,
    f_node_stab = node_stab
  )
  rownames(f_graph_meta) <- syms

  # --- PCA–graph interaction (§11.3) ---
  B <- risk_art$B[syms, , drop = FALSE]
  g_scalar <- sig_art$factor_g_scalar
  if (is.null(g_scalar)) {
    # If unavailable, fall back to shortest-horizon factor trends (still causal)
    fac_tr <- sig_art$factor_trends
    if (!is.null(fac_tr)) g_scalar <- fac_tr[, 1, drop = TRUE] else g_scalar <- rep(0, ncol(B))
  }
  g_scalar <- as.numeric(g_scalar)
  g_scalar[!is.finite(g_scalar)] <- 0

  f_cf1 <- as.vector(B %*% g_scalar)
  names(f_cf1) <- syms

  f_cf2 <- setNames(rep(0, length(syms)), syms)
  for (k in sort(unique(labels))) {
    idx <- names(labels)[labels == k]
    idx <- intersect(idx, syms)
    if (length(idx) >= 2) {
      b_bar <- colMeans(B[idx, , drop = FALSE])
      for (nm in idx) {
        f_cf2[nm] <- sum((B[nm, ] - b_bar) * g_scalar)
      }
    }
  }
  f_pca_graph <- cbind(f_cf1 = f_cf1, f_cf2 = f_cf2)
  rownames(f_pca_graph) <- syms

  # --- Liquidity/activity block (§8 + §11.4) ---
  f_liq <- came_liquidity_features(activity_window, R_window, ew_lambda = spec$risk$lambda_e %||% 0.97)
  f_liq <- f_liq[syms, , drop = FALSE]

  # Liquidity interactions (§11.4): |s| * illiq_z
  f_liq_int <- cbind(
    f_liq_x_mom = abs(sig_art$s_mom[syms]) * f_liq$f_illiq_z,
    f_liq_x_kal = abs(sig_art$s_kal[syms]) * f_liq$f_illiq_z,
    f_liq_x_fac = abs(sig_art$s_fac[syms]) * f_liq$f_illiq_z
  )
  rownames(f_liq_int) <- syms
  f_liq_int[!is.finite(f_liq_int)] <- 0

  # Local liquidity mismatch vs neighbors (architecture §12.2 comp5)
  peer_ltvz <- .came_graph_mv(A, setNames(f_liq$f_ltv_z, syms))
  peer_illiqz <- .came_graph_mv(A, setNames(f_liq$f_illiq_z, syms))
  f_liq_local <- cbind(
    f_peer_ltv_z = peer_ltvz,
    f_mismatch_ltv_z = f_liq$f_ltv_z - peer_ltvz,
    f_peer_illiq_z = peer_illiqz,
    f_mismatch_illiq_z = f_liq$f_illiq_z - peer_illiqz
  )
  rownames(f_liq_local) <- syms
  f_liq_local[!is.finite(f_liq_local)] <- 0

  # --- Ensure liquidity block is a numeric matrix for final assembly ---
  # (keep f_liq as data.frame above because we use $ access)
  f_liq_m <- as.data.frame(f_liq, stringsAsFactors = FALSE)
  for (j in seq_along(f_liq_m)) f_liq_m[[j]] <- as.numeric(f_liq_m[[j]])
  f_liq_m <- as.matrix(f_liq_m)
  storage.mode(f_liq_m) <- "double"
  rownames(f_liq_m) <- syms

  # --- Global state context (§11.5) ---
  m <- m_t
  m[!is.finite(m)] <- 0
  f_state <- matrix(rep(as.numeric(m), each = length(syms)), nrow = length(syms), byrow = FALSE)
  colnames(f_state) <- paste0("f_state_", names(m))
  rownames(f_state) <- syms

  # --- Assemble X ---
  X <- cbind(
    mom_multi,
    resid_multi,
    f_kal,
    f_facproj,
    f_scalar,
    g_mom, g_kal, g_fac,
    f_graph_meta,
    f_pca_graph,
    f_liq_m,
    f_liq_int,
    f_liq_local,
    f_state
  )

  # enforce numeric matrix (avoid data.frame/list issues)
  X <- as.matrix(X)
  storage.mode(X) <- "double"
  X[!is.finite(X)] <- 0

  groups <- list(
    temporal_raw = colnames(mom_multi),
    temporal_resid = colnames(resid_multi),
    kalman = colnames(f_kal),
    factor = colnames(f_facproj),
    scalars = colnames(f_scalar),
    graph_mom = colnames(g_mom),
    graph_kal = colnames(g_kal),
    graph_fac = colnames(g_fac),
    graph_meta = colnames(f_graph_meta),
    pca_graph = colnames(f_pca_graph),
    liquidity = colnames(f_liq),
    liq_interactions = colnames(f_liq_int),
    liq_local = colnames(f_liq_local),
    state = colnames(f_state)
  )

  list(X = X, groups = groups)
}
