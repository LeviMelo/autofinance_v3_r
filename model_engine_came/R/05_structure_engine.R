# 05_structure_engine.R — partial corr, adaptive smoothing, mask, operators, clustering

.came_partial_corr <- function(Theta) {
  came_assert_square_named_matrix(Theta, "struct_theta", "Theta must be named square precision")
  d <- sqrt(diag(Theta))
  d[!is.finite(d) | d <= 0] <- 1e-8
  P <- -Theta / outer(d, d)
  diag(P) <- 0
  P <- came_symmetrize(P)
  dimnames(P) <- dimnames(Theta)
  P
}

.came_structural_shock <- function(P_t, P_prev) {
  if (is.null(P_prev)) return(0)
  common <- intersect(colnames(P_t), colnames(P_prev))
  if (length(common) < 2) return(0)
  D <- P_t[common, common] - P_prev[common, common]
  nf <- sqrt(sum(D^2, na.rm = TRUE))
  denom <- sqrt(length(common) * (length(common) - 1))
  chi <- if (denom > 0) nf / denom else 0
  if (!is.finite(chi)) 0 else chi
}

.came_edge_stability <- function(edge_prev, P_t, P_bar_prev, univ, lambda_edge) {
  p <- length(univ)
  if (is.null(edge_prev) || is.null(P_bar_prev)) {
    out <- matrix(0.5, p, p, dimnames = list(univ, univ))
    diag(out) <- 1
    return(out)
  }
  s_prev <- came_pi_matrix(edge_prev, univ, init_diag = 0.5)
  common <- intersect(univ, intersect(colnames(P_t), colnames(P_bar_prev)))
  out <- s_prev
  if (length(common) >= 2) {
    sm <- sign(P_t[common, common]) == sign(P_bar_prev[common, common])
    sm[is.na(sm)] <- TRUE
    diag(sm) <- TRUE
    out[common, common] <- lambda_edge * s_prev[common, common] + (1 - lambda_edge) * (sm * 1.0)
  }
  diag(out) <- 1
  came_symmetrize(out)
}

.came_alpha_matrix <- function(P_t, P_bar_prev, edge_stab, chi, theta_alpha) {
  # z = b0 + b1*s_edge + b2*chi + b3*xi, xi = |P_t - P_bar_prev|
  syms <- colnames(P_t)
  xi <- abs(P_t - P_bar_prev)
  xi[!is.finite(xi)] <- 0
  s <- edge_stab[syms, syms]
  z <- theta_alpha["intercept"] +
    theta_alpha["edge_stab"] * s +
    theta_alpha["chi"] * chi +
    theta_alpha["xi"] * xi
  A <- 1 / (1 + exp(-z))
  diag(A) <- 1
  A[!is.finite(A)] <- 0.5
  came_symmetrize(A)
}

.came_smooth_P <- function(P_t, P_bar_prev, alpha_mat) {
  if (is.null(P_bar_prev)) return(P_t)
  syms <- colnames(P_t)
  P_prev_m <- P_bar_prev[syms, syms, drop=FALSE]
  A <- alpha_mat
  P_bar <- A * P_prev_m + (1 - A) * P_t
  diag(P_bar) <- 0
  came_symmetrize(P_bar)
}

.came_liquidity_rank <- function(activity_snapshot) {
  # rank log(1+traded_value) cross-section, returns [0,1]
  tv <- activity_snapshot$traded_value
  tv[!is.finite(tv) | tv < 0] <- 0
  x <- log1p(tv)
  r <- rank(x, ties.method = "average") / (length(x) + 1)
  r
}

.came_mask_build <- function(P_bar, spec, prev_M = NULL, liq_rank = NULL, chi = 0) {
  syms <- colnames(P_bar)
  n <- length(syms)
  U <- abs(P_bar); diag(U) <- 0

  # per-node k_i and q_on(i)
  k_min <- as.integer(spec$structure$k_min %||% 2L)
  k_max <- as.integer(spec$structure$k_max %||% 10L)
  if (is.null(liq_rank)) liq_rank <- setNames(rep(0.5, n), syms)
  liq_rank <- liq_rank[syms]
  liq_rank[!is.finite(liq_rank)] <- 0.5

  # k_i increases with liquidity, decreases with shock
  k_i <- k_min + floor((k_max - k_min) * (liq_rank) * exp(-chi))
  k_i <- pmax(k_min, pmin(k_max, k_i))

  q_base <- spec$structure$q_on_base %||% 0.90
  q_chi_scale <- spec$structure$q_on_chi_scale %||% 0.10
  q_i <- pmin(0.99, pmax(0.50, q_base - q_chi_scale * chi + 0.05 * (liq_rank - 0.5)))

  hysteresis <- spec$structure$hysteresis_ratio %||% 0.7
  M_dir <- matrix(FALSE, n, n, dimnames = list(syms, syms))

  for (i in seq_len(n)) {
    s <- U[i, ]
    s[i] <- 0
    vals <- s[is.finite(s)]
    if (length(vals) < 2) next
    thr_on <- as.numeric(stats::quantile(vals, probs = q_i[i], na.rm = TRUE, names = FALSE, type = 7))
    thr_off <- thr_on * hysteresis

    eligible_on <- which(is.finite(s) & s >= thr_on)
    eligible <- eligible_on

    # hysteresis: keep previous edges above off threshold
    if (!is.null(prev_M)) {
      prev_row <- prev_M[syms[i], syms]
      keep_prev <- which(prev_row & is.finite(s) & s >= thr_off)
      eligible <- unique(c(eligible_on, keep_prev))
    }
    if (length(eligible) == 0) next

    # top-k_i control
    ord <- eligible[order(s[eligible], decreasing = TRUE)]
    keep <- head(ord, k_i[i])
    M_dir[i, keep] <- TRUE
  }

  # global exceptions
  qg <- spec$structure$q_global %||% 0.995
  uvals <- U[upper.tri(U)]
  uvals <- uvals[is.finite(uvals)]
  if (length(uvals) > 10) {
    thr_g <- as.numeric(stats::quantile(uvals, probs = qg, na.rm = TRUE, names = FALSE))
    if (is.finite(thr_g) && thr_g > 0) {
      idx <- which(U >= thr_g, arr.ind = TRUE)
      for (k in seq_len(nrow(idx))) {
        i <- idx[k,1]; j <- idx[k,2]
        if (i != j) M_dir[i,j] <- TRUE
      }
    }
  }

  M <- M_dir | t(M_dir)
  diag(M) <- FALSE

  beta_w <- spec$structure$beta_w %||% 1.0
  W_abs <- (U^beta_w) * M
  W_abs <- came_symmetrize(W_abs); diag(W_abs) <- 0
  W_sgn <- (P_bar * (U^(beta_w - 1))) * M
  W_sgn <- came_symmetrize(W_sgn); diag(W_sgn) <- 0

  list(M = M, W_abs = W_abs, W_sgn = W_sgn)
}

.came_operators <- function(W_abs, W_sgn) {
  syms <- colnames(W_abs); n <- length(syms)
  deg <- rowSums(W_abs)
  deg[!is.finite(deg) | deg <= 0] <- 1e-12
  A <- W_abs / deg
  A_sgn <- W_sgn / deg
  D <- diag(deg)
  L <- D - W_abs
  d_inv_sqrt <- 1 / sqrt(deg)
  d_inv_sqrt[!is.finite(d_inv_sqrt)] <- 0
  L_norm <- diag(d_inv_sqrt) %*% L %*% diag(d_inv_sqrt)
  L_norm <- came_symmetrize(L_norm)

  dimnames(A) <- list(syms, syms)
  dimnames(A_sgn) <- list(syms, syms)
  dimnames(L) <- list(syms, syms)
  dimnames(L_norm) <- list(syms, syms)
  names(deg) <- syms
  list(A=A, A_sgn=A_sgn, L=L, L_norm=L_norm, deg=deg)
}

.came_choose_K <- function(evals, K_min, K_max) {
  n <- length(evals)
  K_cand <- min(K_max, n - 1L)
  if (K_cand < K_min) return(K_min)
  gaps <- diff(evals[1:min(n, K_cand + 1L)])
  if (length(gaps) < K_min) return(K_min)
  idx <- which.max(gaps[K_min:length(gaps)]) + K_min - 1L
  max(K_min, min(K_max, idx))
}

.came_spectral_cluster <- function(L_norm, K_min, K_max) {
  n <- ncol(L_norm)
  if (n < K_min) {
    lab <- setNames(rep(1L, n), colnames(L_norm))
    return(list(labels = lab, K = 1L, method="trivial"))
  }
  eig <- eigen(L_norm, symmetric = TRUE)
  ord <- order(eig$values)
  evals <- eig$values[ord]
  evecs <- eig$vectors[, ord, drop=FALSE]
  K <- .came_choose_K(evals, K_min, K_max)
  V <- evecs[, 1:K, drop=FALSE]
  rn <- sqrt(rowSums(V^2)); rn[rn <= 1e-12] <- 1
  Vn <- V / rn
  km <- kmeans(Vn, centers = K, nstart = 10, iter.max = 100)
  lab <- setNames(as.integer(km$cluster), colnames(L_norm))
  list(labels=lab, K=K, method="spectral", within=km$tot.withinss)
}

.came_persist_labels <- function(labels_new, labels_prev) {
  came_require("clue")
  if (is.null(labels_prev)) return(labels_new)
  common <- intersect(names(labels_new), names(labels_prev))
  if (length(common) < 2) return(labels_new)
  K_new <- max(labels_new[common]); K_prev <- max(labels_prev[common])
  K <- max(K_new, K_prev)
  overlap <- matrix(0, K, K)
  for (nm in common) overlap[labels_new[nm], labels_prev[nm]] <- overlap[labels_new[nm], labels_prev[nm]] + 1

  # maximize overlap => cost = -overlap
  perm <- as.integer(clue::solve_LSAP(-overlap))
  # perm[j] = row assigned to column j; we want map new_cluster(row) -> prev_cluster(col)
  map <- rep(NA_integer_, K)
  for (col in seq_len(K)) map[perm[col]] <- col

  out <- labels_new
  for (nm in names(out)) {
    c0 <- out[nm]
    if (c0 <= K && !is.na(map[c0])) out[nm] <- map[c0]
  }
  out
}

came_structure_update <- function(Theta, activity_last, state, spec) {
  P_t <- .came_partial_corr(Theta)
  univ <- colnames(P_t)

  P_bar_prev <- state$structure$P_bar
  edge_prev <- state$structure$edge_stab
  M_prev <- state$structure$M_prev

  chi <- .came_structural_shock(P_t, P_bar_prev)

  lambda_edge <- spec$structure$lambda_edge %||% 0.95
  edge_stab <- .came_edge_stability(edge_prev, P_t, P_bar_prev, univ, lambda_edge)

  # adaptive smoothing
  if (is.null(P_bar_prev)) {
    P_bar <- P_t
  } else {
    P_bar_prev_m <- came_pi_matrix(P_bar_prev, univ, init_diag = 0)
    theta <- spec$structure$theta_alpha
    alpha_mat <- .came_alpha_matrix(P_t, P_bar_prev_m, edge_stab, chi, theta)
    P_bar <- .came_smooth_P(P_t, P_bar_prev_m, alpha_mat)
  }

  liq_rank <- .came_liquidity_rank(activity_last[univ])
  mask <- .came_mask_build(P_bar, spec, prev_M = M_prev, liq_rank = liq_rank, chi = chi)
  ops <- .came_operators(mask$W_abs, mask$W_sgn)

  # diagnostics
  dens <- if (length(univ) > 1) sum(mask$M) / (length(univ) * (length(univ) - 1)) else 0
  eto <- 0
  if (!is.null(M_prev) && is.matrix(M_prev)) {
    common <- intersect(colnames(M_prev), univ)
    if (length(common) >= 2) {
      prev_edges <- sum(M_prev[common, common]) / 2
      if (prev_edges > 0) {
        overlap <- sum(mask$M[common, common] & M_prev[common, common]) / 2
        eto <- 1 - overlap / prev_edges
      }
    }
  }
  node_stab <- setNames(rep(1, length(univ)), univ)
  if (!is.null(M_prev) && is.matrix(M_prev)) {
    common <- intersect(colnames(M_prev), univ)
    for (nm in common) {
      prev_deg <- sum(M_prev[nm, common])
      if (prev_deg > 0) {
        changed <- sum(abs(as.numeric(mask$M[nm, common]) - as.numeric(M_prev[nm, common])))
        node_stab[nm] <- 1 - changed / max(1, prev_deg)
      }
    }
  }

  cl <- .came_spectral_cluster(ops$L_norm, spec$structure$K_min, spec$structure$K_max)
  labels_prev <- state$structure$labels
  if (!is.null(labels_prev)) labels_prev <- labels_prev[names(labels_prev) %||% names(cl$labels)]
  cl$labels <- .came_persist_labels(cl$labels, labels_prev)

  st_out <- state
  st_out$structure$P_bar <- P_bar
  st_out$structure$M_prev <- mask$M
  st_out$structure$edge_stab <- edge_stab
  st_out$structure$labels <- as.integer(cl$labels)

  list(
    structure = list(
      P = P_t, P_bar = P_bar, M = mask$M,
      W_abs = mask$W_abs, W_sgn = mask$W_sgn,
      ops = ops, clustering = cl,
      diag = list(chi=chi, density=dens, eto=eto)
    ),
    state_out = st_out
  )
}
