# 05_structure_engine.R — Structure/graph engine (architecture §6, §9, §10)
# Input: Theta (Glasso precision)
# Output: P_t, Pbar_t, M_t, W_abs, W_sgn, operators (A, A_sgn, L, L_norm), clusters, diagnostics, node stability.

# ---- §6.1 partial correlation from precision ----
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

# ---- §6.2 structural shock chi ----
.came_structural_shock <- function(P_t, P_prev) {
  if (is.null(P_prev)) {
    return(0)
  }
  common <- intersect(colnames(P_t), colnames(P_prev))
  if (length(common) < 2) {
    return(0)
  }
  D <- P_t[common, common] - P_prev[common, common]
  nf <- sqrt(sum(D^2, na.rm = TRUE))
  denom <- sqrt(length(common) * (length(common) - 1))
  chi <- if (denom > 0) nf / denom else 0
  if (!is.finite(chi)) 0 else chi
}

# ---- §6.2 edge stability recursion ----
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

# ---- §6.2 adaptive smoothing gain alpha_ij ----
.came_alpha_matrix <- function(P_t, P_bar_prev, edge_stab, chi, theta_alpha) {
  syms <- colnames(P_t)
  xi <- abs(P_t - P_bar_prev)
  xi[!is.finite(xi)] <- 0

  s <- edge_stab[syms, syms, drop = FALSE]

  z <- theta_alpha["intercept"] +
    theta_alpha["edge_stab"] * s +
    theta_alpha["chi"] * chi +
    theta_alpha["xi"] * xi

  A <- came_sigmoid(z)
  diag(A) <- 1
  A[!is.finite(A)] <- 0.5
  came_symmetrize(A)
}

.came_smooth_P <- function(P_t, P_bar_prev, alpha_mat) {
  if (is.null(P_bar_prev)) {
    return(P_t)
  }
  syms <- colnames(P_t)
  P_prev_m <- P_bar_prev[syms, syms, drop = FALSE]
  A <- alpha_mat
  P_bar <- A * P_prev_m + (1 - A) * P_t
  diag(P_bar) <- 0
  came_symmetrize(P_bar)
}

# ---- liquidity rank proxy r_liq in [0,1] (architecture §6.3.1) ----
.came_liquidity_rank <- function(traded_value_vec) {
  x <- as.numeric(traded_value_vec)
  names(x) <- names(traded_value_vec)
  x[!is.finite(x) | x < 0] <- 0
  lx <- log1p(x)
  r <- rank(lx, ties.method = "average") / (length(lx) + 1)
  names(r) <- names(traded_value_vec)
  r[!is.finite(r)] <- 0.5
  r
}

# ---- diagnostics: density, edge turnover, node stability (architecture §6.6) ----
.came_graph_density <- function(M) {
  n <- ncol(M)
  if (n < 2) {
    return(0)
  }
  sum(M) / (n * (n - 1))
}

.came_edge_turnover <- function(M, M_prev) {
  if (is.null(M_prev)) {
    return(0)
  }
  common <- intersect(colnames(M), colnames(M_prev))
  if (length(common) < 2) {
    return(0)
  }

  A <- M[common, common, drop = FALSE]
  B <- M_prev[common, common, drop = FALSE]

  prev_edges <- sum(B) / 2
  if (prev_edges <= 0) {
    return(0)
  }
  overlap <- sum(A & B) / 2
  1 - overlap / prev_edges
}

.came_node_stability <- function(M, M_prev) {
  univ <- colnames(M)
  out <- setNames(rep(1, length(univ)), univ)
  if (is.null(M_prev)) {
    return(out)
  }

  common <- intersect(colnames(M_prev), univ)
  if (length(common) < 2) {
    return(out)
  }

  for (nm in common) {
    prev_deg <- sum(M_prev[nm, common])
    if (prev_deg > 0) {
      changed <- sum(abs(as.numeric(M[nm, common]) - as.numeric(M_prev[nm, common])))
      out[nm] <- 1 - changed / max(1, prev_deg)
    } else {
      out[nm] <- 1
    }
  }
  out[!is.finite(out)] <- 1
  out
}

# ---- §6.3/§6.4 mask construction (directed -> union -> post-prune) ----
.came_mask_build <- function(P_bar, spec, M_prev = NULL, node_stab_prev = NULL, liq_rank = NULL, chi = 0, delta = 0) {
  syms <- colnames(P_bar)
  n <- length(syms)

  U <- abs(P_bar)
  diag(U) <- 0
  U[!is.finite(U)] <- 0

  if (is.null(liq_rank)) liq_rank <- setNames(rep(0.5, n), syms)
  liq_rank <- liq_rank[syms]
  liq_rank[!is.finite(liq_rank)] <- 0.5

  if (is.null(node_stab_prev)) node_stab_prev <- setNames(rep(1.0, n), syms)
  node_stab_prev <- node_stab_prev[syms]
  node_stab_prev[!is.finite(node_stab_prev)] <- 1.0

  # --- rowwise activation quantile q_on(i)
  # Spec currently provides q_on_base + q_on_chi_scale; we also add small deterministic adjustments for delta and liquidity.
  q_base <- spec$structure$q_on_base %||% 0.90
  q_chi <- spec$structure$q_on_chi_scale %||% 0.10
  q_i <- q_base - q_chi * chi + 0.05 * (liq_rank - 0.5) + 0.05 * (-delta)
  q_i <- pmin(0.995, pmax(0.50, q_i))

  # --- persistence: tau_off = rho_off * tau_on ; here rho_off is constant hysteresis_ratio (allowed special case)
  rho_off <- spec$structure$hysteresis_ratio %||% 0.7
  rho_off <- pmin(0.99, pmax(0.10, rho_off))

  # --- adaptive degree target k_i (architecture §6.3.3; operational deterministic special case)
  k_min <- as.integer(spec$structure$k_min %||% 2L)
  k_max <- as.integer(spec$structure$k_max %||% 10L)
  k_i <- k_min + floor((k_max - k_min) * came_sigmoid(2 * (liq_rank - 0.5) + 1.0 * (node_stab_prev - 0.5) - 1.0 * chi))
  k_i <- pmax(k_min, pmin(k_max, k_i))

  # directed candidate mask
  Mhat <- matrix(FALSE, n, n, dimnames = list(syms, syms))

  for (i in seq_len(n)) {
    s <- U[i, ]
    s[i] <- 0
    vals <- s[is.finite(s)]
    if (length(vals) < 2) next

    tau_on <- came_quantile_safe(vals, q_i[i], default = Inf)
    if (!is.finite(tau_on)) next
    tau_off <- rho_off * tau_on

    eligible_on <- which(is.finite(s) & s >= tau_on)
    eligible <- eligible_on

    if (!is.null(M_prev) && is.matrix(M_prev)) {
      prev_row <- as.logical(M_prev[syms[i], syms])
      keep_prev <- which(prev_row & is.finite(s) & s >= tau_off)
      eligible <- unique(c(eligible_on, keep_prev))
    }

    if (length(eligible) == 0) next

    # top-k_i within eligible
    ord <- eligible[order(s[eligible], decreasing = TRUE)]
    keep <- head(ord, k_i[i])
    Mhat[i, keep] <- TRUE
  }

  # global exception preservation (architecture §6.3.4)
  qg <- spec$structure$q_global %||% 0.995
  uvals <- U[upper.tri(U)]
  uvals <- uvals[is.finite(uvals)]
  if (length(uvals) > 10) {
    thr_g <- came_quantile_safe(uvals, qg, default = NA_real_)
    if (is.finite(thr_g) && thr_g > 0) {
      idx <- which(U >= thr_g, arr.ind = TRUE)
      for (k in seq_len(nrow(idx))) {
        ii <- idx[k, 1]
        jj <- idx[k, 2]
        if (ii != jj) Mhat[ii, jj] <- TRUE
      }
    }
  }

  # union symmetrization (architecture §6.4.1)
  Mu <- Mhat | t(Mhat)
  diag(Mu) <- FALSE

  # post-symmetry pruning (architecture §6.4.2) — simple density band control
  dens_target <- spec$structure$dens_target %||% 0
  dens_target <- if (is.finite(dens_target) && dens_target > 0) dens_target else 0
  if (dens_target > 0 && n >= 3) {
    dens_u <- .came_graph_density(Mu)
    dens_max <- max(dens_target * 1.25, dens_target + 0.01)

    if (is.finite(dens_u) && dens_u > dens_max) {
      # prune weakest edges globally until density <= dens_max
      edges <- which(Mu[upper.tri(Mu)], arr.ind = FALSE)
      # build list of (i,j,u)
      ij <- which(upper.tri(Mu) & Mu, arr.ind = TRUE)
      if (nrow(ij) > 0) {
        w <- U[cbind(ij[, 1], ij[, 2])]
        ord <- order(w, decreasing = FALSE) # remove smallest first
        Mu2 <- Mu
        current_edges <- nrow(ij)
        target_edges <- ceiling(dens_max * (n * (n - 1) / 2))

        if (target_edges < current_edges) {
          remove_n <- current_edges - target_edges
          rem <- ij[ord[seq_len(remove_n)], , drop = FALSE]
          for (r in seq_len(nrow(rem))) {
            i <- rem[r, 1]
            j <- rem[r, 2]
            Mu2[i, j] <- FALSE
            Mu2[j, i] <- FALSE
          }
          Mu <- Mu2
        }
      }
    }
  }

  list(M = Mu, U = U)
}

# ---- §6.5 weights + operators ----
.came_operators <- function(P_bar, M, beta_w) {
  syms <- colnames(P_bar)
  U <- abs(P_bar)
  diag(U) <- 0
  U[!is.finite(U)] <- 0

  bw <- beta_w %||% 1.0
  if (!is.finite(bw) || bw <= 0) bw <- 1.0

  W_abs <- (U^bw) * M
  W_abs <- came_symmetrize(W_abs)
  diag(W_abs) <- 0
  W_abs[!is.finite(W_abs)] <- 0

  # signed weights
  U_pow <- if (abs(bw - 1.0) < 1e-12) matrix(1, nrow(U), ncol(U)) else (U^(bw - 1))
  U_pow[!is.finite(U_pow)] <- 0
  W_sgn <- (P_bar * U_pow) * M
  W_sgn <- came_symmetrize(W_sgn)
  diag(W_sgn) <- 0
  W_sgn[!is.finite(W_sgn)] <- 0

  deg <- rowSums(W_abs)
  deg[!is.finite(deg) | deg <= 0] <- 0
  deg_safe <- pmax(deg, 1e-12)

  A <- W_abs / deg_safe
  A_sgn <- W_sgn / deg_safe

  Dg <- diag(deg_safe)
  L <- Dg - W_abs

  d_inv_sqrt <- 1 / sqrt(deg_safe)
  d_inv_sqrt[!is.finite(d_inv_sqrt)] <- 0
  L_norm <- diag(d_inv_sqrt) %*% L %*% diag(d_inv_sqrt)
  L_norm <- came_symmetrize(L_norm)

  dimnames(W_abs) <- list(syms, syms)
  dimnames(W_sgn) <- list(syms, syms)
  dimnames(A) <- list(syms, syms)
  dimnames(A_sgn) <- list(syms, syms)
  dimnames(L) <- list(syms, syms)
  dimnames(L_norm) <- list(syms, syms)
  names(deg) <- syms

  list(W_abs = W_abs, W_sgn = W_sgn, A = A, A_sgn = A_sgn, L = L, L_norm = L_norm, deg = deg)
}

# ---- clustering (architecture §10) ----
.came_choose_K <- function(evals, K_min, K_max) {
  n <- length(evals)
  K_cand <- min(K_max, n - 1L)
  if (K_cand < K_min) {
    return(K_min)
  }
  gaps <- diff(evals[1:min(n, K_cand + 1L)])
  if (length(gaps) < K_min) {
    return(K_min)
  }
  idx <- which.max(gaps[K_min:length(gaps)]) + K_min - 1L
  max(K_min, min(K_max, idx))
}

.came_spectral_cluster <- function(L_norm, K_min, K_max) {
  n <- ncol(L_norm)
  if (n < K_min) {
    lab <- setNames(rep(1L, n), colnames(L_norm))
    return(list(labels = lab, K = 1L, method = "trivial"))
  }
  eig <- eigen(L_norm, symmetric = TRUE)
  ord <- order(eig$values)
  evals <- eig$values[ord]
  evecs <- eig$vectors[, ord, drop = FALSE]

  K <- .came_choose_K(evals, K_min, K_max)
  V <- evecs[, 1:K, drop = FALSE]
  rn <- sqrt(rowSums(V^2))
  rn[rn <= 1e-12] <- 1
  Vn <- V / rn

  km <- kmeans(Vn, centers = K, nstart = 10, iter.max = 100)
  lab <- setNames(as.integer(km$cluster), colnames(L_norm))
  list(labels = lab, K = K, method = "spectral", within = km$tot.withinss)
}

.came_persist_labels <- function(labels_new, labels_prev) {
  came_require("clue")
  if (is.null(labels_prev)) {
    return(labels_new)
  }

  common <- intersect(names(labels_new), names(labels_prev))
  if (length(common) < 2) {
    return(labels_new)
  }

  K_new <- max(labels_new[common])
  K_prev <- max(labels_prev[common])
  K <- max(K_new, K_prev)

  overlap <- matrix(0, K, K)
  for (nm in common) overlap[labels_new[nm], labels_prev[nm]] <- overlap[labels_new[nm], labels_prev[nm]] + 1

  perm <- as.integer(clue::solve_LSAP(-overlap))
  map <- rep(NA_integer_, K)
  for (col in seq_len(K)) map[perm[col]] <- col

  out <- labels_new
  for (nm in names(out)) {
    c0 <- out[nm]
    if (c0 <= K && !is.na(map[c0])) out[nm] <- map[c0]
  }
  out
}

# ---- main structure update ----
came_structure_update <- function(Theta, traded_value_last, state, spec) {
  P_t <- .came_partial_corr(Theta)
  univ <- colnames(P_t)

  # previous states mapped (runner should call came_state_pi, but we defend by mapping here too)
  P_bar_prev <- state$structure$P_bar
  if (!is.null(P_bar_prev)) P_bar_prev <- came_pi_matrix(P_bar_prev, univ, init_diag = 0)

  M_prev <- state$structure$M_prev
  if (!is.null(M_prev)) M_prev <- came_pi_mask(M_prev, univ)

  edge_prev <- state$structure$edge_stab
  if (!is.null(edge_prev)) edge_prev <- came_pi_matrix(edge_prev, univ, init_diag = 0.5)

  node_prev <- state$structure$node_stab
  if (!is.null(node_prev)) node_prev <- came_pi_vector(node_prev, univ, init_val = 1.0)

  chi <- .came_structural_shock(P_t, P_bar_prev)

  lambda_edge <- spec$structure$lambda_edge %||% 0.95
  if (!is.finite(lambda_edge) || lambda_edge <= 0 || lambda_edge >= 1) came_stop("struct_lambda_edge", "structure$lambda_edge must be in (0,1)")
  edge_stab <- .came_edge_stability(edge_prev, P_t, P_bar_prev, univ, lambda_edge)

  # adaptive smoothing (architecture §6.2)
  if (is.null(P_bar_prev)) {
    P_bar <- P_t
  } else {
    theta <- spec$structure$theta_alpha
    alpha_mat <- .came_alpha_matrix(P_t, P_bar_prev, edge_stab, chi, theta)
    P_bar <- .came_smooth_P(P_t, P_bar_prev, alpha_mat)
  }

  # density deviation delta_t (architecture §6.6): dens_{t-1} - dens_target (lagged is causal)
  dens_prev <- if (!is.null(M_prev)) .came_graph_density(M_prev) else 0
  dens_target <- spec$structure$dens_target %||% 0.05
  if (!is.finite(dens_target) || dens_target <= 0) dens_target <- 0.05
  delta <- dens_prev - dens_target

  # build mask
  names(traded_value_last) <- names(traded_value_last)
  tv_vec <- traded_value_last[univ]
  tv_vec[!is.finite(tv_vec)] <- 0

  liq_rank <- .came_liquidity_rank(tv_vec)

  mask_res <- .came_mask_build(
    P_bar = P_bar,
    spec = spec,
    M_prev = M_prev,
    node_stab_prev = node_prev,
    liq_rank = liq_rank,
    chi = chi,
    delta = delta
  )
  M <- mask_res$M

  # diagnostics (architecture §6.6)
  dens <- .came_graph_density(M)
  eto <- .came_edge_turnover(M, M_prev)
  node_stab <- .came_node_stability(M, M_prev)

  # operators (architecture §6.5)
  beta_w <- spec$structure$beta_w %||% 1.0
  ops <- .came_operators(P_bar, M, beta_w = beta_w)

  # clustering (architecture §10) + persistence
  cl <- .came_spectral_cluster(ops$L_norm, spec$structure$K_min, spec$structure$K_max)
  labels_prev <- state$structure$labels
  if (!is.null(labels_prev)) {
    labels_prev <- labels_prev[names(labels_prev) %||% names(cl$labels)]
  }
  cl$labels <- .came_persist_labels(cl$labels, labels_prev)

  # state update
  st_out <- state
  st_out$structure$P_bar <- P_bar
  st_out$structure$M_prev <- M
  st_out$structure$edge_stab <- edge_stab
  st_out$structure$node_stab <- node_stab
  st_out$structure$labels <- as.integer(cl$labels)

  list(
    structure = list(
      P = P_t,
      P_bar = P_bar,
      M = M,
      ops = ops,
      clustering = cl,
      node_stab = node_stab,
      diag = list(
        chi = chi,
        density = dens,
        eto = eto,
        delta = delta,
        dens_prev = dens_prev,
        dens_target = dens_target
      )
    ),
    state_out = st_out
  )
}
