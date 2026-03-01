#' @title Model Engine — Graph and Structure
#' @description Partial correlations, adaptive smoothing, persistence-aware graph mask,
#' graph operators, spectral clustering, graph diagnostics.
#' Implements architecture.md §§5-6 (partial corr → graph), §9 (operators), §10 (clustering).

# ══════════════════════════════════════════════════════════════════════════════
# §5-6: Partial correlation → graph construction
# ══════════════════════════════════════════════════════════════════════════════

#' @export
me_partial_corr_from_precision <- function(Theta_eps) {
    # P_ij = -Theta_ij / sqrt(Theta_ii * Theta_jj)
    n <- ncol(Theta_eps)
    d <- sqrt(diag(Theta_eps))
    d[d <= 0 | !is.finite(d)] <- 1e-8
    P <- -Theta_eps / outer(d, d)
    diag(P) <- 0
    P <- (P + t(P)) / 2
    dimnames(P) <- dimnames(Theta_eps)
    P
}

# ── §6.2 Structural shock ──────────────────────────────────────────────────

#' @export
me_structural_shock <- function(P_new, P_prev) {
    # χ_t = ||P_t - P_{t-1}||_F / sqrt(p*(p-1))
    if (is.null(P_prev)) {
        return(0)
    }
    p <- ncol(P_new)
    if (p < 2) {
        return(0)
    }
    common <- intersect(colnames(P_new), colnames(P_prev))
    if (length(common) < 2) {
        return(0)
    }
    diff_mat <- P_new[common, common] - P_prev[common, common]
    norm_F <- sqrt(sum(diff_mat^2, na.rm = TRUE))
    denom <- sqrt(length(common) * (length(common) - 1))
    if (denom <= 0) {
        return(0)
    }
    chi <- norm_F / denom
    if (!is.finite(chi)) 0 else chi
}

# ── §6.2 Edge stability state ──────────────────────────────────────────────

#' @export
me_edge_stability_update <- function(edge_stab_prev, P_new, P_bar_prev,
                                     new_univ, lambda_s = 0.95) {
    # s^edge_{ij,t} = lambda_s * s^edge_{ij,t-1} + (1-lambda_s) * 1[sign match]
    p <- length(new_univ)
    if (is.null(edge_stab_prev) || is.null(P_bar_prev) || p < 2) {
        out <- matrix(0.5, p, p, dimnames = list(new_univ, new_univ))
        diag(out) <- 1
        return(out)
    }

    # Map previous stability to current universe
    s_prev <- me_pi_map_matrix(edge_stab_prev, new_univ, init_diag = 0.5)
    common <- intersect(colnames(P_new), colnames(P_bar_prev))
    common <- intersect(common, new_univ)

    out <- s_prev
    if (length(common) >= 2) {
        sign_match <- sign(P_new[common, common]) == sign(P_bar_prev[common, common])
        sign_match[!is.finite(sign_match)] <- TRUE
        diag(sign_match) <- TRUE
        out[common, common] <- lambda_s * s_prev[common, common] +
            (1 - lambda_s) * as.numeric(sign_match)
    }
    diag(out) <- 1
    out <- (out + t(out)) / 2
    out
}

# ── §6.2 Adaptive smoothing of partial correlations ──────────────────────────

#' @export
me_smooth_partial_corr <- function(P_new, P_prev, alpha = 0.3,
                                   edge_stability = NULL, chi_t = 0,
                                   spec_graph = list()) {
    if (is.null(P_prev)) {
        return(P_new)
    }

    if (!is.matrix(P_new) || !is.matrix(P_prev) ||
        nrow(P_new) != ncol(P_new) || nrow(P_prev) != ncol(P_prev) ||
        is.null(rownames(P_new)) || is.null(colnames(P_new)) ||
        is.null(rownames(P_prev)) || is.null(colnames(P_prev))) {
        return(P_new)
    }

    syms_new <- colnames(P_new)
    syms_prev <- colnames(P_prev)
    common <- intersect(syms_new, syms_prev)
    if (length(common) < 2) {
        return(P_new)
    }

    use_adaptive <- isTRUE(spec_graph$adaptive_smoothing %||% TRUE)

    if (use_adaptive && !is.null(edge_stability)) {
        # §6.2 Architecture adaptive smoothing scaffold
        # α_{ij,t} = σ(θ_α' z) where z = [1, s^edge, χ, ξ]
        # Reduced-covariate version: α = σ(θ0 + θ1*s_edge + θ2*χ)
        theta0 <- spec_graph$smooth_theta0 %||% 0 # intercept → base alpha
        theta1 <- spec_graph$smooth_theta1 %||% 1.0 # edge stability: higher → more smoothing
        theta2 <- spec_graph$smooth_theta2 %||% -0.5 # shock: higher → less smoothing (more responsive)

        P_bar <- P_new
        for (i in seq_len(nrow(P_new))) {
            for (j in seq_len(ncol(P_new))) {
                if (i == j) next
                si <- syms_new[i]
                sj <- syms_new[j]
                if (!(si %in% common && sj %in% common)) next

                s_ij <- if (!is.null(edge_stability) && si %in% rownames(edge_stability) &&
                    sj %in% colnames(edge_stability)) {
                    edge_stability[si, sj]
                } else {
                    0.5
                }

                xi_ij <- abs(P_new[i, j] - P_prev[si, sj])
                z <- theta0 + theta1 * s_ij + theta2 * chi_t
                alpha_ij <- 1 / (1 + exp(-z)) # sigmoid → (0,1), high = more smoothing
                P_bar[i, j] <- alpha_ij * P_prev[si, sj] + (1 - alpha_ij) * P_new[i, j]
            }
        }
        P_bar <- (P_bar + t(P_bar)) / 2
        diag(P_bar) <- 0
        return(P_bar)
    }

    # Static smoothing fallback
    if (identical(syms_new, syms_prev) && identical(rownames(P_new), syms_new) &&
        identical(rownames(P_prev), syms_prev)) {
        P_bar <- (1 - alpha) * P_prev + alpha * P_new
        P_bar <- (P_bar + t(P_bar)) / 2
        diag(P_bar) <- 0
        return(P_bar)
    }

    P_bar <- P_new
    P_bar[common, common] <- (1 - alpha) * P_prev[common, common, drop = FALSE] +
        alpha * P_new[common, common, drop = FALSE]
    P_bar <- (P_bar + t(P_bar)) / 2
    diag(P_bar) <- 0
    P_bar
}

# ── §6.3 Persistence-aware graph mask with hysteresis ────────────────────────

#' @export
me_build_graph_mask <- function(P_bar, spec_graph = list(), prev_M = NULL) {
    activation_thr <- spec_graph$activation_thr %||% 0.05
    top_k <- as.integer(spec_graph$top_k %||% 10L)
    top_k <- max(1L, top_k)
    hysteresis_ratio <- spec_graph$hysteresis_ratio %||% 0.6 # off threshold = ratio * on threshold

    n <- ncol(P_bar)
    syms <- colnames(P_bar)

    W_abs_full <- abs(P_bar)
    diag(W_abs_full) <- 0

    M_dir <- matrix(FALSE, n, n, dimnames = list(syms, syms))

    for (i in seq_len(n)) {
        scores <- W_abs_full[i, ]
        scores[i] <- 0

        # §6.3.2 Hysteresis: lower threshold for existing edges
        thr_vec <- rep(activation_thr, n)
        if (!is.null(prev_M) && is.matrix(prev_M)) {
            common_mask <- intersect(syms, colnames(prev_M))
            if (length(common_mask) > 0 && syms[i] %in% rownames(prev_M)) {
                for (j_nm in common_mask) {
                    if (isTRUE(prev_M[syms[i], j_nm])) {
                        j_idx <- which(syms == j_nm)
                        if (length(j_idx) == 1) {
                            thr_vec[j_idx] <- activation_thr * hysteresis_ratio
                        }
                    }
                }
            }
        }

        eligible <- which(is.finite(scores) & (scores > thr_vec))
        if (length(eligible) == 0) next

        if (length(eligible) > top_k) {
            ord <- eligible[order(scores[eligible], decreasing = TRUE)]
            keep <- ord[seq_len(top_k)]
        } else {
            keep <- eligible
        }
        M_dir[i, keep] <- TRUE
    }

    # Symmetry by union
    M <- M_dir | t(M_dir)
    diag(M) <- FALSE

    W_signed <- P_bar * M
    W_signed <- (W_signed + t(W_signed)) / 2
    diag(W_signed) <- 0

    W_abs <- abs(W_signed)
    W_abs <- (W_abs + t(W_abs)) / 2
    diag(W_abs) <- 0

    dimnames(M) <- list(syms, syms)
    dimnames(W_signed) <- list(syms, syms)
    dimnames(W_abs) <- list(syms, syms)

    list(
        M = M,
        W = W_abs,
        W_abs = W_abs,
        W_signed = W_signed,
        n_edges = sum(M) / 2,
        density = if (n > 1) sum(M) / (n * (n - 1)) else 0
    )
}

# ── §6.6 Graph diagnostics ──────────────────────────────────────────────────

#' @export
me_graph_diagnostics <- function(M, prev_M = NULL, density_target = NULL) {
    n <- ncol(M)
    syms <- colnames(M)

    # Density
    dens <- if (n > 1) sum(M) / (n * (n - 1)) else 0

    # Edge turnover
    eto <- 0
    if (!is.null(prev_M) && is.matrix(prev_M)) {
        common <- intersect(colnames(M), colnames(prev_M))
        if (length(common) >= 2) {
            prev_edges <- sum(prev_M[common, common]) / 2
            if (prev_edges > 0) {
                overlap <- sum(M[common, common] & prev_M[common, common]) / 2
                eto <- 1 - overlap / prev_edges
            }
        }
    }

    # Node stability
    node_stab <- setNames(rep(1, n), syms)
    if (!is.null(prev_M) && is.matrix(prev_M)) {
        for (i in seq_len(n)) {
            nm <- syms[i]
            if (nm %in% rownames(prev_M)) {
                common_j <- intersect(syms, colnames(prev_M))
                if (length(common_j) > 0) {
                    prev_deg <- sum(prev_M[nm, common_j])
                    if (prev_deg > 0) {
                        changed <- sum(abs(as.numeric(M[nm, common_j]) -
                            as.numeric(prev_M[nm, common_j])))
                        node_stab[nm] <- 1 - changed / max(1, prev_deg)
                    }
                }
            }
        }
    }

    # Density deviation
    dens_dev <- if (!is.null(density_target)) dens - density_target else 0

    list(
        density = dens,
        edge_turnover = eto,
        node_stability = node_stab,
        density_deviation = dens_dev
    )
}

# ══════════════════════════════════════════════════════════════════════════════
# §9: Graph operators
# ══════════════════════════════════════════════════════════════════════════════

#' @keywords internal
.me_named_graph_mv <- function(M, s) {
    out <- as.vector(M %*% s)
    nm <- rownames(M)
    if (is.null(nm) && !is.null(names(s))) nm <- names(s)
    names(out) <- nm
    out
}

#' @export
me_graph_operators <- function(W, W_signed = NULL) {
    n <- ncol(W)
    syms <- colnames(W)

    # Unsigned row-normalized adjacency (§6.5)
    deg <- rowSums(W)
    deg[deg <= 0 | !is.finite(deg)] <- 1e-12
    A <- W / deg

    # Signed adjacency normalized by absolute row-sum of unsigned (§6.5)
    A_signed <- NULL
    if (!is.null(W_signed)) {
        A_signed <- W_signed / deg # Normalize by unsigned degree (architecture §6.5)
        dimnames(A_signed) <- list(syms, syms)
    }

    D <- diag(deg)
    L <- D - W

    d_inv_sqrt <- 1 / sqrt(deg)
    d_inv_sqrt[!is.finite(d_inv_sqrt)] <- 0
    D_inv_sqrt <- diag(d_inv_sqrt)
    L_norm <- D_inv_sqrt %*% L %*% D_inv_sqrt
    L_norm <- (L_norm + t(L_norm)) / 2

    dimnames(A) <- list(syms, syms)
    dimnames(L) <- list(syms, syms)
    dimnames(L_norm) <- list(syms, syms)
    dimnames(D) <- list(syms, syms)
    names(deg) <- syms

    list(A = A, A_signed = A_signed, L = L, L_norm = L_norm, D = D, deg = deg)
}

#' §9.1 Peer-context transform: G_peer[s] = A * s (UNSIGNED A)
#' @export
me_graph_peer <- function(A, s) {
    .me_named_graph_mv(A, s)
}

#' §9.2 Relative/dislocation transform: G_rel[s] = s - A * s (UNSIGNED A)
#' @export
me_graph_relative <- function(A, s) {
    peer <- .me_named_graph_mv(A, s)
    s_aligned <- s[names(peer)]
    s_aligned[!is.finite(s_aligned)] <- 0
    out <- s_aligned - peer
    names(out) <- names(peer)
    out
}

#' §9.3 Signed-neighborhood transform: G_sgn[s] = A_signed * s
#' @export
me_graph_signed <- function(A_signed, s) {
    .me_named_graph_mv(A_signed, s)
}

#' §9.4 Tension/local incompatibility: G_ten[s] = L * s
#' @export
me_graph_tension <- function(L, s) {
    .me_named_graph_mv(L, s)
}

#' §9.5 Graph-regularized shrinkage: G_shr[s] = (I + lambda * L_norm)^{-1} * s
#' @export
me_graph_shrinkage <- function(L_norm, s, lambda_g = 0.1) {
    n <- length(s)
    M <- diag(n) + lambda_g * L_norm
    out <- tryCatch(as.vector(solve(M, s)),
        error = function(e) as.vector(s)
    )
    nm <- rownames(L_norm)
    if (is.null(nm)) nm <- names(s)
    names(out) <- nm
    out
}

#' §9.6 One-step mixed propagation: G_mix[s] = ((1-nu)*I + nu*A) * s
#' @export
me_graph_mixed <- function(A, s, nu = 0.5) {
    n <- length(s)
    M <- (1 - nu) * diag(n) + nu * A
    out <- as.vector(M %*% s)
    nm <- rownames(A)
    if (is.null(nm)) nm <- names(s)
    names(out) <- nm
    out
}

# ══════════════════════════════════════════════════════════════════════════════
# §10: Spectral clustering with label persistence
# ══════════════════════════════════════════════════════════════════════════════

#' @export
me_spectral_clustering <- function(L_norm, K_min = 2L, K_max = 8L) {
    n <- ncol(L_norm)
    if (n < K_min) {
        labels <- rep(1L, n)
        names(labels) <- colnames(L_norm)
        return(list(labels = labels, K = 1L, method = "trivial"))
    }

    eig <- tryCatch(
        eigen(L_norm, symmetric = TRUE),
        error = function(e) NULL
    )

    if (is.null(eig)) {
        labels <- rep(1L, n)
        names(labels) <- colnames(L_norm)
        return(list(labels = labels, K = 1L, method = "eigen_failed"))
    }

    ord <- order(eig$values)
    evals <- eig$values[ord]
    evecs <- eig$vectors[, ord]

    K_cand <- min(K_max, n - 1)
    if (K_cand < K_min) K_cand <- K_min

    gaps <- diff(evals[1:min(K_cand + 1, n)])
    if (length(gaps) >= K_min) {
        K <- which.max(gaps[K_min:length(gaps)]) + K_min - 1
        K <- max(K_min, min(K, K_max))
    } else {
        K <- K_min
    }

    V <- evecs[, 1:K, drop = FALSE]
    row_norms <- sqrt(rowSums(V^2))
    row_norms[row_norms < 1e-10] <- 1
    V_norm <- V / row_norms

    km <- tryCatch(
        kmeans(V_norm, centers = K, nstart = 10, iter.max = 100),
        error = function(e) NULL
    )

    if (is.null(km)) {
        labels <- rep(1L, n)
        names(labels) <- colnames(L_norm)
        return(list(labels = labels, K = 1L, method = "kmeans_failed"))
    }

    labels <- km$cluster
    names(labels) <- colnames(L_norm)

    list(
        labels = labels, K = K, method = "spectral",
        eigengap = gaps, within_ss = km$tot.withinss
    )
}

#' §10.2 Cluster label persistence via Hungarian matching
#' @export
me_persist_cluster_labels <- function(labels_new, labels_prev) {
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
    for (i in common) {
        c_new <- labels_new[i]
        c_prev <- labels_prev[i]
        if (c_new <= K && c_prev <= K) {
            overlap[c_new, c_prev] <- overlap[c_new, c_prev] + 1
        }
    }

    mapping <- rep(NA_integer_, K)
    used <- rep(FALSE, K)
    for (iter in seq_len(K)) {
        best_val <- -1
        best_i <- 1
        best_j <- 1
        for (i in seq_len(K)) {
            if (!is.na(mapping[i])) next
            for (j in seq_len(K)) {
                if (used[j]) next
                if (overlap[i, j] > best_val) {
                    best_val <- overlap[i, j]
                    best_i <- i
                    best_j <- j
                }
            }
        }
        mapping[best_i] <- best_j
        used[best_j] <- TRUE
    }

    out <- labels_new
    for (i in seq_along(out)) {
        old <- out[i]
        if (old <= K && !is.na(mapping[old])) {
            out[i] <- mapping[old]
        }
    }
    out
}

# ══════════════════════════════════════════════════════════════════════════════
# §10.3 Cluster transforms of signals
# ══════════════════════════════════════════════════════════════════════════════

#' @export
me_cluster_center <- function(s, labels) {
    out <- s
    for (k in unique(labels)) {
        idx <- names(labels)[labels == k]
        idx <- intersect(idx, names(s))
        if (length(idx) > 0) {
            out[idx] <- s[idx] - mean(s[idx], na.rm = TRUE)
        }
    }
    out
}

#' @export
me_cluster_z <- function(s, labels, eps = 1e-8) {
    out <- s
    for (k in unique(labels)) {
        idx <- names(labels)[labels == k]
        idx <- intersect(idx, names(s))
        if (length(idx) > 1) {
            mu <- mean(s[idx], na.rm = TRUE)
            sigma <- sd(s[idx], na.rm = TRUE)
            if (!is.finite(sigma) || sigma < eps) sigma <- eps
            out[idx] <- (s[idx] - mu) / sigma
        } else if (length(idx) == 1) {
            out[idx] <- 0
        }
    }
    out
}

# ══════════════════════════════════════════════════════════════════════════════
# Full graph pipeline orchestrator (architecture-aligned)
# ══════════════════════════════════════════════════════════════════════════════

#' @export
me_run_graph_pipeline <- function(risk_artifact, spec_graph = list(),
                                  prev_P_bar = NULL, prev_labels = NULL,
                                  model_state = NULL) {
    Theta <- risk_artifact$Theta_eps

    if (is.null(Theta) || ncol(Theta) < 3) {
        syms <- NULL
        if (!is.null(Theta) && is.matrix(Theta)) syms <- colnames(Theta)
        if (is.null(syms) || length(syms) == 0) {
            Sigma_ref <- risk_artifact$Sigma_risk_1 %||% risk_artifact$Sigma_total
            if (!is.null(Sigma_ref) && is.matrix(Sigma_ref)) syms <- colnames(Sigma_ref)
        }
        if ((is.null(syms) || length(syms) == 0) && !is.null(risk_artifact$w_baseline)) {
            syms <- names(risk_artifact$w_baseline)
        }
        if ((is.null(syms) || length(syms) == 0) && !is.null(risk_artifact$w_hrp)) {
            syms <- names(risk_artifact$w_hrp)
        }

        syms <- syms %||% character(0)
        n <- length(syms)

        return(list(
            P = NULL, P_bar = NULL, mask = NULL, operators = NULL,
            clustering = list(labels = setNames(rep(1L, n), syms), K = if (n > 0) 1L else 0L),
            diag = list(skipped = TRUE, reason = "no_precision_matrix"),
            graph_state_out = list()
        ))
    }

    new_univ <- colnames(Theta)

    # Extract recursive states
    edge_stab_prev <- if (!is.null(model_state)) model_state$edge_stability else NULL
    prev_M <- if (!is.null(model_state)) model_state$prev_M else NULL

    # 1. Partial correlations
    P <- me_partial_corr_from_precision(Theta)

    # 2. Structural shock (§6.2)
    chi_t <- me_structural_shock(P, prev_P_bar)

    # 3. Edge stability update (§6.2)
    edge_stab <- me_edge_stability_update(
        edge_stab_prev, P, prev_P_bar, new_univ,
        lambda_s = spec_graph$lambda_s %||% 0.95
    )

    # 4. Adaptive smooth with previous (§6.2)
    alpha <- spec_graph$smoothing_alpha %||% 0.3
    P_bar <- me_smooth_partial_corr(P, prev_P_bar, alpha,
        edge_stability = edge_stab,
        chi_t = chi_t,
        spec_graph = spec_graph
    )

    # 5. Build graph mask (with hysteresis)
    mask_result <- me_build_graph_mask(P_bar, spec_graph, prev_M = prev_M)

    # 6. Graph operators
    ops <- me_graph_operators(mask_result$W_abs %||% mask_result$W, mask_result$W_signed)

    # 7. Graph diagnostics (§6.6)
    graph_diag <- me_graph_diagnostics(
        mask_result$M, prev_M,
        density_target = spec_graph$density_target
    )

    # 8. Spectral clustering
    K_min <- spec_graph$K_min %||% 2L
    K_max <- spec_graph$K_max %||% 8L
    clust <- me_spectral_clustering(ops$L_norm, K_min, K_max)

    # 9. Label persistence
    clust$labels <- me_persist_cluster_labels(clust$labels, prev_labels)

    list(
        P = P,
        P_bar = P_bar,
        mask = mask_result,
        operators = ops,
        clustering = clust,
        diag = list(
            n_edges = mask_result$n_edges,
            density = mask_result$density,
            K_clusters = clust$K,
            cluster_method = clust$method,
            chi_t = chi_t,
            edge_turnover = graph_diag$edge_turnover,
            density_deviation = graph_diag$density_deviation
        ),
        # Recursive state outputs
        graph_state_out = list(
            edge_stability = edge_stab,
            node_stability = graph_diag$node_stability,
            prev_M = mask_result$M,
            chi_t = chi_t
        )
    )
}
