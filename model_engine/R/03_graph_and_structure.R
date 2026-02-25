#' @title Model Engine — Graph and Structure
#' @description Partial correlations, graph mask, graph operators, spectral clustering.
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
    # Force symmetry
    P <- (P + t(P)) / 2
    dimnames(P) <- dimnames(Theta_eps)
    P
}

#' @export
me_smooth_partial_corr <- function(P_new, P_prev, alpha = 0.3) {
    if (is.null(P_prev)) {
        return(P_new)
    }

    # Require named square matrices
    if (!is.matrix(P_new) || !is.matrix(P_prev) ||
        nrow(P_new) != ncol(P_new) || nrow(P_prev) != ncol(P_prev) ||
        is.null(rownames(P_new)) || is.null(colnames(P_new)) ||
        is.null(rownames(P_prev)) || is.null(colnames(P_prev))) {
        return(P_new)
    }

    syms_new <- colnames(P_new)
    syms_prev <- colnames(P_prev)

    # Exact alignment if same universe/order
    if (identical(syms_new, syms_prev) && identical(rownames(P_new), syms_new) && identical(rownames(P_prev), syms_prev)) {
        P_bar <- (1 - alpha) * P_prev + alpha * P_new
        P_bar <- (P_bar + t(P_bar)) / 2
        diag(P_bar) <- 0
        return(P_bar)
    }

    # Universe/order changed → smooth only on intersection, keep new values elsewhere
    common <- intersect(syms_new, syms_prev)
    if (length(common) < 2) {
        return(P_new)
    }

    P_bar <- P_new
    P_bar[common, common] <- (1 - alpha) * P_prev[common, common, drop = FALSE] +
        alpha * P_new[common, common, drop = FALSE]
    P_bar <- (P_bar + t(P_bar)) / 2
    diag(P_bar) <- 0
    P_bar
}

#' @export
me_build_graph_mask <- function(P_bar, spec_graph = list()) {
    # Architecture §6: activation + top-k + symmetry
    activation_thr <- spec_graph$activation_thr %||% 0.05
    top_k <- as.integer(spec_graph$top_k %||% 10L)
    top_k <- max(1L, top_k)

    n <- ncol(P_bar)
    syms <- colnames(P_bar)

    W_abs_full <- abs(P_bar)
    diag(W_abs_full) <- 0

    M_dir <- matrix(FALSE, n, n, dimnames = list(syms, syms))

    for (i in seq_len(n)) {
        scores <- W_abs_full[i, ]
        scores[i] <- 0

        eligible <- which(is.finite(scores) & (scores > activation_thr))
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

    # Preserve both signed and abs weighted adjacency
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
        W = W_abs, # backward-compatible alias
        W_abs = W_abs,
        W_signed = W_signed,
        n_edges = sum(M) / 2,
        density = if (n > 1) sum(M) / (n * (n - 1)) else 0
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
    # W should be nonnegative weighted adjacency (for Laplacian/clustering)
    n <- ncol(W)
    syms <- colnames(W)

    # Unsigned row-normalized adjacency
    deg <- rowSums(W)
    deg[deg <= 0 | !is.finite(deg)] <- 1e-12
    A <- W / deg

    # Optional signed adjacency normalized by row abs-sum
    A_signed <- NULL
    if (!is.null(W_signed)) {
        denom_s <- rowSums(abs(W_signed))
        denom_s[denom_s <= 0 | !is.finite(denom_s)] <- 1e-12
        A_signed <- W_signed / denom_s
        dimnames(A_signed) <- list(syms, syms)
    }

    # Degree + Laplacian from unsigned graph (PSD-friendly)
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

#' §9.1 Peer-context transform: G_peer[s] = A * s
#' @export
me_graph_peer <- function(A, s) {
    .me_named_graph_mv(A, s)
}

#' §9.2 Relative/dislocation transform: G_rel[s] = s - A * s
#' @export
me_graph_relative <- function(A, s) {
    peer <- .me_named_graph_mv(A, s)
    s_aligned <- s[names(peer)]
    s_aligned[!is.finite(s_aligned)] <- 0
    out <- s_aligned - peer
    names(out) <- names(peer)
    out
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

    # Eigendecomposition of L_norm
    eig <- tryCatch(
        eigen(L_norm, symmetric = TRUE),
        error = function(e) NULL
    )

    if (is.null(eig)) {
        labels <- rep(1L, n)
        names(labels) <- colnames(L_norm)
        return(list(labels = labels, K = 1L, method = "eigen_failed"))
    }

    # Sort eigenvalues ascending (smallest first for Laplacian)
    ord <- order(eig$values)
    evals <- eig$values[ord]
    evecs <- eig$vectors[, ord]

    # Eigengap heuristic for K selection
    K_cand <- min(K_max, n - 1)
    if (K_cand < K_min) K_cand <- K_min

    gaps <- diff(evals[1:min(K_cand + 1, n)])
    if (length(gaps) >= K_min) {
        K <- which.max(gaps[K_min:length(gaps)]) + K_min - 1
        K <- max(K_min, min(K, K_max))
    } else {
        K <- K_min
    }

    # Use first K eigenvectors
    V <- evecs[, 1:K, drop = FALSE]
    # Row-normalize
    row_norms <- sqrt(rowSums(V^2))
    row_norms[row_norms < 1e-10] <- 1
    V_norm <- V / row_norms

    # K-means clustering
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

    # Build overlap matrix
    overlap <- matrix(0, K, K)
    for (i in common) {
        c_new <- labels_new[i]
        c_prev <- labels_prev[i]
        if (c_new <= K && c_prev <= K) {
            overlap[c_new, c_prev] <- overlap[c_new, c_prev] + 1
        }
    }

    # Greedy best-match relabeling (Hungarian approximation)
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

    # Apply mapping
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

#' Cluster-centered signal: s_i - mean(s in same cluster)
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

#' Cluster-z signal: (s_i - cluster_mean) / cluster_sd
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
# Full graph pipeline orchestrator
# ══════════════════════════════════════════════════════════════════════════════

#' @export
me_run_graph_pipeline <- function(risk_artifact, spec_graph = list(),
                                  prev_P_bar = NULL, prev_labels = NULL) {
    Theta <- risk_artifact$Theta_eps
    if (is.null(Theta) || ncol(Theta) < 3) {
        n <- length(risk_artifact$w_hrp)
        syms <- names(risk_artifact$w_hrp)
        return(list(
            P = NULL, P_bar = NULL, mask = NULL, operators = NULL,
            clustering = list(labels = setNames(rep(1L, n), syms), K = 1L),
            diag = list(skipped = TRUE, reason = "no_precision_matrix")
        ))
    }

    # 1. Partial correlations
    P <- me_partial_corr_from_precision(Theta)

    # 2. Smooth with previous
    alpha <- spec_graph$smoothing_alpha %||% 0.3
    P_bar <- me_smooth_partial_corr(P, prev_P_bar, alpha)

    # 3. Build graph mask
    mask_result <- me_build_graph_mask(P_bar, spec_graph)

    # 4. Graph operators
    ops <- me_graph_operators(mask_result$W_abs %||% mask_result$W, mask_result$W_signed)

    # 5. Spectral clustering
    K_min <- spec_graph$K_min %||% 2L
    K_max <- spec_graph$K_max %||% 8L
    clust <- me_spectral_clustering(ops$L_norm, K_min, K_max)

    # 6. Label persistence
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
            cluster_method = clust$method
        )
    )
}
