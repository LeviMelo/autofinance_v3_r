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
    # P_bar_t = (1 - alpha) * P_bar_{t-1} + alpha * P_t
    if (is.null(P_prev)) {
        return(P_new)
    }
    if (!all(dim(P_new) == dim(P_prev))) {
        return(P_new)
    } # universe changed
    P_bar <- (1 - alpha) * P_prev + alpha * P_new
    P_bar <- (P_bar + t(P_bar)) / 2
    diag(P_bar) <- 0
    P_bar
}

#' @export
me_build_graph_mask <- function(P_bar, spec_graph = list()) {
    # Architecture §6: activation + persistence + top-k + symmetry
    activation_thr <- spec_graph$activation_thr %||% 0.05
    top_k <- spec_graph$top_k %||% 10L
    n <- ncol(P_bar)
    syms <- colnames(P_bar)

    # Absolute partial correlations
    W_abs <- abs(P_bar)

    # Activation mask: |P_bar_ij| > threshold
    M <- W_abs > activation_thr

    # Top-k enforcement: each node keeps at most top_k neighbors
    for (i in seq_len(n)) {
        scores <- W_abs[i, ]
        scores[i] <- 0
        if (sum(scores > 0) > top_k) {
            cutoff <- sort(scores, decreasing = TRUE)[top_k]
            M[i, scores < cutoff] <- FALSE
        }
    }

    # Symmetrize (union: if either direction active, keep)
    M <- M | t(M)
    diag(M) <- FALSE

    # Build weighted adjacency
    W <- W_abs * M
    W <- (W + t(W)) / 2
    diag(W) <- 0
    dimnames(W) <- list(syms, syms)

    list(M = M, W = W, n_edges = sum(M) / 2, density = sum(M) / (n * (n - 1)))
}

# ══════════════════════════════════════════════════════════════════════════════
# §9: Graph operators
# ══════════════════════════════════════════════════════════════════════════════

#' @export
me_graph_operators <- function(W) {
    # Build all canonical graph operators from weighted adjacency
    n <- ncol(W)
    syms <- colnames(W)

    # Row-normalized adjacency A_t
    deg <- rowSums(W)
    deg[deg <= 0] <- 1e-12
    A <- W / deg

    # Signed adjacency (normalize by sum of absolutes per row)
    P_signed <- W # W is already |P_bar| * M, need original P_bar for signs
    # We'll use A as the unsigned normalized operator

    # Degree matrix and Laplacian
    D <- diag(deg)
    L <- D - W

    # Normalized Laplacian: L_norm = D^{-1/2} L D^{-1/2}
    d_inv_sqrt <- 1 / sqrt(deg)
    d_inv_sqrt[!is.finite(d_inv_sqrt)] <- 0
    D_inv_sqrt <- diag(d_inv_sqrt)
    L_norm <- D_inv_sqrt %*% L %*% D_inv_sqrt
    L_norm <- (L_norm + t(L_norm)) / 2

    dimnames(A) <- list(syms, syms)
    dimnames(L) <- list(syms, syms)
    dimnames(L_norm) <- list(syms, syms)

    list(A = A, L = L, L_norm = L_norm, D = D, deg = deg)
}

#' §9.1 Peer-context transform: G_peer[s] = A * s
#' @export
me_graph_peer <- function(A, s) {
    as.vector(A %*% s)
}

#' §9.2 Relative/dislocation transform: G_rel[s] = s - A * s
#' @export
me_graph_relative <- function(A, s) {
    s - as.vector(A %*% s)
}

#' §9.4 Tension/local incompatibility: G_ten[s] = L * s
#' @export
me_graph_tension <- function(L, s) {
    as.vector(L %*% s)
}

#' §9.5 Graph-regularized shrinkage: G_shr[s] = (I + lambda * L_norm)^{-1} * s
#' @export
me_graph_shrinkage <- function(L_norm, s, lambda_g = 0.1) {
    n <- length(s)
    M <- diag(n) + lambda_g * L_norm
    tryCatch(as.vector(solve(M, s)),
        error = function(e) s
    ) # fallback to identity
}

#' §9.6 One-step mixed propagation: G_mix[s] = ((1-nu)*I + nu*A) * s
#' @export
me_graph_mixed <- function(A, s, nu = 0.5) {
    n <- length(s)
    M <- (1 - nu) * diag(n) + nu * A
    as.vector(M %*% s)
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
    ops <- me_graph_operators(mask_result$W)

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
