#' @title Model Engine — Feature Engine
#' @description Feature assembly: temporal, structural, graph, liquidity, state.
#' Implements architecture.md §11: complete feature vector construction.

# ══════════════════════════════════════════════════════════════════════════════
# §11.1 Temporal signal features (from signal engine outputs)
# ══════════════════════════════════════════════════════════════════════════════

#' @export
me_features_temporal <- function(scalars, risk_artifact) {
    # scalars = list(s_mom, s_kal, s_fac) from scalarization
    # All are already in [-1,1] via tanh. Output named list of feature vectors.
    syms <- names(scalars$s_mom %||% scalars$s_kal)
    if (length(syms) == 0) {
        return(list())
    }

    features <- list()
    if (!is.null(scalars$s_mom)) {
        features$f_mom <- scalars$s_mom[syms]
    }
    if (!is.null(scalars$s_kal)) {
        features$f_kal <- scalars$s_kal[syms]
    }
    if (!is.null(scalars$s_fac)) {
        features$f_fac <- scalars$s_fac[syms]
    }
    features
}

# ══════════════════════════════════════════════════════════════════════════════
# §11.2 Structural features (factor exposure, volatility rank)
# ══════════════════════════════════════════════════════════════════════════════

#' @export
me_features_structural <- function(risk_artifact, syms) {
    features <- list()
    n <- length(syms)

    # Factor exposure intensity: ||B_i||_2
    B <- risk_artifact$B_t
    if (!is.null(B)) {
        common <- intersect(syms, rownames(B))
        f_fac_exp <- setNames(rep(0, n), syms)
        if (length(common) > 0) {
            norms <- sqrt(rowSums(B[common, , drop = FALSE]^2))
            f_fac_exp[common] <- norms
        }
        # Cross-sectional z-score
        mu <- mean(f_fac_exp, na.rm = TRUE)
        sig <- sd(f_fac_exp, na.rm = TRUE)
        if (is.finite(sig) && sig > 1e-8) f_fac_exp <- (f_fac_exp - mu) / sig
        features$f_factor_exposure <- .tanh_scale(f_fac_exp, 2.0)
    }

    # Volatility rank (cross-sectional percentile)
    vols <- risk_artifact$sigma_t
    if (!is.null(vols)) {
        common <- intersect(syms, names(vols))
        f_vol_rank <- setNames(rep(0.5, n), syms)
        if (length(common) > 1) {
            r <- rank(vols[common]) / (length(common) + 1)
            f_vol_rank[common] <- 2 * (r - 0.5) # center to [-1, 1]
        }
        features$f_vol_rank <- f_vol_rank
    }

    # Idiosyncratic vol fraction: diag(Sigma_eps) / sigma^2
    if (!is.null(risk_artifact$Sigma_eps) && !is.null(vols)) {
        id_var <- diag(risk_artifact$Sigma_eps) * 252
        total_var <- vols^2
        common <- intersect(syms, names(id_var))
        f_idio_frac <- setNames(rep(0, n), syms)
        if (length(common) > 0) {
            ratio <- id_var[common] / pmax(total_var[common], 1e-8)
            ratio[!is.finite(ratio)] <- 0
            f_idio_frac[common] <- 2 * (ratio - 0.5) # re-center
        }
        features$f_idio_frac <- .tanh_scale(f_idio_frac, 1.0)
    }

    features
}

# ══════════════════════════════════════════════════════════════════════════════
# §11.3 Graph-derived features
# ══════════════════════════════════════════════════════════════════════════════

#' @export
me_features_graph <- function(graph_artifact, signal_artifact, syms) {
    features <- list()
    n <- length(syms)

    if (is.null(graph_artifact) || isTRUE(graph_artifact$diag$skipped)) {
        return(features)
    }

    ops <- graph_artifact$operators
    clust <- graph_artifact$clustering
    if (is.null(ops)) {
        return(features)
    }

    A <- ops$A
    L <- ops$L
    graph_syms <- colnames(A)
    common <- intersect(syms, graph_syms)
    if (length(common) < 2) {
        return(features)
    }

    # Build TSMOM signal vector aligned to graph universe
    s_raw <- signal_artifact$tsmom %||% signal_artifact$kalman
    if (is.null(s_raw)) {
        return(features)
    }

    s <- setNames(rep(0, length(graph_syms)), graph_syms)
    sc <- intersect(names(s_raw), graph_syms)
    s[sc] <- s_raw[sc]

    # §9.1 Peer context
    peer <- me_graph_peer(A, s)
    f_peer <- setNames(rep(0, n), syms)
    f_peer[common] <- peer[common]
    features$f_graph_peer <- .tanh_scale(f_peer, 2.0)

    # §9.2 Relative dislocation
    rel <- me_graph_relative(A, s)
    f_rel <- setNames(rep(0, n), syms)
    f_rel[common] <- rel[common]
    features$f_graph_relative <- .tanh_scale(f_rel, 2.0)

    # §9.4 Tension
    ten <- me_graph_tension(L, s)
    f_ten <- setNames(rep(0, n), syms)
    f_ten[common] <- ten[common]
    features$f_graph_tension <- .tanh_scale(f_ten, 2.0)

    # §10.3 Cluster-centered signal
    if (!is.null(clust$labels)) {
        cz <- me_cluster_z(s, clust$labels)
        f_clust_z <- setNames(rep(0, n), syms)
        f_clust_z[common] <- cz[common]
        features$f_cluster_z <- .tanh_scale(f_clust_z, 2.0)
    }

    # Degree centrality as a feature
    deg <- ops$deg
    f_centrality <- setNames(rep(0, n), syms)
    deg_common <- deg[common]
    if (sd(deg_common) > 1e-8) {
        deg_common <- (deg_common - mean(deg_common)) / sd(deg_common)
    }
    f_centrality[common] <- deg_common
    features$f_centrality <- .tanh_scale(f_centrality, 2.0)

    features
}

# ══════════════════════════════════════════════════════════════════════════════
# §11.4 Liquidity features
# ══════════════════════════════════════════════════════════════════════════════

#' @export
me_features_liquidity <- function(adapter, as_of_date, syms) {
    features <- list()
    n <- length(syms)

    sub <- adapter$panel_upto(as_of_date)
    cal <- sort(unique(sub$refdate))
    lkb <- min(63, length(cal))
    if (lkb < 5) {
        return(features)
    }

    dates <- tail(cal, lkb)
    sub <- sub[sub$refdate %in% dates, ]

    # Turnover-based liquidity score
    tv_col <- if ("traded_value" %in% names(sub)) "traded_value" else if ("turnover" %in% names(sub)) "turnover" else NULL

    if (!is.null(tv_col)) {
        med_tv <- tapply(sub[[tv_col]], sub$symbol, median, na.rm = TRUE)
        f_liq <- setNames(rep(0, n), syms)
        common <- intersect(syms, names(med_tv))
        if (length(common) > 1) {
            vals <- log1p(med_tv[common])
            mu <- mean(vals, na.rm = TRUE)
            sig <- sd(vals, na.rm = TRUE)
            if (is.finite(sig) && sig > 1e-8) vals <- (vals - mu) / sig
            f_liq[common] <- vals
        }
        features$f_liquidity <- .tanh_scale(f_liq, 2.0)
    }

    # ILLIQ (Amihud) proxy: mean(|r|/volume)
    close_col <- if ("close" %in% names(sub)) "close" else NULL
    if (!is.null(close_col) && !is.null(tv_col)) {
        sub_sorted <- sub[order(sub$symbol, sub$refdate), ]
        illiq_vals <- tapply(seq_len(nrow(sub_sorted)), sub_sorted$symbol, function(idx) {
            cl <- sub_sorted[[close_col]][idx]
            tv <- sub_sorted[[tv_col]][idx]
            r <- abs(diff(log(cl)))
            vol <- tv[-1]
            vol[vol <= 0 | !is.finite(vol)] <- 1e8
            mean(r / vol, na.rm = TRUE)
        })

        f_illiq <- setNames(rep(0, n), syms)
        common <- intersect(syms, names(illiq_vals))
        if (length(common) > 1) {
            vals <- log1p(illiq_vals[common])
            mu <- mean(vals, na.rm = TRUE)
            sig <- sd(vals, na.rm = TRUE)
            if (is.finite(sig) && sig > 1e-8) vals <- (vals - mu) / sig
            f_illiq[common] <- vals
        }
        features$f_illiq <- .tanh_scale(f_illiq, 2.0)
    }

    features
}

# ══════════════════════════════════════════════════════════════════════════════
# §11.5 Market-state context features (broadcast to all assets)
# ══════════════════════════════════════════════════════════════════════════════

#' @export
me_features_market_state <- function(m_t, syms) {
    n <- length(syms)
    features <- list()
    if (is.null(m_t) || length(m_t) == 0) {
        return(features)
    }

    # Broadcast each state feature to all assets (uniform cross-section)
    for (feat_name in names(m_t)) {
        val <- m_t[feat_name]
        if (is.finite(val)) {
            f <- setNames(rep(val, n), syms)
            features[[paste0("f_state_", feat_name)]] <- f
        }
    }
    features
}

# ══════════════════════════════════════════════════════════════════════════════
# Full feature assembly orchestrator
# ══════════════════════════════════════════════════════════════════════════════

#' @export
me_run_feature_engine <- function(signal_artifact, risk_artifact,
                                  graph_artifact, state_artifact,
                                  adapter, as_of_date, syms) {
    # 1. Scalarize signals
    scalars <- me_scalarize_signals(signal_artifact)

    # 2. Temporal features
    f_temp <- me_features_temporal(scalars, risk_artifact)

    # 3. Structural features
    f_struct <- me_features_structural(risk_artifact, syms)

    # 4. Graph features
    f_graph <- me_features_graph(graph_artifact, signal_artifact, syms)

    # 5. Liquidity features
    f_liq <- me_features_liquidity(adapter, as_of_date, syms)

    # 6. Market-state context
    m_t <- state_artifact$market_state
    f_state <- me_features_market_state(m_t, syms)

    # Combine all feature groups
    all_features <- c(f_temp, f_struct, f_graph, f_liq, f_state)

    # Build feature matrix X (n_assets × n_features)
    if (length(all_features) == 0) {
        return(list(
            X = matrix(0, length(syms), 0),
            feature_names = character(0), diag = list(n_features = 0)
        ))
    }

    X <- do.call(cbind, lapply(all_features, function(f) {
        aligned <- setNames(rep(0, length(syms)), syms)
        idx <- intersect(names(f), syms)
        aligned[idx] <- f[idx]
        aligned
    }))
    colnames(X) <- names(all_features)
    rownames(X) <- syms
    X[!is.finite(X)] <- 0

    list(
        X = X,
        feature_names = names(all_features),
        feature_groups = list(
            temporal   = names(f_temp),
            structural = names(f_struct),
            graph      = names(f_graph),
            liquidity  = names(f_liq),
            state      = names(f_state)
        ),
        diag = list(
            n_features = ncol(X),
            n_assets = nrow(X),
            feature_names = colnames(X),
            coverage = colMeans(X != 0)
        )
    )
}
