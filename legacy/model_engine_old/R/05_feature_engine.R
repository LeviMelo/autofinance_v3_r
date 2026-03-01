#' @title Model Engine — Feature Engine
#' @description Feature assembly: temporal, structural, graph (unsigned/signed), PCA-graph,
#' liquidity (expanded), signal×liquidity interaction, state context.
#' Implements architecture.md §11: complete feature vector construction.

# ══════════════════════════════════════════════════════════════════════════════
# §11.1 Temporal signal features (from signal engine outputs)
# ══════════════════════════════════════════════════════════════════════════════

#' @export
me_features_temporal <- function(signal_artifact) {
    syms <- names(signal_artifact$s_mom)
    n <- length(syms)

    feats <- data.frame(
        f_mom = signal_artifact$s_mom[syms],
        f_kal = signal_artifact$s_kal[syms],
        f_fac = signal_artifact$s_fac[syms],
        row.names = syms,
        stringsAsFactors = FALSE
    )

    # Richer Kalman outputs (§7.2)
    if (!is.null(signal_artifact$kalman_slope)) {
        feats$f_kal_slope <- signal_artifact$kalman_slope[syms]
        feats$f_kal_slope[!is.finite(feats$f_kal_slope)] <- 0
    }
    if (!is.null(signal_artifact$kalman_uncertainty)) {
        feats$f_kal_uncert <- signal_artifact$kalman_uncertainty[syms]
        feats$f_kal_uncert[!is.finite(feats$f_kal_uncert)] <- 0
    }
    if (!is.null(signal_artifact$kalman_innov_ratio)) {
        feats$f_kal_innov_ratio <- signal_artifact$kalman_innov_ratio[syms]
        feats$f_kal_innov_ratio[!is.finite(feats$f_kal_innov_ratio)] <- 0
    }

    # Multiscale TSMOM raw (§7.1)
    if (!is.null(signal_artifact$tsmom_multi) && is.matrix(signal_artifact$tsmom_multi)) {
        tm <- signal_artifact$tsmom_multi
        for (col_nm in colnames(tm)) {
            feats[[paste0("f_", col_nm)]] <- tm[syms, col_nm]
        }
    }

    # Multiscale TSMOM residual (§7.1)
    if (!is.null(signal_artifact$resid_tsmom_multi) && is.matrix(signal_artifact$resid_tsmom_multi)) {
        rtm <- signal_artifact$resid_tsmom_multi
        for (col_nm in colnames(rtm)) {
            feats[[paste0("f_resid_", col_nm)]] <- rtm[syms, col_nm]
        }
    }

    # Scalarized composite (§7.4)
    if (!is.null(signal_artifact$s_scalar)) {
        feats$f_scalar <- signal_artifact$s_scalar[syms]
        feats$f_scalar[!is.finite(feats$f_scalar)] <- 0
    }

    feats[!is.finite(as.matrix(feats))] <- 0
    feats
}

# ══════════════════════════════════════════════════════════════════════════════
# §11.2 Structural features (factor exposure, vol rank, idio)
# ══════════════════════════════════════════════════════════════════════════════

#' @export
me_features_structural <- function(risk_artifact) {
    B <- risk_artifact$B_t
    syms <- rownames(B)
    n <- length(syms)

    # Average absolute factor exposure
    f_factor_exposure <- rowMeans(abs(B), na.rm = TRUE)
    names(f_factor_exposure) <- syms

    # Idiosyncratic fraction: var(e_i) / var(r_i)
    Sigma_eps <- risk_artifact$Sigma_eps
    Sigma_total <- risk_artifact$Sigma_risk_1 %||% risk_artifact$Sigma_total
    idio_var <- diag(Sigma_eps)
    total_var <- diag(Sigma_total)
    f_idio_frac <- ifelse(total_var > 0, idio_var / total_var, 0.5)
    f_idio_frac[!is.finite(f_idio_frac)] <- 0.5
    names(f_idio_frac) <- syms

    # Vol cross-sectional rank
    vols <- risk_artifact$sigma_t[syms]
    vols[!is.finite(vols)] <- median(vols, na.rm = TRUE)
    f_vol_rank <- rank(vols) / (n + 1)
    names(f_vol_rank) <- syms

    data.frame(
        f_factor_exposure = f_factor_exposure[syms],
        f_vol_rank = f_vol_rank[syms],
        f_idio_frac = f_idio_frac[syms],
        row.names = syms,
        stringsAsFactors = FALSE
    )
}

# ══════════════════════════════════════════════════════════════════════════════
# §11.3 Graph features — fixed semantics: unsigned A for peer/relative
# ══════════════════════════════════════════════════════════════════════════════

#' @export
me_features_graph <- function(signal_artifact, graph_artifact) {
    syms <- names(signal_artifact$s_mom)
    n <- length(syms)

    # Default zeros if graph was skipped
    defaults <- data.frame(
        f_graph_peer = rep(0, n),
        f_graph_relative = rep(0, n),
        f_graph_tension = rep(0, n),
        f_cluster_z = rep(0, n),
        f_centrality = rep(0, n),
        row.names = syms,
        stringsAsFactors = FALSE
    )

    if (is.null(graph_artifact$operators) || is.null(graph_artifact$mask)) {
        return(defaults)
    }

    ops <- graph_artifact$operators
    A <- ops$A # §9.1: UNSIGNED row-normalized adjacency
    A_signed <- ops$A_signed # §9.3: SIGNED adjacency
    L <- ops$L
    L_norm <- ops$L_norm
    deg <- ops$deg

    s_mom <- signal_artifact$s_mom
    labels <- graph_artifact$clustering$labels

    feats <- defaults

    # §9.1 Peer context (UNSIGNED A)
    feats$f_graph_peer <- me_graph_peer(A, s_mom[syms])
    # §9.2 Relative dislocation (UNSIGNED A)
    feats$f_graph_relative <- me_graph_relative(A, s_mom[syms])
    # §9.4 Tension
    feats$f_graph_tension <- me_graph_tension(L, s_mom[syms])
    # §10.3 Cluster z
    feats$f_cluster_z <- me_cluster_z(s_mom[syms], labels)
    # Centrality (degree)
    feats$f_centrality <- deg[syms] / max(deg, 1e-8)
    feats$f_centrality[!is.finite(feats$f_centrality)] <- 0

    # §9.3 SIGNED graph transform (own channel)
    if (!is.null(A_signed)) {
        feats$f_graph_signed <- me_graph_signed(A_signed, s_mom[syms])
    }

    # §9.5 Graph shrinkage features
    feats$f_graph_shr_mom <- me_graph_shrinkage(L_norm, s_mom[syms], lambda_g = 0.1)
    if (!is.null(signal_artifact$s_kal)) {
        feats$f_graph_shr_kal <- me_graph_shrinkage(L_norm, signal_artifact$s_kal[syms], lambda_g = 0.1)
    }

    # §9.6 Mixed propagation
    feats$f_graph_mixed_mom <- me_graph_mixed(A, s_mom[syms], nu = 0.5)

    feats[!is.finite(as.matrix(feats))] <- 0
    feats
}

# ══════════════════════════════════════════════════════════════════════════════
# §11.4 PCA-graph interaction features
# ══════════════════════════════════════════════════════════════════════════════

#' @export
me_features_pca_graph <- function(risk_artifact, graph_artifact) {
    B <- risk_artifact$B_t
    syms <- rownames(B)
    n <- length(syms)
    k <- ncol(B)
    labels <- graph_artifact$clustering$labels

    # φ_cf1: factor-graph alignment = b_i' ḡ (average factor loading of neighbors)
    # φ_cf2: cluster-relative factor exposure dislocation
    phi_cf1 <- rep(0, n)
    phi_cf2 <- rep(0, n)
    names(phi_cf1) <- syms
    names(phi_cf2) <- syms

    if (!is.null(graph_artifact$operators)) {
        A <- graph_artifact$operators$A
        for (i in seq_len(n)) {
            s <- syms[i]
            b_i <- B[s, ]
            # Average neighbor factor loading: ḡ_i = A_i × B
            g_i <- as.vector(A[s, , drop = TRUE] %*% B[syms, ])
            phi_cf1[s] <- sum(b_i * g_i)

            # Cluster-relative: b_i - b̄_{c(i)}
            ci <- labels[s]
            cluster_mates <- names(labels)[labels == ci]
            cluster_mates <- intersect(cluster_mates, syms)
            if (length(cluster_mates) > 1) {
                b_bar <- colMeans(B[cluster_mates, , drop = FALSE])
                dislo <- b_i - b_bar
                phi_cf2[s] <- sum(dislo * g_i) # dislocation × neighbor context
            }
        }
    }

    data.frame(
        f_pca_graph_align = phi_cf1,
        f_pca_graph_dislo = phi_cf2,
        row.names = syms,
        stringsAsFactors = FALSE
    )
}

# ══════════════════════════════════════════════════════════════════════════════
# §11.5 Liquidity features (expanded)
# ══════════════════════════════════════════════════════════════════════════════

#' @export
me_features_liquidity <- function(prices_window, volume_window, risk_artifact,
                                  extra_volume = NULL) {
    syms <- colnames(prices_window)
    n <- length(syms)
    Tn <- nrow(prices_window)
    vols <- risk_artifact$sigma_t

    feats <- data.frame(row.names = syms, stringsAsFactors = FALSE)

    # Basic liquidity: log(avg turnover * price)
    if (!is.null(volume_window) && ncol(volume_window) > 0) {
        avg_vol <- colMeans(volume_window, na.rm = TRUE)
        avg_vol[!is.finite(avg_vol) | avg_vol <= 0] <- 1
        last_price <- prices_window[Tn, ]
        last_price[!is.finite(last_price) | last_price <= 0] <- 1
        traded_value <- avg_vol * last_price
        feats$f_liquidity <- log1p(traded_value)
        feats$f_liquidity[!is.finite(feats$f_liquidity)] <- 0

        # Traded units / n_trades (if available)
        feats$f_traded_value <- .tanh_scale(log1p(traded_value), 15)
        feats$f_traded_units <- .tanh_scale(log1p(avg_vol), 15)

        # Amihud-like illiquidity: mean(|r| / volume)
        R_abs <- abs(diff(log(pmax(prices_window, 1e-8))))
        vol_tail <- volume_window[-1, , drop = FALSE]
        vol_tail[vol_tail <= 0 | !is.finite(vol_tail)] <- 1
        amihud_daily <- R_abs / vol_tail
        amihud_daily[!is.finite(amihud_daily)] <- 0
        f_illiq <- colMeans(amihud_daily, na.rm = TRUE)
        f_illiq[!is.finite(f_illiq)] <- 0
        feats$f_illiq <- .tanh_scale(f_illiq * 1e6, 2.0)

        # Rolling standardized liquidity (7-day window)
        if (Tn >= 7) {
            recent_vol <- colMeans(volume_window[max(1, Tn - 6):Tn, , drop = FALSE], na.rm = TRUE)
            feats$f_liq_zscore <- (recent_vol - avg_vol) / pmax(apply(volume_window, 2, sd, na.rm = TRUE), 1)
            feats$f_liq_zscore[!is.finite(feats$f_liq_zscore)] <- 0
        } else {
            feats$f_liq_zscore <- 0
        }

        # Average trade size proxy (if n_trades available)
        if (!is.null(extra_volume) && !is.null(extra_volume$n_trades)) {
            n_trades <- extra_volume$n_trades[syms]
            n_trades[!is.finite(n_trades) | n_trades <= 0] <- 1
            feats$f_avg_trade_size <- .tanh_scale(log1p(avg_vol / n_trades), 10)
            feats$f_n_trades <- .tanh_scale(log1p(n_trades), 10)
        }
    } else {
        feats$f_liquidity <- rep(0, n)
        feats$f_traded_value <- rep(0, n)
        feats$f_traded_units <- rep(0, n)
        feats$f_illiq <- rep(0, n)
        feats$f_liq_zscore <- rep(0, n)
    }

    feats[!is.finite(as.matrix(feats))] <- 0
    feats
}

# ══════════════════════════════════════════════════════════════════════════════
# §11.6 Signal × Liquidity interaction features
# ══════════════════════════════════════════════════════════════════════════════

#' @export
me_features_signal_liquidity <- function(signal_artifact, liq_feats) {
    syms <- names(signal_artifact$s_mom)
    n <- length(syms)

    feats <- data.frame(row.names = syms, stringsAsFactors = FALSE)

    liq_score <- if (!is.null(liq_feats$f_liq_zscore)) {
        liq_feats$f_liq_zscore
    } else if (!is.null(liq_feats$f_liquidity)) {
        liq_feats$f_liquidity
    } else {
        setNames(rep(0, n), syms)
    }

    feats$f_mom_x_liq <- abs(signal_artifact$s_mom[syms]) * liq_score
    feats$f_kal_x_liq <- abs(signal_artifact$s_kal[syms]) * liq_score

    feats[!is.finite(as.matrix(feats))] <- 0
    feats
}

# ══════════════════════════════════════════════════════════════════════════════
# §11.7 State context features (global + graph diagnostics)
# ══════════════════════════════════════════════════════════════════════════════

#' @export
me_features_state <- function(market_state, graph_artifact = NULL) {
    # market_state can be a named vector or a list
    if (is.numeric(market_state)) {
        disp <- market_state["disp"] %||% 0
        eta <- market_state["eta"] %||% 0
        vov <- market_state["VoV"] %||% 0
    } else {
        disp <- market_state$dispersion %||% market_state$disp %||% 0
        eta <- market_state$eta %||% 0
        vov <- market_state$VoV %||% market_state$vov %||% 0
    }

    feats <- data.frame(
        f_state_disp = as.numeric(disp),
        f_state_eta = as.numeric(eta),
        f_state_VoV = as.numeric(vov),
        stringsAsFactors = FALSE
    )

    # §11.7: Graph diagnostics in state context
    if (!is.null(graph_artifact$diag)) {
        gd <- graph_artifact$diag
        feats$f_state_density <- gd$density %||% 0
        feats$f_state_eto <- gd$edge_turnover %||% 0
        feats$f_state_chi <- gd$chi_t %||% 0
    }

    feats
}

# ══════════════════════════════════════════════════════════════════════════════
# Full feature assembly (architecture §11)
# ══════════════════════════════════════════════════════════════════════════════

#' @export
me_assemble_features <- function(signal_artifact, risk_artifact, graph_artifact,
                                 market_state, prices_window, volume_window = NULL,
                                 extra_volume = NULL) {
    syms <- names(signal_artifact$s_mom)
    n <- length(syms)

    # §11.1 Temporal
    f_temporal <- me_features_temporal(signal_artifact)
    # §11.2 Structural
    f_struct <- me_features_structural(risk_artifact)
    # §11.3 Graph (unsigned peer/relative, signed own channel)
    f_graph <- me_features_graph(signal_artifact, graph_artifact)
    # §11.4 PCA-graph interaction
    f_pca_graph <- me_features_pca_graph(risk_artifact, graph_artifact)
    # §11.5 Liquidity (expanded)
    f_liq <- me_features_liquidity(prices_window, volume_window, risk_artifact,
        extra_volume = extra_volume
    )
    # §11.6 Signal × Liquidity interactions
    f_sig_liq <- me_features_signal_liquidity(signal_artifact, f_liq)
    # §11.7 State context (with graph diag)
    f_state_raw <- me_features_state(market_state, graph_artifact)

    # Align structural/graph/pca_graph to syms
    f_struct <- f_struct[syms, , drop = FALSE]
    f_graph <- f_graph[syms, , drop = FALSE]
    f_pca_graph <- f_pca_graph[syms, , drop = FALSE]

    # State features: broadcast scalar to all assets
    n_state_cols <- ncol(f_state_raw)
    f_state <- as.data.frame(
        matrix(rep(as.numeric(f_state_raw[1, ]), each = n),
            nrow = n, ncol = n_state_cols,
            dimnames = list(syms, names(f_state_raw))
        )
    )

    # Combine all
    X <- cbind(f_temporal, f_struct, f_graph, f_pca_graph, f_liq, f_sig_liq, f_state)
    X[!is.finite(as.matrix(X))] <- 0

    # Feature group metadata
    groups <- list(
        temporal = names(f_temporal),
        structural = names(f_struct),
        graph = names(f_graph),
        pca_graph = names(f_pca_graph),
        liquidity = names(f_liq),
        signal_liquidity = names(f_sig_liq),
        state = names(f_state)
    )

    list(X = X, groups = groups, n_features = ncol(X))
}
