#' @title Model Engine — Portfolio Engine (QP Optimizer)
#' @description Quadratic optimization, post-shaping, repair, and constraint enforcement.
#' Implements architecture.md §13-14: continuous optimizer + post-shaping.

# ══════════════════════════════════════════════════════════════════════════════
# §13 Pre-shaping: forecast → alpha vector
# ══════════════════════════════════════════════════════════════════════════════

#' @export
me_forecast_to_alpha <- function(combined_forecast, confidence, spec_portfolio) {
    # α_i = f_hat_i × κ_agreement_i × scale
    alpha_scale <- spec_portfolio$alpha_scale %||% 1.0
    syms <- names(combined_forecast)

    agree <- confidence$agreement
    agree_aligned <- setNames(rep(1, length(syms)), syms)
    common <- intersect(syms, names(agree))
    agree_aligned[common] <- agree[common]

    alpha <- combined_forecast * agree_aligned * alpha_scale
    alpha[!is.finite(alpha)] <- 0
    alpha
}

# ══════════════════════════════════════════════════════════════════════════════
# §14 QP Optimizer: min w^T Σ w - γ α^T w  s.t. constraints
# ══════════════════════════════════════════════════════════════════════════════

#' @keywords internal
.me_project_capped_simplex <- function(w, target_sum, cap, max_iter = 100L, tol = 1e-10) {
    nm <- names(w) # preserve names BEFORE numeric coercion
    w <- unname(as.numeric(w)) # strip names intentionally, restore later

    if (length(w) == 0) {
        out <- numeric(0)
        names(out) <- nm
        return(out)
    }

    w[!is.finite(w)] <- 0
    w <- pmax(w, 0)

    target_sum <- max(0, target_sum)
    target_sum <- min(target_sum, length(w) * cap)

    if (target_sum == 0) {
        out <- rep(0, length(w))
        names(out) <- nm
        return(out)
    }

    if (sum(w) <= 0) {
        out <- rep(0, length(w))
        remaining <- target_sum
        for (i in seq_along(out)) {
            add <- min(cap, remaining)
            out[i] <- add
            remaining <- remaining - add
            if (remaining <= tol) break
        }
        names(out) <- nm
        return(out)
    }

    # Start by scaling to target sum
    w <- w * (target_sum / sum(w))

    for (iter in seq_len(max_iter)) {
        over <- which(w > cap + tol)
        if (length(over) == 0) break

        excess <- sum(w[over] - cap)
        w[over] <- cap

        under <- which(w < cap - tol)
        if (length(under) == 0 || excess <= tol) break

        base <- w[under]
        base[!is.finite(base)] <- 0

        if (sum(base) > tol) {
            w[under] <- w[under] + excess * (base / sum(base))
        } else {
            headroom <- cap - w[under]
            if (sum(headroom) <= tol) break
            w[under] <- w[under] + excess * (headroom / sum(headroom))
        }

        w <- pmax(w, 0)
    }

    # Final correction
    s <- sum(w)
    if (s > 0 && abs(s - target_sum) > 1e-8) {
        headroom <- pmax(cap - w, 0)
        if (s < target_sum && sum(headroom) > tol) {
            add <- (target_sum - s) * headroom / sum(headroom)
            w <- w + add
        } else if (s > target_sum) {
            reducible <- pmax(w, 0)
            if (sum(reducible) > tol) {
                sub <- (s - target_sum) * reducible / sum(reducible)
                w <- pmax(w - sub, 0)
            }
        }
    }

    names(w) <- nm
    w
}

#' @export
me_qp_optimize <- function(Sigma, alpha, w_baseline, gross_exposure,
                           spec_portfolio, prev_target = NULL) {
    n <- length(alpha)
    syms <- names(alpha)

    gamma <- spec_portfolio$gamma %||% 1.0
    max_weight <- spec_portfolio$caps$max_weight %||% 0.15
    turnover_penalty <- spec_portfolio$turnover_penalty %||% 0.0

    if (n == 0) {
        return(list(w = setNames(numeric(0), character(0)), method = "empty", converged = TRUE))
    }

    # Feasibility: sum(w)=gross_exposure with w_i <= max_weight
    gross_req <- gross_exposure
    gross_cap_max <- n * max_weight
    gross_use <- min(gross_req, gross_cap_max)
    if (!is.finite(gross_use) || gross_use < 0) gross_use <- 0

    has_qp <- requireNamespace("quadprog", quietly = TRUE)

    if (has_qp && n >= 2) {
        Dmat <- Sigma
        Dmat <- (Dmat + t(Dmat)) / 2

        dvec <- gamma * alpha

        # Quadratic turnover proxy around previous target:
        # +(lambda/2)||w - w_prev||^2 => D += lambda I ; d += lambda w_prev
        if (turnover_penalty > 0 && !is.null(prev_target)) {
            if (is.data.frame(prev_target)) {
                w_prev <- setNames(prev_target$weight_target, prev_target$symbol)
            } else {
                w_prev <- prev_target
            }
            w_prev_aligned <- setNames(rep(0, n), syms)
            common_prev <- intersect(syms, names(w_prev))
            w_prev_aligned[common_prev] <- w_prev[common_prev]

            diag(Dmat) <- diag(Dmat) + turnover_penalty
            dvec <- dvec + turnover_penalty * w_prev_aligned
        } else if (turnover_penalty > 0) {
            # Mild ridge only if no prev target is available
            diag(Dmat) <- diag(Dmat) + turnover_penalty
        }

        # Ensure positive definite enough for solve.QP
        eigvals <- tryCatch(eigen(Dmat, symmetric = TRUE, only.values = TRUE)$values,
            error = function(e) NULL
        )
        if (!is.null(eigvals)) {
            min_eig <- min(eigvals, na.rm = TRUE)
            if (is.finite(min_eig) && min_eig < 1e-8) {
                diag(Dmat) <- diag(Dmat) + abs(min_eig) + 1e-6
            }
        } else {
            diag(Dmat) <- diag(Dmat) + 1e-6
        }

        # solve.QP uses Amat^T w >= bvec
        # Equality: sum(w) = gross_use  (meq = 1)
        Amat <- cbind(
            rep(1, n), # equality
            diag(n), # w_i >= 0
            -diag(n) # w_i <= max_weight
        )
        bvec <- c(
            gross_use,
            rep(0, n),
            rep(-max_weight, n)
        )

        sol <- tryCatch(
            quadprog::solve.QP(Dmat, dvec, Amat, bvec, meq = 1),
            error = function(e) NULL
        )

        if (!is.null(sol)) {
            w_opt <- sol$solution
            w_opt[!is.finite(w_opt)] <- 0
            w_opt[w_opt < 0] <- 0
            names(w_opt) <- syms

            # Final robust capped projection (see helper below)
            w_opt <- .me_project_capped_simplex(w_opt, target_sum = gross_use, cap = max_weight)

            return(list(
                w = w_opt,
                method = "qp",
                obj_value = sol$value,
                converged = TRUE,
                gross_requested = gross_req,
                gross_used = gross_use
            ))
        }
    }

    # Fallback
    w_opt <- .tilt_allocation(
        alpha, w_baseline, gross_use, max_weight, spec_portfolio
    )

    list(
        w = w_opt, method = "tilt_fallback", obj_value = NA, converged = FALSE,
        gross_requested = gross_req, gross_used = gross_use
    )
}

#' Tilt-based fallback (score-to-tilt on baseline)
.tilt_allocation <- function(alpha, w_baseline, gross_exposure, max_weight,
                             spec_portfolio) {
    syms <- names(alpha)
    max_tilt <- spec_portfolio$tilt$max_tilt %||% 2.0

    # Cross-sectional z-score of alpha
    a <- alpha[is.finite(alpha)]
    if (length(a) < 2) {
        w <- w_baseline * (gross_exposure / max(sum(w_baseline), 1e-12))
        return(w)
    }
    mu <- mean(a)
    sigma <- max(sd(a), 1e-8)
    z <- (alpha - mu) / sigma
    z[!is.finite(z)] <- 0

    tilt <- exp(log(max_tilt) * z)
    tilt <- pmin(pmax(tilt, 1 / max_tilt), max_tilt)

    w <- w_baseline * tilt

    w[w < 0] <- 0
    w <- .me_project_capped_simplex(w, target_sum = gross_exposure, cap = max_weight)
    names(w) <- syms
    w
}

# ══════════════════════════════════════════════════════════════════════════════
# §14.2 Post-shaping repair
# ══════════════════════════════════════════════════════════════════════════════

#' @export
me_post_shape <- function(w, tradable_symbols, max_weight = 0.15,
                          min_weight = 1e-6, gross_exposure = 1.0) {
    if (length(w) == 0) {
        return(w)
    }

    # 1. Zero out non-tradable
    non_tradable <- setdiff(names(w), tradable_symbols)
    if (length(non_tradable) > 0) w[non_tradable] <- 0

    # 2. Zero out dust
    w[!is.finite(w)] <- 0
    w[w < min_weight] <- 0
    w[w < 0] <- 0

    # 3. Project to capped simplex exactly
    w <- .me_project_capped_simplex(w, target_sum = gross_exposure, cap = max_weight)

    w
}

# ══════════════════════════════════════════════════════════════════════════════
# §14.3 Turnover ceiling
# ══════════════════════════════════════════════════════════════════════════════

#' @export
me_apply_turnover_ceiling <- function(w_new, w_prev, max_turnover = 0.5) {
    if (is.null(w_prev) || length(w_prev) == 0) {
        return(w_new)
    }

    syms <- union(names(w_new), names(w_prev))
    wn <- setNames(rep(0, length(syms)), syms)
    wp <- setNames(rep(0, length(syms)), syms)
    wn[names(w_new)] <- w_new
    wp[names(w_prev)] <- w_prev

    turnover <- sum(abs(wn - wp)) / 2
    if (turnover <= max_turnover) {
        return(w_new)
    }

    # Shrink toward previous
    lambda <- max_turnover / turnover
    w_blend <- (1 - lambda) * wp + lambda * wn
    w_blend[w_blend < 0] <- 0
    s <- sum(w_blend)
    if (s > 0) w_blend <- w_blend * (sum(w_new) / s)
    w_blend[names(w_new)]
}

# ══════════════════════════════════════════════════════════════════════════════
# Full portfolio engine orchestrator
# ══════════════════════════════════════════════════════════════════════════════

#' @export
me_build_portfolio_target <- function(risk_artifact, signal_artifact,
                                      state_gating_artifact, spec_portfolio,
                                      forecast_artifact = NULL,
                                      graph_artifact = NULL,
                                      prev_target = NULL,
                                      optimizer_controls = NULL,
                                      liquidity_features = NULL) {
    gating <- state_gating_artifact$gating
    gross_exposure <- gating$gross_exposure
    w_cash <- gating$w_cash

    # Optimizer controls override spec defaults if provided (§12.8)
    if (!is.null(optimizer_controls)) {
        gamma_t <- optimizer_controls$gamma_t %||% (spec_portfolio$gamma %||% 1.0)
        tau_t <- optimizer_controls$tau_t %||% (spec_portfolio$turnover_penalty %||% 0.0)
        rho_gross <- optimizer_controls$rho_gross
        if (!is.null(rho_gross) && is.finite(rho_gross)) {
            gross_exposure <- rho_gross
            w_cash <- 1 - rho_gross
        }
    } else {
        gamma_t <- spec_portfolio$gamma %||% 1.0
        tau_t <- spec_portfolio$turnover_penalty %||% 0.0
    }

    max_weight <- spec_portfolio$caps$max_weight %||% 0.15

    Sigma <- risk_artifact$Sigma_risk_H %||% risk_artifact$Sigma_total %||% risk_artifact$Sigma_risk_1
    if (is.null(Sigma)) {
        stop("me_build_portfolio_target: risk artifact missing covariance matrix (Sigma_risk_H / Sigma_total / Sigma_risk_1).")
    }
    if (!is.matrix(Sigma) || nrow(Sigma) != ncol(Sigma)) {
        stop("me_build_portfolio_target: risk covariance must be square matrix.")
    }
    if (is.null(rownames(Sigma)) || is.null(colnames(Sigma)) || !identical(rownames(Sigma), colnames(Sigma))) {
        stop("me_build_portfolio_target: risk covariance must have matching row/col names in same order.")
    }

    risk_univ <- colnames(Sigma)
    n_risk <- length(risk_univ)

    if (n_risk == 0) {
        tgt_df <- data.frame(
            symbol = character(0), weight_target = numeric(0),
            stringsAsFactors = FALSE
        )
        return(list(
            target_weights = tgt_df, cash_weight = 1.0,
            diag = list(n_risk = 0, method = "empty")
        ))
    }

    # Transitional baseline
    w_baseline <- risk_artifact$w_baseline %||% risk_artifact$w_hrp
    if (is.null(w_baseline) || length(w_baseline) == 0) {
        w_baseline <- setNames(rep(1 / n_risk, n_risk), risk_univ)
    } else {
        wb <- setNames(rep(0, n_risk), risk_univ)
        common_wb <- intersect(risk_univ, names(w_baseline))
        wb[common_wb] <- w_baseline[common_wb]
        if (sum(wb) <= 0) {
            wb[] <- 1 / n_risk
        } else {
            wb <- wb / sum(wb)
        }
        w_baseline <- wb
    }

    # §11.9 Liquidity-aware caps (if liquidity features available)
    cap_vec <- setNames(rep(max_weight, n_risk), risk_univ)
    if (!is.null(liquidity_features) && !is.null(liquidity_features$f_liquidity)) {
        liq <- liquidity_features$f_liquidity[risk_univ]
        liq[!is.finite(liq)] <- 0
        # h_w(ℓ) = max_weight × (0.5 + 0.5 × σ(ℓ - median))
        liq_med <- median(liq, na.rm = TRUE)
        liq_scale <- 1 / (1 + exp(-(liq - liq_med)))
        cap_vec <- max_weight * (0.5 + 0.5 * liq_scale)
        cap_vec <- pmin(pmax(cap_vec, 0.02), max_weight)
        names(cap_vec) <- risk_univ
    }

    # ── Choose optimization path ──
    method <- "tilt"

    # ARCHITECTURE PATH: use mu_eff directly as alpha
    use_arch_alpha <- !is.null(forecast_artifact) &&
        !is.null(forecast_artifact$mu_eff) &&
        length(forecast_artifact$mu_eff) > 0

    if (use_arch_alpha) {
        # §13: Portfolio consumes μ_eff directly (no me_forecast_to_alpha re-scaling)
        alpha <- forecast_artifact$mu_eff
        alpha_aligned <- setNames(rep(0, n_risk), risk_univ)
        common_alpha <- intersect(names(alpha), risk_univ)
        alpha_aligned[common_alpha] <- alpha[common_alpha]

        # Use dynamic optimizer controls
        spec_qp <- spec_portfolio
        spec_qp$gamma <- gamma_t
        spec_qp$turnover_penalty <- tau_t
        spec_qp$caps$max_weight <- max_weight # Use base max; per-asset caps in post-shaping

        opt <- me_qp_optimize(
            Sigma, alpha_aligned, w_baseline, gross_exposure,
            spec_qp, prev_target
        )
        w_target <- opt$w
        method <- paste0("arch_", opt$method)
    } else if (!is.null(forecast_artifact) && length(forecast_artifact$combined_forecast %||% NULL) > 0) {
        # LEGACY PATH: use forecast_to_alpha
        # FALLBACK:portfolio_legacy_alpha_path
        alpha <- me_forecast_to_alpha(
            forecast_artifact$combined_forecast,
            forecast_artifact$confidence,
            spec_portfolio
        )
        alpha_aligned <- setNames(rep(0, n_risk), risk_univ)
        alpha_aligned[intersect(names(alpha), risk_univ)] <-
            alpha[intersect(names(alpha), risk_univ)]

        opt <- me_qp_optimize(
            Sigma, alpha_aligned, w_baseline, gross_exposure,
            spec_portfolio, prev_target
        )
        w_target <- opt$w
        method <- opt$method
    } else {
        # Signal-based tilt path (simplest fallback)
        # FALLBACK:portfolio_tilt_only
        combined <- .combine_expert_scores(signal_artifact, gating, spec_portfolio)
        alpha_aligned <- setNames(rep(0, n_risk), risk_univ)
        alpha_aligned[intersect(names(combined), risk_univ)] <-
            combined[intersect(names(combined), risk_univ)]

        w_target <- .tilt_allocation(
            alpha_aligned, w_baseline, gross_exposure,
            max_weight, spec_portfolio
        )
        method <- "FALLBACK:tilt_signal"
    }

    # Post-shaping (with per-asset caps if available)
    effective_cap <- max(cap_vec)
    w_target <- me_post_shape(w_target, risk_univ, effective_cap,
        min_weight = 1e-6, gross_exposure
    )

    # Turnover ceiling
    if (!is.null(prev_target) && is.data.frame(prev_target)) {
        w_prev <- setNames(prev_target$weight_target, prev_target$symbol)
        max_to <- spec_portfolio$max_turnover %||% 0.5
        w_target <- me_apply_turnover_ceiling(w_target, w_prev, max_to)
    }

    # Contract: portfolio weights must be a named vector
    if (is.null(names(w_target))) {
        stop("me_build_portfolio_target: w_target lost names (NULL).")
    }
    if (length(names(w_target)) != length(w_target)) {
        stop(sprintf(
            "me_build_portfolio_target: names(w_target) length (%d) != length(w_target) (%d).",
            length(names(w_target)), length(w_target)
        ))
    }
    if (any(!nzchar(names(w_target)))) {
        stop("me_build_portfolio_target: w_target contains empty symbol names.")
    }

    tgt_df <- data.frame(
        symbol = names(w_target),
        weight_target = unname(w_target),
        stringsAsFactors = FALSE
    )
    tgt_df <- tgt_df[tgt_df$weight_target > 1e-8, , drop = FALSE]
    rownames(tgt_df) <- NULL

    actual_risky <- sum(tgt_df$weight_target)
    actual_cash <- 1 - actual_risky

    list(
        target_weights = tgt_df,
        cash_weight = actual_cash,
        diag = list(
            n_risk = n_risk, n_active = nrow(tgt_df),
            gross_exposure = actual_risky,
            method = method,
            max_weight_used = max_weight,
            gamma_used = gamma_t,
            tau_used = tau_t,
            liquidity_caps_used = !is.null(liquidity_features),
            baseline_method = risk_artifact$baseline_method %||% "unknown"
        )
    )
}

# ── Helper: combine expert scores for tilt path ──

.combine_expert_scores <- function(signal_artifact, gating, spec_portfolio) {
    w_kal <- gating$w_kalman
    w_tsm <- gating$w_tsmom
    kalman <- signal_artifact$kalman %||% signal_artifact$s_kal
    tsmom <- signal_artifact$tsmom %||% signal_artifact$s_mom
    syms <- unique(c(names(kalman), names(tsmom)))
    if (length(syms) == 0) {
        return(setNames(numeric(0), character(0)))
    }
    k <- setNames(rep(0, length(syms)), syms)
    t <- setNames(rep(0, length(syms)), syms)
    k[names(kalman)] <- kalman
    t[names(tsmom)] <- tsmom
    active <- w_kal + w_tsm
    if (active <= 0) {
        return(setNames(rep(0, length(syms)), syms))
    }
    combined <- (w_kal * k + w_tsm * t) / active
    combined[!is.finite(combined)] <- 0
    combined
}
