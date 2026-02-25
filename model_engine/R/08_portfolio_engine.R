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

#' @export
me_qp_optimize <- function(Sigma, alpha, w_baseline, gross_exposure,
                           spec_portfolio, prev_target = NULL) {
    n <- length(alpha)
    syms <- names(alpha)
    gamma <- spec_portfolio$gamma %||% 1.0
    max_weight <- spec_portfolio$caps$max_weight %||% 0.15
    turnover_penalty <- spec_portfolio$turnover_penalty %||% 0.0

    # Check for quadprog
    has_qp <- requireNamespace("quadprog", quietly = TRUE)

    if (has_qp && n >= 2) {
        # ── QP formulation ──
        # min 0.5 * w^T (Sigma + turnover_pen * I) w - (gamma * alpha)^T w
        # s.t.: sum(w) = gross_exposure, 0 <= w_i <= max_weight

        # Turnover regularization
        Dmat <- Sigma
        if (turnover_penalty > 0) {
            diag(Dmat) <- diag(Dmat) + turnover_penalty
        }

        # Ensure PD (add small ridge)
        min_eig <- min(eigen(Dmat, symmetric = TRUE, only.values = TRUE)$values)
        if (min_eig < 1e-8) {
            diag(Dmat) <- diag(Dmat) + abs(min_eig) + 1e-6
        }

        dvec <- gamma * alpha

        # Constraints: Amat^T w >= bvec
        # 1. sum(w) = gross_exposure    → +sum and -sum
        # 2. w_i >= 0                    → identity rows
        # 3. w_i <= max_weight           → -identity rows
        Amat <- cbind(
            rep(1, n), # sum = gross_exposure (eq via two ineq)
            rep(-1, n), # -sum >= -gross_exposure
            diag(n), # w_i >= 0
            -diag(n) # -w_i >= -max_weight
        )
        bvec <- c(
            gross_exposure - 1e-6, # sum >= gross - eps
            -(gross_exposure + 1e-6), # -sum >= -(gross + eps)
            rep(0, n), # w_i >= 0
            rep(-max_weight, n) # -w_i >= -max_weight
        )

        sol <- tryCatch(
            quadprog::solve.QP(Dmat, dvec, Amat, bvec, meq = 0),
            error = function(e) NULL
        )

        if (!is.null(sol)) {
            w_opt <- sol$solution
            w_opt[w_opt < 0] <- 0
            # Rescale to exact gross exposure
            s <- sum(w_opt)
            if (s > 0) w_opt <- w_opt * (gross_exposure / s)
            names(w_opt) <- syms
            return(list(
                w = w_opt, method = "qp",
                obj_value = sol$value,
                converged = TRUE
            ))
        }
    }

    # ── Fallback: tilt-based allocation ──
    w_opt <- .tilt_allocation(
        alpha, w_baseline, gross_exposure, max_weight,
        spec_portfolio
    )
    list(w = w_opt, method = "tilt_fallback", obj_value = NA, converged = FALSE)
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
    s <- sum(w)
    if (s > 0) w <- w * (gross_exposure / s)

    # Apply caps
    for (iter in 1:20) {
        over <- w > max_weight
        if (!any(over)) break
        overflow <- sum(w[over] - max_weight)
        w[over] <- max_weight
        under <- !over & w > 0
        if (!any(under)) break
        w[under] <- w[under] + overflow * (w[under] / sum(w[under]))
    }
    w <- w * (gross_exposure / max(sum(w), 1e-12))
    names(w) <- syms
    w
}

# ══════════════════════════════════════════════════════════════════════════════
# §14.2 Post-shaping repair
# ══════════════════════════════════════════════════════════════════════════════

#' @export
me_post_shape <- function(w, tradable_mask, max_weight = 0.15,
                          min_weight = 1e-6, gross_exposure = 1.0) {
    # 1. Zero out non-tradable
    non_tradable <- setdiff(names(w), tradable_mask)
    w[non_tradable] <- 0

    # 2. Zero out dust positions
    w[w < min_weight] <- 0

    # 3. Re-enforce caps
    w <- pmin(w, max_weight)

    # 4. Renormalize to gross exposure
    s <- sum(w)
    if (s > 0) {
        w <- w * (gross_exposure / s)
    }

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
                                      prev_target = NULL) {
    w_hrp <- risk_artifact$w_hrp
    gating <- state_gating_artifact$gating
    gross_exposure <- gating$gross_exposure
    w_cash <- gating$w_cash
    max_weight <- spec_portfolio$caps$max_weight %||% 0.15

    risk_univ <- names(w_hrp)
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

    # ── Choose optimization path ──
    method <- "tilt"

    if (!is.null(forecast_artifact) && length(forecast_artifact$combined_forecast) > 0) {
        # Full QP path: use forecast as alpha
        alpha <- me_forecast_to_alpha(
            forecast_artifact$combined_forecast,
            forecast_artifact$confidence,
            spec_portfolio
        )
        # Align to risk universe
        alpha_aligned <- setNames(rep(0, n_risk), risk_univ)
        alpha_aligned[intersect(names(alpha), risk_univ)] <-
            alpha[intersect(names(alpha), risk_univ)]

        Sigma <- risk_artifact$Sigma_total
        opt <- me_qp_optimize(
            Sigma, alpha_aligned, w_hrp, gross_exposure,
            spec_portfolio, prev_target
        )
        w_target <- opt$w
        method <- opt$method
    } else {
        # Signal-based tilt path (fallback to simple)
        combined <- .combine_expert_scores(signal_artifact, gating, spec_portfolio)
        alpha_aligned <- setNames(rep(0, n_risk), risk_univ)
        alpha_aligned[intersect(names(combined), risk_univ)] <-
            combined[intersect(names(combined), risk_univ)]

        w_target <- .tilt_allocation(
            alpha_aligned, w_hrp, gross_exposure,
            max_weight, spec_portfolio
        )
        method <- "tilt_signal"
    }

    # Post-shaping
    w_target <- me_post_shape(w_target, risk_univ, max_weight,
        min_weight = 1e-6, gross_exposure
    )

    # Turnover ceiling
    if (!is.null(prev_target) && is.data.frame(prev_target)) {
        w_prev <- setNames(prev_target$weight_target, prev_target$symbol)
        max_to <- spec_portfolio$max_turnover %||% 0.5
        w_target <- me_apply_turnover_ceiling(w_target, w_prev, max_to)
    }

    # Build target dataframe
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
            max_weight_used = max_weight
        )
    )
}

# ── Helper: combine expert scores for tilt path ──

.combine_expert_scores <- function(signal_artifact, gating, spec_portfolio) {
    w_kal <- gating$w_kalman
    w_tsm <- gating$w_tsmom
    kalman <- signal_artifact$kalman
    tsmom <- signal_artifact$tsmom
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
