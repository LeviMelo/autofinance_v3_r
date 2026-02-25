#' @title Model Engine — Portfolio Constructor
#' @description Convert risk baseline + signals + gating into target weights.

# ── Combine expert scores using gating weights ────────────────────────────────

#' @export
me_combine_expert_scores <- function(signal_artifact, gating_artifact, spec_portfolio) {
    w_kal <- gating_artifact$w_kalman
    w_tsm <- gating_artifact$w_tsmom

    kalman_scores <- signal_artifact$kalman
    tsmom_scores <- signal_artifact$tsmom

    # Union universe
    syms <- unique(c(names(kalman_scores), names(tsmom_scores)))
    if (length(syms) == 0) {
        return(setNames(numeric(0), character(0)))
    }

    # Align
    k <- rep(0, length(syms))
    names(k) <- syms
    t <- rep(0, length(syms))
    names(t) <- syms
    k[names(kalman_scores)] <- kalman_scores
    t[names(tsmom_scores)] <- tsmom_scores

    # Weighted combination (excluding cash from active weight)
    active_sum <- w_kal + w_tsm
    if (active_sum <= 0) {
        return(setNames(rep(0, length(syms)), syms))
    }

    combined <- (w_kal * k + w_tsm * t) / active_sum
    combined[!is.finite(combined)] <- 0
    combined
}

# ── Score to tilt ─────────────────────────────────────────────────────────────

#' @export
me_score_to_tilt <- function(scores, max_tilt = 2.0) {
    if (length(scores) == 0) {
        return(numeric(0))
    }

    # Cross-sectional z-score
    s <- scores[is.finite(scores)]
    if (length(s) < 2) {
        tilt <- rep(1, length(scores))
        names(tilt) <- names(scores)
        return(tilt)
    }

    mu <- mean(s)
    sigma <- sd(s)
    if (!is.finite(sigma) || sigma < 1e-8) sigma <- 1e-8

    z <- (scores - mu) / sigma
    z[!is.finite(z)] <- 0

    # Bounded exponential tilt: exp(lambda * z) clamped to [1/max_tilt, max_tilt]
    lambda <- log(max_tilt)
    tilt <- exp(lambda * z)
    tilt <- pmin(pmax(tilt, 1 / max_tilt), max_tilt)

    names(tilt) <- names(scores)
    tilt
}

# ── Apply tilts to HRP baseline ───────────────────────────────────────────────

#' @export
me_apply_tilts <- function(w_hrp, tilts, gross_exposure) {
    syms <- names(w_hrp)
    if (length(syms) == 0) {
        return(setNames(numeric(0), character(0)))
    }

    # Align tilts to risk universe
    t <- rep(1, length(syms))
    names(t) <- syms
    shared <- intersect(names(tilts), syms)
    t[shared] <- tilts[shared]

    # Apply tilt and renormalize to gross exposure
    w_tilted <- w_hrp * t
    w_tilted[w_tilted < 0] <- 0
    w_sum <- sum(w_tilted)
    if (w_sum > 0) {
        w_tilted <- w_tilted * (gross_exposure / w_sum)
    } else {
        w_tilted <- w_hrp * (gross_exposure / sum(w_hrp))
    }

    w_tilted
}

# ── Apply weight caps ─────────────────────────────────────────────────────────

#' @export
me_apply_caps <- function(w, max_weight = 0.15, max_iter = 20) {
    if (length(w) == 0) {
        return(w)
    }
    target_sum <- sum(w)
    if (target_sum <= 0) {
        return(w)
    }

    for (iter in seq_len(max_iter)) {
        over <- w > max_weight
        if (!any(over)) break

        overflow <- sum(w[over] - max_weight)
        w[over] <- max_weight

        # Redistribute overflow proportionally to non-capped
        under <- !over & w > 0
        if (!any(under)) break

        w[under] <- w[under] + overflow * (w[under] / sum(w[under]))
    }

    # Final renormalize
    w <- w * (target_sum / sum(w))
    w
}

# ── Full portfolio construction orchestrator ──────────────────────────────────

#' @export
me_build_portfolio_target <- function(risk_artifact, signal_artifact,
                                      state_gating_artifact, spec_portfolio,
                                      prev_target = NULL) {
    w_hrp <- risk_artifact$w_hrp
    gating <- state_gating_artifact$gating
    gross_exposure <- gating$gross_exposure
    w_cash <- gating$w_cash
    max_tilt <- spec_portfolio$tilt$max_tilt %||% 2.0
    max_weight <- spec_portfolio$caps$max_weight %||% 0.15

    # Risk universe
    risk_univ <- names(w_hrp)
    n_risk <- length(risk_univ)

    tilt_fallback <- FALSE
    tilt_reason <- "none"

    if (n_risk == 0) {
        tgt_df <- data.frame(
            symbol = character(0), weight_target = numeric(0),
            stringsAsFactors = FALSE
        )
        return(list(
            target_weights = tgt_df,
            cash_weight = 1.0,
            diag = list(
                n_risk = 0, tilt_fallback_to_hrp = TRUE,
                tilt_fallback_reason = "empty_universe"
            )
        ))
    }

    # 1. Combine expert scores
    combined <- me_combine_expert_scores(signal_artifact, gating, spec_portfolio)

    # 2. Score to tilt
    tilts <- me_score_to_tilt(combined, max_tilt)

    # 3. Apply tilts
    if (length(tilts) == 0 || all(tilts == 1)) {
        w_target <- w_hrp * (gross_exposure / sum(w_hrp))
        tilt_fallback <- TRUE
        tilt_reason <- "no_valid_scores"
    } else {
        w_target <- me_apply_tilts(w_hrp, tilts, gross_exposure)
    }

    # 4. Apply caps
    cap_infeasible <- FALSE
    w_capped <- me_apply_caps(w_target, max_weight)
    if (any(w_capped >= max_weight - 1e-6)) {
        cap_infeasible <- TRUE
    }

    # Ensure non-negative
    w_capped[w_capped < 0] <- 0
    w_capped <- w_capped * (gross_exposure / max(sum(w_capped), 1e-12))

    # Build target dataframe
    tgt_df <- data.frame(
        symbol = names(w_capped),
        weight_target = unname(w_capped),
        stringsAsFactors = FALSE
    )
    # Remove zero-weight positions
    tgt_df <- tgt_df[tgt_df$weight_target > 1e-8, , drop = FALSE]
    rownames(tgt_df) <- NULL

    # Recalculate cash
    actual_risky <- sum(tgt_df$weight_target)
    actual_cash <- 1 - actual_risky

    list(
        target_weights = tgt_df,
        cash_weight = actual_cash,
        diag = list(
            n_risk               = n_risk,
            n_active             = nrow(tgt_df),
            gross_exposure       = actual_risky,
            tilt_fallback_to_hrp = tilt_fallback,
            tilt_fallback_reason = tilt_reason,
            cap_infeasible       = cap_infeasible,
            max_weight_used      = max_weight,
            max_tilt_used        = max_tilt
        )
    )
}
