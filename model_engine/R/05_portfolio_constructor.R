#' @title Portfolio Constructor
#' @description Convert risk baseline + signals + gating into target weights.

#' @export
me_combine_expert_scores <- function(signal_artifact, gating_artifact, spec_portfolio) {
    wk <- gating_artifact$w_kalman
    wm <- gating_artifact$w_tsmom

    w_tot <- wk + wm
    if (w_tot == 0) {
        v <- signal_artifact$kalman * 0
        names(v) <- names(signal_artifact$kalman)
        return(v)
    }

    wk <- wk / w_tot
    wm <- wm / w_tot

    S_t <- (wk * signal_artifact$kalman) + (wm * signal_artifact$tsmom)
    S_t
}

#' @export
me_score_to_tilt <- function(S_t, spec_tilt) {
    max_tilt <- spec_tilt$max_tilt %||% 2.0
    mult <- exp(S_t * log(max_tilt))
    mult
}

#' @export
me_apply_tilt_to_baseline <- function(w_hrp, tilt_mult, spec_portfolio) {
    # Align vectors
    mult_align <- rep(1.0, length(w_hrp))
    names(mult_align) <- names(w_hrp)
    found <- intersect(names(tilt_mult), names(w_hrp))
    mult_align[found] <- tilt_mult[found]

    w <- w_hrp * mult_align
    if (sum(w) > 0) w / sum(w) else w_hrp
}

#' @export
me_apply_weight_caps <- function(w, spec_caps) {
    cap <- spec_caps$max_weight %||% 0.15
    if (cap >= 1.0) {
        return(w)
    }

    if (length(w) * cap < 1.0) {
        # Cannot cap if mathematically impossible to sum to 1
        return(w)
    }

    # Iterative proportional redistribution
    for (iter in 1:100) {
        if (all(w <= cap + 1e-6)) break

        cap_idx <- w > cap
        excess <- sum(w[cap_idx] - cap)
        w[cap_idx] <- cap

        uncapped_idx <- w < cap
        if (sum(uncapped_idx) == 0) break

        # Redistribute proportionally to uncapped weights
        w_un <- w[uncapped_idx]
        if (sum(w_un) > 0) {
            w[uncapped_idx] <- w_un + excess * (w_un / sum(w_un))
        } else {
            w[uncapped_idx] <- w_un + excess / length(w_un)
        }
    }
    w / sum(w)
}

#' @export
me_build_portfolio_target <- function(risk_artifact, signal_artifact, state_gating_artifact, spec_portfolio, prev_target = NULL) {
    g_t <- state_gating_artifact$gating$gross_exposure
    w_hrp <- risk_artifact$w_hrp

    S_t <- me_combine_expert_scores(signal_artifact, state_gating_artifact$gating, spec_portfolio)
    tilt_mult <- me_score_to_tilt(S_t, spec_portfolio$tilt)

    w_tilted <- me_apply_tilt_to_baseline(w_hrp, tilt_mult, spec_portfolio)
    w_capped <- me_apply_weight_caps(w_tilted, spec_portfolio$caps)

    w_final_risky <- g_t * w_capped
    w_cash <- 1 - g_t

    w_final_risky[w_final_risky < 1e-4] <- 0

    tgt_table <- data.frame(
        symbol = names(w_final_risky),
        weight_target = unname(w_final_risky),
        stringsAsFactors = FALSE
    )

    tgt_table <- tgt_table[tgt_table$weight_target > 0, ]

    list(
        target_weights = tgt_table,
        cash_weight = w_cash,
        diag = list(
            cap_hits = sum(w_capped >= (spec_portfolio$caps$max_weight %||% 1) - 1e-4),
            pre_cap_sum = sum(w_tilted),
            post_cap_sum = sum(w_capped),
            gross_exposure = g_t
        )
    )
}
