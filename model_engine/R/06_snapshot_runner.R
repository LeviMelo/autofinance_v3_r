#' @title Snapshot Runner
#' @description Orchestrate the full one-date modeling pipeline.

#' @export
me_run_snapshot <- function(data_bundle_or_panel, as_of_date, spec = NULL, prev_target = NULL, aux = list()) {
    spec <- me_get_spec(spec)
    me_validate_spec(spec)

    adapter <- me_make_data_adapter(data_bundle_or_panel, aux)

    warns <- character(0)
    .push_warn <- function(msg) {
        warns <<- c(warns, as.character(msg))
        invisible(NULL)
    }

    # Explicit placeholders / reserved interface warnings
    if (!is.null(prev_target)) {
        .push_warn("prev_target was supplied but is currently not used by model_engine (turnover-aware construction not implemented yet).")
    }
    if (length(aux) > 0) {
        .push_warn("aux was supplied but is currently unused by model_engine.")
    }

    cal <- adapter$calendar()
    if (!(as_of_date %in% cal)) {
        past_dates <- cal[cal <= as_of_date]
        if (length(past_dates) == 0) stop("No data before as_of_date")
        orig_date <- as_of_date
        as_of_date <- max(past_dates)
        .push_warn(sprintf(
            "as_of_date %s not in calendar; snapped back to latest available date %s.",
            as.character(orig_date), as.character(as_of_date)
        ))
    }

    syms <- adapter$investability_snapshot(as_of_date, spec$data)

    if (length(syms) == 0) {
        # Extra diagnostic for type-mismatch path
        sub_asof <- adapter$panel_upto(as_of_date)
        if (nrow(sub_asof) > 0 && "asset_type" %in% names(sub_asof)) {
            panel_types <- sort(unique(as.character(sub_asof$asset_type)))
            allowed_types <- as.character(spec$data$allowed_types %||% character(0))
            overlap <- intersect(panel_types, allowed_types)

            if (length(panel_types) > 0 && length(overlap) == 0) {
                .push_warn(sprintf(
                    "No tradable symbols. asset_type mismatch likely: panel has {%s} but spec$data$allowed_types = {%s}.",
                    paste(panel_types, collapse = ", "),
                    paste(allowed_types, collapse = ", ")
                ))
            } else {
                .push_warn("No tradable symbols after investability filters (coverage/liquidity/type constraints).")
            }
        } else {
            .push_warn("No tradable symbols (empty panel up to as_of_date or investability filters removed all assets).")
        }

        res_empty <- list(
            as_of_date = as_of_date,
            tradable_symbols = character(0),
            target_weights = data.frame(symbol = character(0), weight_target = numeric(0)),
            cash_weight = 1.0,
            risk = list(), signals = list(), market_state = list(), gating = list(), portfolio_diag = list(),
            meta = list(
                warns = "No tradable symbols",
                spec_hash = me_hash_spec(spec)
            ),
            warnings = warns
        )
        me_validate_snapshot_artifact(res_empty)
        return(res_empty)
    }

    # Module specific lookbacks
    lkb_kalman <- spec$signals$kalman$lookback %||% 252L
    lkb_tsmom <- spec$signals$tsmom$horizon %||% 252L
    lkb_risk <- max(spec$risk$vol$lookback %||% 252L, spec$risk$pca$lookback %||% 252L)
    lkb_disp <- spec$market_state$dispersion$lookback %||% 1L
    lkb_eta <- spec$market_state$eta$lookback %||% 63L
    lkb_vov <- spec$market_state$vov$lookback %||% 126L
    lkb_vol <- spec$risk$vol$lookback %||% 63L

    # vov depends on vol sequence spanning states
    max_lkb <- max(lkb_kalman, lkb_tsmom, lkb_risk, lkb_disp, lkb_eta, lkb_vov + lkb_vol - 1) + 1
    prices_max <- adapter$price_matrix(as_of_date, lookback = max_lkb, symbols = syms, strict = FALSE)
    ret_max <- adapter$returns_matrix(as_of_date, lookback = max_lkb - 1, symbols = syms, strict = FALSE)

    avail <- nrow(ret_max)
    if (avail < max_lkb - 1) {
        .push_warn(sprintf("Global history (%d) is less than max requested lookback (%d).", avail, max_lkb - 1))
    }

    .slice_mat <- function(mat, n) {
        if (nrow(mat) < n) {
            return(mat)
        }
        tail(mat, n)
    }

    R_risk <- .slice_mat(ret_max, lkb_risk)
    P_kalman <- .slice_mat(prices_max, lkb_kalman)
    R_tsmom <- .slice_mat(ret_max, lkb_tsmom)
    R_disp <- .slice_mat(ret_max, lkb_disp)
    R_eta <- .slice_mat(ret_max, lkb_eta)
    R_vov <- .slice_mat(ret_max, lkb_vov + lkb_vol - 1)

    # Execute Risk Canonical Base First
    risk_artifact <- tryCatch(me_run_risk_engine(R_risk, spec$risk), error = function(e) {
        .push_warn(paste("Risk engine failed:", e$message))
        NULL
    })

    if (is.null(risk_artifact)) {
        res_err <- list(
            as_of_date = as_of_date,
            tradable_symbols = syms,
            target_weights = data.frame(symbol = character(0), weight_target = numeric(0)),
            cash_weight = 1.0,
            risk = list(), signals = list(), market_state = list(), gating = list(), portfolio_diag = list(),
            meta = list(spec_hash = me_hash_spec(spec)),
            warnings = c(warns, "Reverting to cash position")
        )
        me_validate_snapshot_artifact(res_err)
        return(res_err)
    }

    # Risk diagnostics -> warnings
    rd <- risk_artifact$diag
    if (!is.null(rd$n_assets_input) && !is.null(rd$n_assets_dropped) && rd$n_assets_dropped > 0) {
        .push_warn(sprintf(
            "Risk universe NA-drop: dropped %d/%d assets (%.1f%%) due to missing data in risk lookback.",
            rd$n_assets_dropped, rd$n_assets_input, 100 * (rd$frac_assets_dropped %||% 0)
        ))
    }
    if (isTRUE(rd$was_repaired)) {
        .push_warn("Risk covariance matrix required nearPD repair before allocation.")
    }
    if (isTRUE(rd$allocator_fallback)) {
        .push_warn(sprintf(
            "Allocator fallback engaged: %s (%s).",
            rd$allocator_method %||% "unknown",
            rd$allocator_reason %||% "unknown"
        ))
    }

    # Exact alignment protocol: risk outputs govern the final cross-sectional universe
    risk_univ <- names(risk_artifact$w_hrp)
    sigma_t <- risk_artifact$sigma_t

    # Execute Signals and State using available data (subset cleanly downstream in portfolio)
    signal_artifact <- me_run_signal_engine(P_kalman, R_tsmom, sigma_t, spec$signals)

    sdg <- signal_artifact$diag
    if (isTRUE(sdg$both_all_zero)) {
        .push_warn("Both signal experts returned all-zero scores (alpha layer effectively neutralized).")
    } else {
        if (isTRUE(sdg$kalman_all_zero)) .push_warn("Kalman signal returned all-zero scores.")
        if (isTRUE(sdg$tsmom_all_zero)) .push_warn("TSMOM signal returned all-zero scores.")
    }

    state_gating_artifact <- me_run_state_and_gating(R_disp, R_eta, R_vov, risk_artifact, spec$market_state, spec$gating, vol_lookback = lkb_vol)

    sgd <- state_gating_artifact$diag
    if (isTRUE(sgd$eta_likely_defaulted)) .push_warn("Market-state eta likely defaulted/neutral due to insufficient clean data.")
    if (isTRUE(sgd$vov_likely_defaulted)) .push_warn("Market-state VoV likely defaulted/neutral due to insufficient history.")
    if (isTRUE(sgd$disp_likely_degenerate)) .push_warn("Market-state dispersion likely degenerate due to low cross-sectional breadth.")
    if (isTRUE(sgd$gating_is_static)) .push_warn("Gating W matrix is all zeros; gating is static (state features do not affect expert/cash weights).")
    if (isFALSE(sgd$risk_artifact_used)) .push_warn("State module currently accepts risk_artifact but does not use it (reserved interface).")

    port_artifact <- me_build_portfolio_target(risk_artifact, signal_artifact, state_gating_artifact, spec$portfolio, prev_target)

    pd <- port_artifact$diag
    if (isTRUE(pd$tilt_fallback_to_hrp)) {
        .push_warn(sprintf(
            "Portfolio tilt path fell back to pure HRP baseline (%s).",
            pd$tilt_fallback_reason %||% "unknown"
        ))
    }
    if (isTRUE(pd$cap_infeasible)) {
        .push_warn(sprintf(
            "Weight cap infeasible for current universe size (%s); cap step returned uncapped weights.",
            pd$cap_reason %||% "unknown"
        ))
    }
    if (isTRUE(pd$prev_target_supplied) && isFALSE(pd$prev_target_used)) {
        .push_warn("prev_target was supplied but not used in portfolio construction (no turnover-aware transition logic yet).")
    }

    if (nrow(port_artifact$target_weights) == 0 && port_artifact$cash_weight < 0.999) {
        .push_warn("Portfolio target has no risky weights but cash_weight < 1.0 (unexpected pruning/cap path).")
    }

    if (nrow(port_artifact$target_weights) == 0 && port_artifact$cash_weight >= 0.999) {
        .push_warn("Portfolio collapsed to cash (check gating logits, risk universe drops, and signal coverage).")
    }

    if (isFALSE(spec$meta$retain_windows)) {
        risk_artifact$E_t <- NULL
        risk_artifact$F_t <- NULL
    }
    if (isFALSE(spec$meta$retain_matrices)) {
        risk_artifact$Sigma_f <- NULL
        risk_artifact$Sigma_eps <- NULL
        risk_artifact$Theta_eps <- NULL
        risk_artifact$Sigma_total <- NULL
    }

    meta <- list(
        spec_hash = me_hash_spec(spec),
        universe_counts = list(
            investable = length(syms),
            risk_kept = length(risk_univ),
            risk_dropped = length(risk_artifact$diag$dropped_assets),
            final_risky_names = nrow(port_artifact$target_weights)
        ),
        lookbacks_used = list(
            kalman = lkb_kalman,
            tsmom = lkb_tsmom,
            risk = lkb_risk,
            vov_window = lkb_vov + lkb_vol - 1
        ),
        implementation_flags = list(
            prev_target_used = FALSE,
            state_uses_risk_artifact = FALSE,
            aux_used = FALSE
        )
    )

    res <- list(
        as_of_date = as_of_date,
        tradable_symbols = syms,
        target_weights = port_artifact$target_weights,
        cash_weight = port_artifact$cash_weight,
        risk = risk_artifact,
        signals = signal_artifact,
        market_state = state_gating_artifact$market_state,
        gating = state_gating_artifact$gating,
        portfolio_diag = port_artifact$diag,
        meta = meta,
        warnings = unique(warns)
    )

    me_validate_snapshot_artifact(res)
    res
}
