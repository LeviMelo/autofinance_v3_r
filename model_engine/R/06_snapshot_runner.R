#' @title Snapshot Runner
#' @description Orchestrate the full one-date modeling pipeline.

#' @export
me_run_snapshot <- function(data_bundle_or_panel, as_of_date, spec = NULL, prev_target = NULL, aux = list()) {
    spec <- me_get_spec(spec)
    me_validate_spec(spec)

    adapter <- me_make_data_adapter(data_bundle_or_panel, aux)

    cal <- adapter$calendar()
    if (!(as_of_date %in% cal)) {
        past_dates <- cal[cal <= as_of_date]
        if (length(past_dates) == 0) stop("No data before as_of_date")
        as_of_date <- max(past_dates)
    }

    syms <- adapter$investability_snapshot(as_of_date, spec$data)
    if (length(syms) == 0) {
        res_empty <- list(
            as_of_date = as_of_date,
            tradable_symbols = character(0),
            target_weights = data.frame(symbol = character(0), weight_target = numeric(0)),
            cash_weight = 1.0,
            risk = list(), signals = list(), market_state = list(), gating = list(), portfolio_diag = list(),
            meta = list(warns = "No tradable symbols"), warnings = character(0)
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

    warns <- character(0)

    # vov depends on vol sequence spanning states
    max_lkb <- max(lkb_kalman, lkb_tsmom, lkb_risk, lkb_disp, lkb_eta, lkb_vov + lkb_vol - 1) + 1
    prices_max <- adapter$price_matrix(as_of_date, lookback = max_lkb, symbols = syms, strict = FALSE)
    ret_max <- adapter$returns_matrix(as_of_date, lookback = max_lkb - 1, symbols = syms, strict = FALSE)

    avail <- nrow(ret_max)
    if (avail < max_lkb - 1) {
        warns <- c(warns, sprintf("Global history (%d) is less than max requested lookback (%d)", avail, max_lkb - 1))
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
        warns <<- c(warns, paste("Risk engine failed:", e$message))
        NULL
    })

    if (is.null(risk_artifact)) {
        res_err <- list(
            as_of_date = as_of_date,
            tradable_symbols = syms,
            target_weights = data.frame(symbol = character(0), weight_target = numeric(0)),
            cash_weight = 1.0,
            risk = list(), signals = list(), market_state = list(), gating = list(), portfolio_diag = list(),
            meta = list(), warnings = c(warns, "Reverting to cash position")
        )
        me_validate_snapshot_artifact(res_err)
        return(res_err)
    }

    # Exact alignment protocol: risk outputs govern the final cross-sectional universe
    risk_univ <- names(risk_artifact$w_hrp)
    sigma_t <- risk_artifact$sigma_t

    # Execute Signals and State using available data (subset cleanly downstream in portfolio)
    signal_artifact <- me_run_signal_engine(P_kalman, R_tsmom, sigma_t, spec$signals)

    state_gating_artifact <- me_run_state_and_gating(R_disp, R_eta, R_vov, risk_artifact, spec$market_state, spec$gating, vol_lookback = lkb_vol)

    port_artifact <- me_build_portfolio_target(risk_artifact, signal_artifact, state_gating_artifact, spec$portfolio, prev_target)

    if (nrow(port_artifact$target_weights) == 0 && port_artifact$cash_weight < 0.999) {
        warns <- c(warns, "Portfolio target has no risky weights but cash_weight < 1.0 (unexpected pruning/cap path).")
    }

    if (nrow(port_artifact$target_weights) == 0 && port_artifact$cash_weight >= 0.999) {
        warns <- c(warns, "Portfolio collapsed to cash (check gating logits, risk universe drops, and signal coverage).")
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
            risk_dropped = length(risk_artifact$diag$dropped_assets)
        ),
        lookbacks_used = list(
            kalman = lkb_kalman,
            tsmom = lkb_tsmom,
            risk = lkb_risk,
            vov_window = lkb_vov + lkb_vol - 1
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
        warnings = warns
    )

    me_validate_snapshot_artifact(res)
    res
}
