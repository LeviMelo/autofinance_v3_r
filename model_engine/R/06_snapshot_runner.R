#' @title Model Engine — Snapshot Runner
#' @description Orchestrate the full one-date modeling pipeline.

# ── Internal helpers ──────────────────────────────────────────────────────────

.slice_mat <- function(mat, n) {
    if (is.null(mat) || nrow(mat) == 0) {
        return(mat)
    }
    tail(mat, min(n, nrow(mat)))
}

# ── Main snapshot runner ──────────────────────────────────────────────────────

#' @export
me_run_snapshot <- function(data_bundle_or_panel, as_of_date, spec = NULL,
                            prev_target = NULL, model_state = NULL,
                            aux = list()) {
    spec <- me_get_spec(spec)
    me_validate_spec(spec)

    # ── Warning accumulator ──
    warns <- character(0)
    .push_warn <- function(msg) warns <<- c(warns, msg)

    # ── Build adapter ──
    adapter <- me_make_data_adapter(data_bundle_or_panel, aux)

    # ── Snap to available calendar ──
    cal <- adapter$calendar()
    if (length(cal) == 0) stop("Empty calendar in data adapter")

    orig_date <- as_of_date
    if (!as_of_date %in% cal) {
        as_of_date <- max(cal[cal <= as_of_date])
        if (length(as_of_date) == 0 || is.na(as_of_date)) {
            stop("No calendar dates at or before as_of_date")
        }
        .push_warn(sprintf(
            "Snapped as_of_date from %s to %s",
            as.character(orig_date), as.character(as_of_date)
        ))
    }

    # ── Investability filter ──
    syms <- adapter$investability_snapshot(as_of_date, spec$data)

    if (length(syms) == 0) {
        .push_warn("No tradable symbols after investability filter")
        res_empty <- list(
            as_of_date = as_of_date,
            tradable_symbols = character(0),
            target_weights = data.frame(
                symbol = character(0),
                weight_target = numeric(0)
            ),
            cash_weight = 1.0,
            risk = list(),
            signals = list(),
            market_state = list(),
            gating = list(),
            portfolio_diag = list(),
            meta = list(spec_hash = me_hash_spec(spec)),
            warnings = warns
        )
        me_validate_snapshot_artifact(res_empty)
        return(res_empty)
    }

    # ── Compute lookbacks ──
    lkb_risk <- max(
        spec$risk$vol$lookback %||% 252L,
        spec$risk$pca$lookback %||% 252L
    )
    lkb_kalman <- spec$signals$kalman$lookback %||% 252L
    lkb_tsmom <- spec$signals$tsmom$horizon %||% 252L
    lkb_disp <- spec$market_state$dispersion$lookback %||% 63L
    lkb_eta <- spec$market_state$eta$lookback %||% 126L
    lkb_vov <- spec$market_state$vov$lookback %||% 63L
    lkb_vol <- spec$market_state$vov$vol_lookback %||% 21L

    max_lkb <- max(
        lkb_risk, lkb_kalman, lkb_tsmom + 1,
        lkb_eta, lkb_vov + lkb_vol
    )

    # ── Fetch price and return matrices ──
    prices_max <- adapter$price_matrix(as_of_date, max_lkb + 1,
        "close", syms,
        strict = TRUE
    )
    if (ncol(prices_max) == 0) {
        .push_warn("No assets with complete price history")
        res_empty <- list(
            as_of_date = as_of_date,
            tradable_symbols = character(0),
            target_weights = data.frame(
                symbol = character(0),
                weight_target = numeric(0)
            ),
            cash_weight = 1.0,
            risk = list(), signals = list(), market_state = list(),
            gating = list(), portfolio_diag = list(),
            meta = list(spec_hash = me_hash_spec(spec)),
            warnings = warns
        )
        me_validate_snapshot_artifact(res_empty)
        return(res_empty)
    }

    # Update symbol universe to what has data
    syms <- colnames(prices_max)

    ret_max <- diff(log(prices_max))
    ret_max[!is.finite(ret_max)] <- 0

    # ── Slice windows ──
    R_risk <- .slice_mat(ret_max, lkb_risk)
    P_kalman <- .slice_mat(prices_max, lkb_kalman)
    R_tsmom <- .slice_mat(ret_max, lkb_tsmom)
    R_disp <- .slice_mat(ret_max, lkb_disp)
    R_eta <- .slice_mat(ret_max, lkb_eta)
    R_vov <- .slice_mat(ret_max, lkb_vov + lkb_vol - 1)

    # ══════════════════════════════════════════════════════════════════════════
    # STAGE 1: Risk Engine
    # ══════════════════════════════════════════════════════════════════════════

    risk_artifact <- tryCatch(
        me_run_risk_engine(R_risk, spec$risk),
        error = function(e) {
            .push_warn(paste("Risk engine failed:", e$message))
            NULL
        }
    )

    if (is.null(risk_artifact)) {
        res_err <- list(
            as_of_date = as_of_date,
            tradable_symbols = syms,
            target_weights = data.frame(
                symbol = character(0),
                weight_target = numeric(0)
            ),
            cash_weight = 1.0,
            risk = list(), signals = list(), market_state = list(),
            gating = list(), portfolio_diag = list(risk_failed = TRUE),
            meta = list(spec_hash = me_hash_spec(spec)),
            warnings = warns
        )
        me_validate_snapshot_artifact(res_err)
        return(res_err)
    }

    # Risk diagnostics
    rd <- risk_artifact$diag
    if (!is.null(rd$n_assets_dropped) && rd$n_assets_dropped > 0) {
        .push_warn(sprintf(
            "Risk dropped %d/%d assets (%.0f%%)",
            rd$n_assets_dropped, rd$n_assets_input,
            100 * rd$frac_assets_dropped
        ))
    }
    if (isTRUE(rd$was_repaired)) {
        .push_warn("Covariance required nearPD repair")
    }
    if (isTRUE(rd$allocator_fallback)) {
        .push_warn(sprintf("HRP fallback: %s", rd$allocator_method))
    }

    # Canonical risk universe
    risk_univ <- names(risk_artifact$w_hrp)

    # ══════════════════════════════════════════════════════════════════════════
    # STAGE 2: Signal Engine
    # ══════════════════════════════════════════════════════════════════════════

    # Restrict to risk universe
    P_sig <- P_kalman[, intersect(colnames(P_kalman), risk_univ), drop = FALSE]
    R_sig <- R_tsmom[, intersect(colnames(R_tsmom), risk_univ), drop = FALSE]

    signal_artifact <- tryCatch(
        me_run_signal_engine(
            P_sig, R_sig, risk_artifact$sigma_t,
            spec$signals,
            E_window = risk_artifact$E_t
        ),
        error = function(e) {
            .push_warn(paste("Signal engine failed:", e$message))
            NULL
        }
    )

    if (is.null(signal_artifact)) {
        signal_artifact <- list(
            kalman = setNames(rep(0, length(risk_univ)), risk_univ),
            tsmom  = setNames(rep(0, length(risk_univ)), risk_univ),
            diag   = list(kalman_all_zero = TRUE, tsmom_all_zero = TRUE)
        )
        .push_warn("Signal engine returned null — using zero scores")
    } else {
        sdg <- signal_artifact$diag
        if (isTRUE(sdg$kalman_all_zero)) {
            .push_warn("Kalman signal returned all-zero scores")
        }
        if (isTRUE(sdg$tsmom_all_zero)) {
            .push_warn("TSMOM signal returned all-zero scores")
        }
    }

    # ══════════════════════════════════════════════════════════════════════════
    # STAGE 3: State and Gating
    # ══════════════════════════════════════════════════════════════════════════

    state_gating_artifact <- withCallingHandlers(
        me_run_state_and_gating(
            R_disp, R_eta, R_vov, risk_artifact,
            spec$market_state, spec$gating,
            vol_lookback = lkb_vol
        ),
        warning = function(w) {
            .push_warn(w$message)
            invokeRestart("muffleWarning")
        }
    )

    # ══════════════════════════════════════════════════════════════════════════
    # STAGE 4: Portfolio Construction
    # ══════════════════════════════════════════════════════════════════════════

    port_artifact <- me_build_portfolio_target(
        risk_artifact, signal_artifact, state_gating_artifact,
        spec$portfolio, prev_target
    )

    pd <- port_artifact$diag
    if (isTRUE(pd$tilt_fallback_to_hrp)) {
        .push_warn(sprintf(
            "Tilt fallback to pure HRP: %s",
            pd$tilt_fallback_reason %||% "unknown"
        ))
    }
    if (isTRUE(pd$cap_infeasible)) {
        .push_warn("Weight caps were binding")
    }

    # ══════════════════════════════════════════════════════════════════════════
    # Assemble artifact
    # ══════════════════════════════════════════════════════════════════════════

    # Optionally strip large matrices
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
            signal_common = signal_artifact$diag$n_common %||% 0,
            active_names = nrow(port_artifact$target_weights)
        ),
        timestamps = list(
            as_of_date = as_of_date,
            snapped    = !identical(orig_date, as_of_date)
        )
    )

    res <- list(
        as_of_date       = as_of_date,
        tradable_symbols = risk_univ,
        target_weights   = port_artifact$target_weights,
        cash_weight      = port_artifact$cash_weight,
        risk             = risk_artifact,
        signals          = signal_artifact,
        market_state     = state_gating_artifact$market_state,
        gating           = state_gating_artifact$gating,
        portfolio_diag   = pd,
        meta             = meta,
        warnings         = unique(warns)
    )

    me_validate_snapshot_artifact(res)
    res
}
