#' @title Model Engine — Snapshot Runner
#' @description Full one-date pipeline orchestrator integrating all modules.
#' architecture.md §16: end-to-end update flow.

.slice_mat <- function(mat, n) {
    if (is.null(mat) || nrow(mat) == 0) {
        return(mat)
    }
    tail(mat, min(n, nrow(mat)))
}

#' @export
me_run_snapshot <- function(data_bundle_or_panel, as_of_date, spec = NULL,
                            prev_target = NULL, model_state = NULL,
                            aux = list()) {
    spec <- me_get_spec(spec)
    me_validate_spec(spec)

    warns <- character(0)
    .w <- function(msg) warns <<- c(warns, msg)

    # ══════════════════════════════════════════════════════════════════════════
    # STAGE 0: Data adapter + calendar + investability
    # ══════════════════════════════════════════════════════════════════════════

    adapter <- me_make_data_adapter(data_bundle_or_panel, aux)
    cal <- adapter$calendar()
    if (length(cal) == 0) stop("Empty calendar")

    orig_date <- as_of_date
    if (!as_of_date %in% cal) {
        as_of_date <- max(cal[cal <= as_of_date])
        if (length(as_of_date) == 0 || is.na(as_of_date)) {
            stop("No calendar dates at or before as_of_date")
        }
        .w(sprintf("Snapped as_of_date from %s to %s", orig_date, as_of_date))
    }

    syms <- adapter$investability_snapshot(as_of_date, spec$data)

    .empty_result <- function() {
        res <- list(
            as_of_date = as_of_date, tradable_symbols = character(0),
            target_weights = data.frame(
                symbol = character(0),
                weight_target = numeric(0)
            ),
            cash_weight = 1.0,
            risk = list(), signals = list(), market_state = list(),
            gating = list(), graph = list(), features = list(),
            forecast = list(), portfolio_diag = list(),
            meta = list(spec_hash = me_hash_spec(spec)),
            warnings = warns
        )
        me_validate_snapshot_artifact(res)
        res
    }

    if (length(syms) == 0) {
        .w("No tradable symbols")
        return(.empty_result())
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
        .w("No complete price history")
        return(.empty_result())
    }

    syms <- colnames(prices_max)
    ret_max <- diff(log(prices_max))
    ret_max[!is.finite(ret_max)] <- 0

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
            .w(paste("Risk failed:", e$message))
            NULL
        }
    )
    if (is.null(risk_artifact)) {
        return(.empty_result())
    }

    rd <- risk_artifact$diag
    if (rd$n_assets_dropped > 0) {
        .w(sprintf("Risk dropped %d/%d assets", rd$n_assets_dropped, rd$n_assets_input))
    }
    if (isTRUE(rd$was_repaired)) .w("Covariance required nearPD repair")

    risk_univ <- names(risk_artifact$w_hrp)

    # ══════════════════════════════════════════════════════════════════════════
    # STAGE 2: Graph and Structure (§§5-6, 9-10)
    # ══════════════════════════════════════════════════════════════════════════

    prev_P_bar <- if (!is.null(model_state)) model_state$prev_P_bar else NULL
    prev_labels <- if (!is.null(model_state)) model_state$prev_labels else NULL

    # Need Glasso enabled for graph pipeline
    spec_graph <- spec$graph %||% list()
    graph_artifact <- tryCatch(
        me_run_graph_pipeline(risk_artifact, spec_graph, prev_P_bar, prev_labels),
        error = function(e) {
            .w(paste("Graph failed:", e$message))
            NULL
        }
    )

    # ══════════════════════════════════════════════════════════════════════════
    # STAGE 3: Signal Engine (§§7-8)
    # ══════════════════════════════════════════════════════════════════════════

    P_sig <- P_kalman[, intersect(colnames(P_kalman), risk_univ), drop = FALSE]
    R_sig <- R_tsmom[, intersect(colnames(R_tsmom), risk_univ), drop = FALSE]

    signal_artifact <- tryCatch(
        me_run_signal_engine(P_sig, R_sig, risk_artifact$sigma_t,
            spec$signals,
            E_window = risk_artifact$E_t,
            B_t = risk_artifact$B_t,
            F_window = risk_artifact$F_t
        ),
        error = function(e) {
            .w(paste("Signal failed:", e$message))
            list(
                kalman = setNames(rep(0, length(risk_univ)), risk_univ),
                tsmom = setNames(rep(0, length(risk_univ)), risk_univ),
                diag = list(kalman_all_zero = TRUE, tsmom_all_zero = TRUE)
            )
        }
    )

    # ══════════════════════════════════════════════════════════════════════════
    # STAGE 4: State and Gating (§11.5, §12.4)
    # ══════════════════════════════════════════════════════════════════════════

    state_gating_artifact <- withCallingHandlers(
        me_run_state_and_gating(
            R_disp, R_eta, R_vov, risk_artifact,
            spec$market_state, spec$gating, lkb_vol
        ),
        warning = function(w) {
            .w(w$message)
            invokeRestart("muffleWarning")
        }
    )

    # ══════════════════════════════════════════════════════════════════════════
    # STAGE 5: Feature Engine (§11)
    # ══════════════════════════════════════════════════════════════════════════

    feature_artifact <- tryCatch(
        me_run_feature_engine(
            signal_artifact, risk_artifact, graph_artifact,
            state_gating_artifact, adapter, as_of_date,
            risk_univ
        ),
        error = function(e) {
            .w(paste("Feature engine failed:", e$message))
            list(
                X = matrix(0, length(risk_univ), 0,
                    dimnames = list(risk_univ, NULL)
                ),
                feature_names = character(0),
                diag = list(n_features = 0)
            )
        }
    )

    # ══════════════════════════════════════════════════════════════════════════
    # STAGE 6: Forecast Engine (§12)
    # ══════════════════════════════════════════════════════════════════════════

    spec_forecast <- spec$forecast %||% list()
    forecast_artifact <- tryCatch(
        {
            me_run_forecast_engine(
                X_current = feature_artifact$X,
                R_window = R_risk,
                X_hist_window = NULL, # future: carry feature history
                gating_artifact = state_gating_artifact,
                risk_artifact = risk_artifact,
                spec_forecast = spec_forecast
            )
        },
        error = function(e) {
            .w(paste("Forecast engine failed:", e$message))
            NULL
        }
    )

    # ══════════════════════════════════════════════════════════════════════════
    # STAGE 7: Portfolio Construction (§§13-14)
    # ══════════════════════════════════════════════════════════════════════════

    port_artifact <- me_build_portfolio_target(
        risk_artifact, signal_artifact, state_gating_artifact,
        spec$portfolio,
        forecast_artifact = forecast_artifact,
        graph_artifact = graph_artifact,
        prev_target = prev_target
    )

    pd <- port_artifact$diag
    if (identical(pd$method, "tilt_fallback") || identical(pd$method, "tilt_signal")) {
        .w(sprintf("Portfolio used %s method", pd$method))
    }

    # ══════════════════════════════════════════════════════════════════════════
    # Assemble artifact
    # ══════════════════════════════════════════════════════════════════════════

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
            investable    = length(syms),
            risk_kept     = length(risk_univ),
            risk_dropped  = length(risk_artifact$diag$dropped_assets),
            signal_common = signal_artifact$diag$n_common %||% 0,
            active_names  = nrow(port_artifact$target_weights)
        ),
        feature_info = list(
            n_features = feature_artifact$diag$n_features,
            groups     = feature_artifact$feature_groups %||% list()
        ),
        forecast_info = if (!is.null(forecast_artifact)) forecast_artifact$diag else list(),
        graph_info = if (!is.null(graph_artifact)) graph_artifact$diag else list(),
        timestamps = list(
            as_of_date = as_of_date,
            snapped = !identical(orig_date, as_of_date)
        )
    )

    res <- list(
        as_of_date = as_of_date,
        tradable_symbols = risk_univ,
        target_weights = port_artifact$target_weights,
        cash_weight = port_artifact$cash_weight,
        risk = risk_artifact,
        signals = signal_artifact,
        market_state = state_gating_artifact$market_state,
        gating = state_gating_artifact$gating,
        graph = if (!is.null(graph_artifact)) {
            list(
                clustering = graph_artifact$clustering,
                diag = graph_artifact$diag
            )
        } else {
            list()
        },
        features = list(
            n_features = feature_artifact$diag$n_features,
            groups     = feature_artifact$feature_groups %||% list()
        ),
        forecast = if (!is.null(forecast_artifact)) {
            list(
                combined = forecast_artifact$combined_forecast,
                confidence = forecast_artifact$confidence$avg_confidence,
                uncertainty = forecast_artifact$uncertainty
            )
        } else {
            list()
        },
        portfolio_diag = pd,
        meta = meta,
        warnings = unique(warns)
    )

    me_validate_snapshot_artifact(res)
    res
}
