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
    me_validate_model_state(model_state)

    # Convenience: if caller passed model_state but forgot prev_target separately,
    # recover it from state.
    if (is.null(prev_target) && !is.null(model_state) && !is.null(model_state$prev_target)) {
        prev_target <- model_state$prev_target
    }

    warns <- character(0)
    runtime_warns <- character(0)
    fallbacks <- list()
    architecture_violations <- character(0)

    .w <- function(msg) {
        warns <<- unique(c(warns, as.character(msg)))
    }

    .rw <- function(stage, msg) {
        full <- sprintf("[%s][WARN] %s", stage, as.character(msg))
        runtime_warns <<- unique(c(runtime_warns, full))
        warns <<- unique(c(warns, full))
        if (isTRUE(spec$meta$strict_warnings)) {
            stop(sprintf("Strict warning mode: %s", full), call. = FALSE)
        }
    }

    .fb <- function(stage, code, message, severity = "warn") {
        entry <- list(
            stage = as.character(stage),
            code = as.character(code),
            message = as.character(message),
            severity = as.character(severity)
        )
        fallbacks[[length(fallbacks) + 1L]] <<- entry
        .w(sprintf("[%s][FALLBACK:%s] %s", stage, code, message))
        if (isTRUE(spec$meta$strict_fallbacks)) {
            stop(sprintf("Strict fallback mode triggered at stage '%s' (%s)", stage, code), call. = FALSE)
        }
    }

    .archv <- function(message) {
        architecture_violations <<- unique(c(architecture_violations, as.character(message)))
        .w(sprintf("[ARCH] %s", message))
        if (isTRUE(spec$meta$strict_architecture)) {
            stop(sprintf("Strict architecture mode: %s", message), call. = FALSE)
        }
    }

    .run_stage <- function(stage_name, expr) {
        withCallingHandlers(
            tryCatch(
                expr,
                error = function(e) {
                    .w(sprintf("[%s][ERROR] %s", stage_name, conditionMessage(e)))
                    NULL
                }
            ),
            warning = function(w) {
                if (isTRUE(spec$meta$capture_stage_warnings %||% TRUE)) {
                    .rw(stage_name, conditionMessage(w))
                    invokeRestart("muffleWarning")
                }
            }
        )
    }

    # ══════════════════════════════════════════════════════════════════════════
    # STAGE 0: Data adapter + calendar + investability/admissibility
    # ══════════════════════════════════════════════════════════════════════════

    adapter <- .run_stage("data_adapter", me_make_data_adapter(data_bundle_or_panel, aux))
    if (is.null(adapter)) stop("Failed to create data adapter")

    cal <- .run_stage("calendar", adapter$calendar())
    if (is.null(cal) || length(cal) == 0) stop("Empty calendar")

    orig_date <- as_of_date
    if (!as_of_date %in% cal) {
        as_of_date <- max(cal[cal <= as_of_date])
        if (length(as_of_date) == 0 || is.na(as_of_date)) {
            stop("No calendar dates at or before as_of_date")
        }
        .w(sprintf("Snapped as_of_date from %s to %s", orig_date, as_of_date))
    }

    syms_investable <- .run_stage("investability", adapter$investability_snapshot(as_of_date, spec$data))
    if (is.null(syms_investable)) syms_investable <- character(0)

    # Admissible universe = investable ∪ previous holdings (architecture-consistent transitional patch)
    eps_hold <- 1e-8
    syms_prev_hold <- character(0)
    if (!is.null(prev_target) && is.data.frame(prev_target) &&
        all(c("symbol", "weight_target") %in% names(prev_target))) {
        syms_prev_hold <- prev_target$symbol[is.finite(prev_target$weight_target) & (prev_target$weight_target > eps_hold)]
        syms_prev_hold <- unique(as.character(syms_prev_hold))
    }
    syms <- unique(c(as.character(syms_investable), syms_prev_hold))
    if (length(setdiff(syms_prev_hold, syms_investable)) > 0) {
        .w(sprintf(
            "Admissible universe extended with %d previous holdings outside investability snapshot",
            length(setdiff(syms_prev_hold, syms_investable))
        ))
    }

    .empty_result <- function() {
        architecture_flags <- list(
            glasso_residual_precision_configured = isTRUE(spec$risk$resid$use_glasso),
            glasso_residual_precision_used = FALSE,
            horizon_covariance_available = FALSE,
            forecast_feature_history_available = FALSE,
            forecast_stage_executed = FALSE,
            forecast_asset_level_training_implemented = FALSE,
            legacy_hrp_alias_present = FALSE
        )

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
            meta = list(
                spec_hash = me_hash_spec(spec),
                fallbacks = fallbacks,
                runtime_warnings = runtime_warns,
                architecture_flags = architecture_flags,
                architecture_violations = architecture_violations
            ),
            model_state_out = list(
                prev_P_bar = NULL,
                prev_labels = NULL,
                prev_target = data.frame(
                    symbol = character(0),
                    weight_target = numeric(0),
                    stringsAsFactors = FALSE
                )
            ),
            warnings = warns
        )
        me_validate_snapshot_artifact(res)
        res
    }

    if (length(syms) == 0) {
        .w("No admissible symbols")
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
    prices_max <- .run_stage(
        "price_matrix",
        adapter$price_matrix(as_of_date, max_lkb + 1, "close", syms, strict = TRUE)
    )

    if (is.null(prices_max) || ncol(prices_max) == 0) {
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

    risk_artifact <- .run_stage("risk", me_run_risk_engine(R_risk, spec$risk))
    if (is.null(risk_artifact)) {
        return(.empty_result())
    }

    # Ensure horizon covariance exists and matches forecast horizon (default scaled approximation)
    H_fc <- as.integer(spec$forecast$label_horizon %||% 1L)
    if (!is.finite(H_fc) || H_fc < 1L) H_fc <- 1L

    if (is.null(risk_artifact$Sigma_risk_1) && !is.null(risk_artifact$Sigma_total)) {
        risk_artifact$Sigma_risk_1 <- risk_artifact$Sigma_total
    }
    if (is.null(risk_artifact$Sigma_risk_H) && !is.null(risk_artifact$Sigma_risk_1)) {
        risk_artifact$Sigma_risk_H <- H_fc * risk_artifact$Sigma_risk_1
        .w(sprintf("Risk covariance horizon approximation applied: Sigma_risk_H = %d * Sigma_risk_1", H_fc))
    } else if (!is.null(risk_artifact$Sigma_risk_1) && !is.null(risk_artifact$Sigma_risk_H) &&
        H_fc > 1L && identical(risk_artifact$Sigma_risk_H, risk_artifact$Sigma_risk_1)) {
        # If risk engine returned only daily covariance aliased into H slot, rescale here
        risk_artifact$Sigma_risk_H <- H_fc * risk_artifact$Sigma_risk_1
        .w(sprintf("Risk covariance horizon slot rescaled to forecast horizon H=%d", H_fc))
    }

    rd <- risk_artifact$diag %||% list()
    if (!is.null(rd$n_assets_dropped) && rd$n_assets_dropped > 0) {
        .w(sprintf("Risk dropped %d/%d assets", rd$n_assets_dropped, rd$n_assets_input %||% NA_integer_))
    }
    if (isTRUE(rd$was_repaired)) .w("Covariance required nearPD repair")

    if (!isTRUE(spec$risk$resid$use_glasso)) {
        .archv("spec$risk$resid$use_glasso is FALSE (architecture requires Glasso-only residual precision)")
    }
    if (!isTRUE(rd$glasso_used)) {
        .archv(sprintf(
            "Residual precision not produced by Glasso (method=%s; fallback=%s; reason=%s)",
            rd$residual_precision_method %||% "unknown",
            as.character(isTRUE(rd$residual_precision_fallback)),
            rd$residual_precision_fallback_reason %||% "NA"
        ))
    }
    if (isTRUE(rd$allocator_fallback)) {
        .fb("risk", "baseline_allocator_fallback", sprintf(
            "Baseline allocator fallback used (%s)",
            rd$allocator_fallback_reason %||% (rd$allocator_method %||% "unknown")
        ))
    }

    risk_univ <- colnames(risk_artifact$Sigma_risk_H %||% risk_artifact$Sigma_risk_1 %||% risk_artifact$Sigma_total)
    if (is.null(risk_univ)) risk_univ <- names(risk_artifact$w_baseline %||% risk_artifact$w_hrp)

    # ══════════════════════════════════════════════════════════════════════════
    # STAGE 2: Graph and Structure (§§5-6, 9-10)
    # ══════════════════════════════════════════════════════════════════════════

    prev_P_bar <- if (!is.null(model_state)) model_state$prev_P_bar else NULL
    prev_labels <- if (!is.null(model_state)) model_state$prev_labels else NULL

    spec_graph <- spec$graph %||% list()
    graph_artifact <- .run_stage(
        "graph",
        me_run_graph_pipeline(risk_artifact, spec_graph, prev_P_bar, prev_labels)
    )
    if (is.null(graph_artifact)) {
        .fb("graph", "graph_stage_failed", "Graph stage failed; continuing without graph features")
    } else if (isTRUE(graph_artifact$diag$skipped)) {
        .fb("graph", "graph_stage_skipped", graph_artifact$diag$reason %||% "unknown")
    }

    # ══════════════════════════════════════════════════════════════════════════
    # STAGE 3: Signal Engine (§§7-8)
    # ══════════════════════════════════════════════════════════════════════════

    P_sig <- P_kalman[, intersect(colnames(P_kalman), risk_univ), drop = FALSE]
    R_sig <- R_tsmom[, intersect(colnames(R_tsmom), risk_univ), drop = FALSE]

    signal_artifact <- .run_stage(
        "signal",
        me_run_signal_engine(P_sig, R_sig, risk_artifact$sigma_t,
            spec$signals,
            E_window = risk_artifact$E_t,
            B_t = risk_artifact$B_t,
            F_window = risk_artifact$F_t
        )
    )

    if (is.null(signal_artifact)) {
        .fb("signal", "signal_engine_failed", "Signal engine failed; using zero signals")
        signal_artifact <- list(
            kalman = setNames(rep(0, length(risk_univ)), risk_univ),
            tsmom = setNames(rep(0, length(risk_univ)), risk_univ),
            diag = list(kalman_all_zero = TRUE, tsmom_all_zero = TRUE, n_common = length(risk_univ))
        )
    }

    # ══════════════════════════════════════════════════════════════════════════
    # STAGE 4: State and Gating (§11.5, §12.4)
    # ══════════════════════════════════════════════════════════════════════════

    state_gating_artifact <- .run_stage(
        "state_gating",
        me_run_state_and_gating(
            R_disp, R_eta, R_vov, risk_artifact,
            spec$market_state, spec$gating, lkb_vol
        )
    )

    if (is.null(state_gating_artifact)) {
        .fb("state_gating", "state_gating_failed", "State/gating failed; using neutral gating fallback")
        state_gating_artifact <- list(
            market_state = c(disp = 0, eta = 0, VoV = 0),
            gating = list(
                w_kalman = 0.5,
                w_tsmom = 0.5,
                w_cash = 0.0,
                gross_exposure = 1.0,
                softmax_weights = c(kalman = 0.5, tsmom = 0.5, cash = 0.0)
            ),
            diag = list(dispersion = 0, eta = 0, vov = 0)
        )
    }

    # ══════════════════════════════════════════════════════════════════════════
    # STAGE 5: Feature Engine (§11)
    # ══════════════════════════════════════════════════════════════════════════

    feature_artifact <- .run_stage(
        "feature",
        me_run_feature_engine(
            signal_artifact, risk_artifact, graph_artifact,
            state_gating_artifact, adapter, as_of_date,
            risk_univ
        )
    )

    if (is.null(feature_artifact)) {
        .fb("feature", "feature_engine_failed", "Feature engine failed; using empty feature matrix")
        feature_artifact <- list(
            X = matrix(0, length(risk_univ), 0, dimnames = list(risk_univ, NULL)),
            feature_names = character(0),
            feature_groups = list(),
            diag = list(n_features = 0)
        )
    }

    # ══════════════════════════════════════════════════════════════════════════
    # STAGE 6: Forecast Engine (§12)
    # Truthful behavior: do NOT call fake forecast path if no feature history exists
    # ══════════════════════════════════════════════════════════════════════════

    spec_forecast <- spec$forecast %||% list()
    X_hist_window <- NULL

    # Optional future-compatible locations for feature history in model_state
    if (!is.null(model_state) && is.matrix(model_state$X_hist_window)) {
        X_hist_window <- model_state$X_hist_window
    }
    if (!is.null(model_state) && is.list(model_state$feature_history) &&
        is.matrix(model_state$feature_history$X_hist_window)) {
        X_hist_window <- model_state$feature_history$X_hist_window
    }

    forecast_artifact <- NULL
    if (is.null(X_hist_window)) {
        .fb(
            "forecast", "missing_feature_history",
            "Forecast stage skipped: X_hist_window not provided/implemented"
        )
        .archv("Forecast feature history panel (X_hist_window) is not implemented/available")
    } else {
        forecast_artifact <- .run_stage(
            "forecast",
            me_run_forecast_engine(
                X_current = feature_artifact$X,
                R_window = R_risk,
                X_hist_window = X_hist_window,
                gating_artifact = state_gating_artifact,
                risk_artifact = risk_artifact,
                spec_forecast = spec_forecast
            )
        )

        if (is.null(forecast_artifact)) {
            .fb("forecast", "forecast_engine_failed", "Forecast engine failed; portfolio will use signal fallback path")
        } else if (!is.null(forecast_artifact$diag$reason)) {
            # Surface all explicit forecast fallback/degrade reasons
            .fb(
                "forecast", paste0("forecast_diag_", forecast_artifact$diag$reason),
                sprintf("Forecast engine reported reason='%s'", forecast_artifact$diag$reason)
            )
        }
    }

    # Current forecast engine is still off-spec conceptually (time-level training vs asset-level panel)
    .w("[ARCH] Current forecast engine implementation is scaffold/off-spec for asset-level matured training panel")

    # ══════════════════════════════════════════════════════════════════════════
    # STAGE 7: Portfolio Construction (§§13-14)
    # ══════════════════════════════════════════════════════════════════════════

    port_artifact <- .run_stage(
        "portfolio",
        me_build_portfolio_target(
            risk_artifact, signal_artifact, state_gating_artifact,
            spec$portfolio,
            forecast_artifact = forecast_artifact,
            graph_artifact = graph_artifact,
            prev_target = prev_target
        )
    )

    if (is.null(port_artifact)) {
        .fb("portfolio", "portfolio_engine_failed", "Portfolio construction failed; returning empty portfolio")
        return(.empty_result())
    }

    pd <- port_artifact$diag %||% list()
    if (identical(pd$method, "tilt_fallback") || identical(pd$method, "tilt_signal")) {
        .fb(
            "portfolio", paste0("portfolio_", pd$method),
            sprintf("Portfolio used %s method", pd$method)
        )
    }

    # ══════════════════════════════════════════════════════════════════════════
    # Assemble artifact
    # ══════════════════════════════════════════════════════════════════════════

    if (isFALSE(spec$meta$retain_windows)) {
        risk_artifact$E_t <- NULL
        risk_artifact$F_t <- NULL
    }
    if (isFALSE(spec$meta$retain_matrices)) {
        # Keep at least one covariance for validation / portfolio audit
        if (!is.null(risk_artifact$Sigma_risk_H)) {
            risk_artifact$Sigma_total <- NULL
            risk_artifact$Sigma_f <- NULL
            risk_artifact$Sigma_eps <- NULL
            risk_artifact$Theta_eps <- NULL
        } else {
            risk_artifact$Sigma_f <- NULL
            risk_artifact$Sigma_eps <- NULL
            risk_artifact$Theta_eps <- NULL
        }
    }

    architecture_flags <- list(
        glasso_residual_precision_configured = isTRUE(spec$risk$resid$use_glasso),
        glasso_residual_precision_used = isTRUE(risk_artifact$diag$glasso_used),
        horizon_covariance_available = !is.null(risk_artifact$Sigma_risk_H),
        forecast_feature_history_available = !is.null(X_hist_window),
        forecast_stage_executed = !is.null(forecast_artifact),
        forecast_asset_level_training_implemented = FALSE,
        legacy_hrp_alias_present = !is.null(risk_artifact$w_hrp)
    )

    meta <- list(
        spec_hash = me_hash_spec(spec),
        universe_counts = list(
            investable    = length(syms_investable),
            admissible    = length(unique(c(syms_investable, syms_prev_hold))),
            risk_kept     = length(risk_univ),
            risk_dropped  = length(risk_artifact$diag$dropped_assets %||% character(0)),
            signal_common = signal_artifact$diag$n_common %||% 0,
            active_names  = nrow(port_artifact$target_weights)
        ),
        feature_info = list(
            n_features = feature_artifact$diag$n_features %||% 0,
            groups     = feature_artifact$feature_groups %||% list()
        ),
        forecast_info = if (!is.null(forecast_artifact)) forecast_artifact$diag else list(stage = "skipped"),
        graph_info = if (!is.null(graph_artifact)) graph_artifact$diag else list(),
        fallbacks = fallbacks,
        runtime_warnings = runtime_warns,
        architecture_flags = architecture_flags,
        architecture_violations = architecture_violations,
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
            n_features = feature_artifact$diag$n_features %||% 0,
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
        model_state_out = list(
            prev_P_bar = if (!is.null(graph_artifact)) graph_artifact$P_bar else NULL,
            prev_labels = if (!is.null(graph_artifact) &&
                !is.null(graph_artifact$clustering) &&
                !is.null(graph_artifact$clustering$labels)) {
                graph_artifact$clustering$labels
            } else {
                NULL
            },
            prev_target = port_artifact$target_weights
        ),
        warnings = unique(warns)
    )

    me_validate_snapshot_artifact(res)
    res
}
