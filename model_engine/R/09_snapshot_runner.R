#' @title Model Engine — Snapshot Runner
#' @description Full one-date pipeline orchestrator integrating all modules.
#' architecture.md §16: end-to-end update flow.

.slice_mat <- function(mat, n) {
    if (is.null(mat) || nrow(mat) == 0) {
        return(mat)
    }
    tail(mat, min(n, nrow(mat)))
}

.me_append_feature_snapshot_history <- function(model_state, as_of_date,
                                                feature_artifact, risk_univ,
                                                spec_forecast = list(),
                                                date_idx = NULL) {
    fh <- NULL
    if (!is.null(model_state) && is.list(model_state$feature_history)) {
        fh <- model_state$feature_history
    }
    if (is.null(fh) || !is.list(fh)) fh <- list()

    fh$schema <- "asset_feature_snapshots_v2"
    if (is.null(fh$snapshots) || !is.list(fh$snapshots)) {
        fh$snapshots <- list()
    }

    # Build aligned asset x feature matrix snapshot
    X <- feature_artifact$X
    if (is.null(X)) {
        X <- matrix(0,
            nrow = length(risk_univ), ncol = 0,
            dimnames = list(risk_univ, character(0))
        )
    } else {
        if (!is.matrix(X)) X <- as.matrix(X)
        if (is.null(rownames(X))) {
            stop("feature_artifact$X must have rownames (symbols) for feature history accumulation")
        }
        feat_names <- colnames(X)
        if (is.null(feat_names)) {
            feat_names <- paste0("f", seq_len(ncol(X)))
            colnames(X) <- feat_names
        }

        X_aligned <- matrix(0,
            nrow = length(risk_univ), ncol = ncol(X),
            dimnames = list(risk_univ, colnames(X))
        )
        common_syms <- intersect(risk_univ, rownames(X))
        if (length(common_syms) > 0 && ncol(X) > 0) {
            X_aligned[common_syms, ] <- X[common_syms, , drop = FALSE]
        }
        X <- X_aligned
    }

    # Include date index for training panel builder
    snap_idx <- length(fh$snapshots) + 1L
    fh$snapshots[[snap_idx]] <- list(
        as_of_date = as.Date(as_of_date),
        date = as.character(as_of_date),
        date_idx = date_idx %||% snap_idx,
        X = X
    )

    # De-duplicate by date (keep latest for same date)
    snap_dates <- vapply(
        fh$snapshots,
        function(s) as.character(s$as_of_date %||% NA),
        character(1)
    )
    if (anyDuplicated(snap_dates)) {
        keep_idx <- !duplicated(snap_dates, fromLast = TRUE)
        fh$snapshots <- fh$snapshots[keep_idx]
    }

    # Retention cap
    keep_n <- as.integer(spec_forecast$history_snapshots_keep %||% 252L)
    if (!is.finite(keep_n) || keep_n < 2L) keep_n <- 252L
    if (length(fh$snapshots) > keep_n) {
        fh$snapshots <- tail(fh$snapshots, keep_n)
    }

    fh$n_snapshots <- length(fh$snapshots)
    fh$last_as_of_date <- as.Date(as_of_date)

    fh
}

#' @export
me_run_snapshot <- function(data_bundle_or_panel, as_of_date, spec = NULL,
                            prev_target = NULL, model_state = NULL,
                            aux = list()) {
    spec <- me_get_spec(spec)
    me_validate_spec(spec)
    me_validate_model_state(model_state)

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
            standardized_path_used = FALSE,
            factor_alignment_used = FALSE,
            recursive_factor_cov_used = FALSE,
            recursive_resid_target_used = FALSE,
            forecast_stage_executed = FALSE,
            forecast_method = "none",
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
                prev_P_bar = if (!is.null(model_state)) model_state$prev_P_bar else NULL,
                prev_labels = if (!is.null(model_state)) model_state$prev_labels else NULL,
                prev_target = data.frame(
                    symbol = character(0),
                    weight_target = numeric(0),
                    stringsAsFactors = FALSE
                ),
                feature_history = if (!is.null(model_state) && is.list(model_state$feature_history)) {
                    model_state$feature_history
                } else {
                    NULL
                },
                # Carry forward all recursive states on empty
                ewma_vol_state = if (!is.null(model_state)) model_state$ewma_vol_state else NULL,
                factor_cov_state = if (!is.null(model_state)) model_state$factor_cov_state else NULL,
                resid_cov_state = if (!is.null(model_state)) model_state$resid_cov_state else NULL,
                B_prev = if (!is.null(model_state)) model_state$B_prev else NULL,
                edge_stability = if (!is.null(model_state)) model_state$edge_stability else NULL,
                node_stability = if (!is.null(model_state)) model_state$node_stability else NULL,
                prev_M = if (!is.null(model_state)) model_state$prev_M else NULL,
                kalman_states = if (!is.null(model_state)) model_state$kalman_states else NULL,
                scalar_weights = if (!is.null(model_state)) model_state$scalar_weights else NULL,
                component_model_fits = if (!is.null(model_state)) model_state$component_model_fits else NULL,
                component_error_states = if (!is.null(model_state)) model_state$component_error_states else NULL,
                forecast_step = if (!is.null(model_state)) model_state$forecast_step else NULL
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
    lkb_tsmom <- max(spec$signals$tsmom$horizons %||% 252L)
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

    # Volume matrix (if available from adapter)
    vol_window <- .run_stage("volume_matrix", tryCatch(
        adapter$price_matrix(as_of_date, max_lkb + 1, "volume", syms, strict = FALSE),
        error = function(e) NULL
    ))

    # ══════════════════════════════════════════════════════════════════════════
    # STAGE 1: Risk Engine (§5) — with recursive states
    # ══════════════════════════════════════════════════════════════════════════

    risk_artifact <- .run_stage(
        "risk",
        me_run_risk_engine(R_risk, spec$risk, model_state = model_state)
    )
    if (is.null(risk_artifact)) {
        return(.empty_result())
    }

    # Ensure horizon covariance matches forecast horizon
    H_fc <- as.integer(spec$forecast$label_horizon %||% 1L)
    if (!is.finite(H_fc) || H_fc < 1L) H_fc <- 1L

    if (is.null(risk_artifact$Sigma_risk_1) && !is.null(risk_artifact$Sigma_total)) {
        risk_artifact$Sigma_risk_1 <- risk_artifact$Sigma_total
    }
    if (is.null(risk_artifact$Sigma_risk_H) && !is.null(risk_artifact$Sigma_risk_1)) {
        risk_artifact$Sigma_risk_H <- H_fc * risk_artifact$Sigma_risk_1
    } else if (!is.null(risk_artifact$Sigma_risk_1) && !is.null(risk_artifact$Sigma_risk_H) &&
        H_fc > 1L && identical(risk_artifact$Sigma_risk_H, risk_artifact$Sigma_risk_1)) {
        risk_artifact$Sigma_risk_H <- H_fc * risk_artifact$Sigma_risk_1
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
    # STAGE 2: Graph and Structure (§§5-6, 9-10) — with recursive states
    # ══════════════════════════════════════════════════════════════════════════

    prev_P_bar <- if (!is.null(model_state)) model_state$prev_P_bar else NULL
    prev_labels <- if (!is.null(model_state)) model_state$prev_labels else NULL

    spec_graph <- spec$graph %||% list()
    graph_artifact <- .run_stage(
        "graph",
        me_run_graph_pipeline(risk_artifact, spec_graph, prev_P_bar, prev_labels,
            model_state = model_state
        )
    )
    if (is.null(graph_artifact)) {
        .fb("graph", "graph_stage_failed", "Graph stage failed; continuing without graph features")
    } else if (isTRUE(graph_artifact$diag$skipped)) {
        .fb("graph", "graph_stage_skipped", graph_artifact$diag$reason %||% "unknown")
    }

    # ══════════════════════════════════════════════════════════════════════════
    # STAGE 3: Signal Engine (§§7-8) — with recursive Kalman + scalarization
    # ══════════════════════════════════════════════════════════════════════════

    P_sig <- P_kalman[, intersect(colnames(P_kalman), risk_univ), drop = FALSE]
    R_sig <- R_tsmom[, intersect(colnames(R_tsmom), risk_univ), drop = FALSE]

    signal_artifact <- .run_stage(
        "signal",
        me_run_signal_engine(P_sig, R_sig, risk_artifact,
            spec$signals,
            model_state = model_state
        )
    )

    if (is.null(signal_artifact)) {
        .fb("signal", "signal_engine_failed", "Signal engine failed; using zero signals")
        signal_artifact <- list(
            s_mom = setNames(rep(0, length(risk_univ)), risk_univ),
            s_kal = setNames(rep(0, length(risk_univ)), risk_univ),
            s_fac = setNames(rep(0, length(risk_univ)), risk_univ),
            s_scalar = setNames(rep(0, length(risk_univ)), risk_univ),
            kalman_slope = setNames(rep(0, length(risk_univ)), risk_univ),
            kalman_uncertainty = setNames(rep(0, length(risk_univ)), risk_univ),
            kalman_innovation = setNames(rep(0, length(risk_univ)), risk_univ),
            kalman_innov_ratio = setNames(rep(0, length(risk_univ)), risk_univ),
            diag = list(n_common = length(risk_univ)),
            signal_state_out = list()
        )
    }

    # ══════════════════════════════════════════════════════════════════════════
    # STAGE 4: State and Gating (§11.5, §12.4) — extended m_t + 5-comp gating
    # ══════════════════════════════════════════════════════════════════════════

    # Build extended m_t (pass full return window — each sub-state uses own lookback)
    graph_diag_for_state <- if (!is.null(graph_artifact)) graph_artifact$diag else NULL
    market_state_vec <- .run_stage(
        "market_state",
        me_build_market_state_vector(ret_max, spec$market_state,
            graph_diag = graph_diag_for_state
        )
    )
    if (is.null(market_state_vec)) {
        market_state_vec <- c(
            disp = 0, eta = 0, VoV = 0, dens = 0, eto = 0, chi = 0,
            liq_avg = 0, liq_disp = 0, liq_trend = 0
        )
    }

    # Legacy 3-way gating
    legacy_gating <- .run_stage(
        "legacy_gating",
        me_softmax_gating(market_state_vec, spec$gating)
    )
    if (is.null(legacy_gating)) {
        legacy_gating <- list(w_kalman = 0.5, w_tsmom = 0.5, w_cash = 0, gross_exposure = 1.0)
    }

    # Architecture 5-component gating
    arch_gating <- .run_stage(
        "arch_gating",
        me_softmax_gating_5c(market_state_vec, spec$gating)
    )

    # Optimizer controls
    opt_controls <- .run_stage(
        "optimizer_controls",
        me_optimizer_controls(market_state_vec, spec$portfolio)
    )

    state_gating_artifact <- list(
        market_state = market_state_vec,
        gating = legacy_gating,
        arch_gating = arch_gating,
        optimizer_controls = opt_controls,
        diag = list(
            dispersion = market_state_vec["disp"],
            eta = market_state_vec["eta"],
            vov = market_state_vec["VoV"]
        )
    )

    # ══════════════════════════════════════════════════════════════════════════
    # STAGE 5: Feature Engine (§11) — architecture-aligned
    # ══════════════════════════════════════════════════════════════════════════

    # Prepare volume window for liquidity features
    vol_feat_window <- NULL
    if (!is.null(vol_window) && ncol(vol_window) > 0) {
        vol_feat_window <- vol_window[, intersect(colnames(vol_window), risk_univ), drop = FALSE]
    }

    P_feat <- prices_max[, intersect(colnames(prices_max), risk_univ), drop = FALSE]

    feature_artifact <- .run_stage(
        "feature",
        me_assemble_features(
            signal_artifact, risk_artifact, graph_artifact,
            market_state_vec, P_feat,
            volume_window = vol_feat_window
        )
    )

    if (is.null(feature_artifact)) {
        .fb("feature", "feature_engine_failed", "Feature engine failed; using empty feature matrix")
        feature_artifact <- list(
            X = data.frame(matrix(0, length(risk_univ), 0, dimnames = list(risk_univ, NULL))),
            groups = list(),
            n_features = 0
        )
    }

    # ══════════════════════════════════════════════════════════════════════════
    # STAGE 6: Forecast Engine (§12) — 5-component panel architecture
    # ══════════════════════════════════════════════════════════════════════════

    spec_forecast <- spec$forecast %||% list()

    # Persist feature snapshots
    feature_history_out <- .run_stage(
        "feature_history",
        .me_append_feature_snapshot_history(
            model_state = model_state,
            as_of_date = as_of_date,
            feature_artifact = feature_artifact,
            risk_univ = risk_univ,
            spec_forecast = spec_forecast
        )
    )

    if (is.null(feature_history_out)) {
        .fb(
            "feature_history", "feature_history_append_failed",
            "Failed to append feature snapshot history"
        )
        if (!is.null(model_state) && is.list(model_state$feature_history)) {
            feature_history_out <- model_state$feature_history
        }
    }

    # Build feature history list for panel builder
    fh_snapshots <- if (!is.null(feature_history_out) &&
        is.list(feature_history_out$snapshots)) {
        feature_history_out$snapshots
    } else {
        NULL
    }

    # Build R_history for panel labels
    R_history <- ret_max # Full available return history

    forecast_artifact <- .run_stage(
        "forecast",
        me_run_forecast_engine(
            feature_artifact = feature_artifact,
            risk_artifact = risk_artifact,
            graph_artifact = graph_artifact,
            signal_artifact = signal_artifact,
            market_state_vec = market_state_vec,
            spec_forecast = spec_forecast,
            spec_gating = spec$gating,
            feature_history = fh_snapshots,
            R_history = R_history,
            model_state = model_state
        )
    )

    if (is.null(forecast_artifact)) {
        .fb("forecast", "forecast_engine_failed", "Forecast engine failed; portfolio will use signal fallback path")
    } else {
        fc_diag <- forecast_artifact$diag %||% list()
        if (grepl("FALLBACK", fc_diag$method %||% "", fixed = TRUE)) {
            .fb(
                "forecast", paste0("forecast_", fc_diag$method),
                sprintf("Forecast: %s (reason: %s)", fc_diag$method, fc_diag$reason %||% "")
            )
        }
    }

    # ══════════════════════════════════════════════════════════════════════════
    # STAGE 7: Portfolio Construction (§§13-14) — consume μ_eff
    # ══════════════════════════════════════════════════════════════════════════

    # Liquidity features for caps (if available)
    liq_features_for_port <- NULL
    if (!is.null(feature_artifact$X) && "f_liquidity" %in% colnames(feature_artifact$X)) {
        liq_features_for_port <- list(
            f_liquidity = feature_artifact$X[, "f_liquidity"]
        )
    }

    port_artifact <- .run_stage(
        "portfolio",
        me_build_portfolio_target(
            risk_artifact, signal_artifact, state_gating_artifact,
            spec$portfolio,
            forecast_artifact = forecast_artifact,
            graph_artifact = graph_artifact,
            prev_target = prev_target,
            optimizer_controls = opt_controls,
            liquidity_features = liq_features_for_port
        )
    )

    if (is.null(port_artifact)) {
        .fb("portfolio", "portfolio_engine_failed", "Portfolio construction failed; returning empty portfolio")
        return(.empty_result())
    }

    pd <- port_artifact$diag %||% list()
    if (grepl("FALLBACK", pd$method %||% "", fixed = TRUE)) {
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
        glasso_residual_precision_used = isTRUE(rd$glasso_used),
        standardized_path_used = isTRUE(rd$standardized_path_used),
        factor_alignment_used = isTRUE(rd$factor_alignment_used),
        recursive_factor_cov_used = isTRUE(rd$recursive_factor_cov_used),
        recursive_resid_target_used = isTRUE(rd$recursive_resid_target_used),
        horizon_covariance_available = !is.null(risk_artifact$Sigma_risk_H),
        forecast_stage_executed = !is.null(forecast_artifact),
        forecast_method = if (!is.null(forecast_artifact)) {
            forecast_artifact$diag$method %||% "unknown"
        } else {
            "skipped"
        },
        feature_snapshot_history_count = if (!is.null(feature_history_out) &&
            is.list(feature_history_out$snapshots)) {
            length(feature_history_out$snapshots)
        } else {
            0L
        },
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
            n_features = feature_artifact$n_features %||% ncol(feature_artifact$X %||% matrix(0, 0, 0)),
            groups = feature_artifact$groups %||% list()
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

    # ── Collect all recursive state updates ──
    risk_state <- risk_artifact$risk_state_out %||% list()
    graph_state <- if (!is.null(graph_artifact)) graph_artifact$graph_state_out %||% list() else list()
    signal_state <- signal_artifact$signal_state_out %||% list()
    forecast_state <- if (!is.null(forecast_artifact)) forecast_artifact$forecast_state_out %||% list() else list()

    res <- list(
        as_of_date = as_of_date,
        tradable_symbols = risk_univ,
        target_weights = port_artifact$target_weights,
        cash_weight = port_artifact$cash_weight,
        risk = risk_artifact,
        signals = signal_artifact,
        market_state = state_gating_artifact$market_state,
        gating = state_gating_artifact$gating,
        arch_gating = state_gating_artifact$arch_gating,
        graph = if (!is.null(graph_artifact)) {
            list(
                clustering = graph_artifact$clustering,
                diag = graph_artifact$diag
            )
        } else {
            list()
        },
        features = list(
            n_features = feature_artifact$n_features %||% 0,
            groups     = feature_artifact$groups %||% list()
        ),
        forecast = if (!is.null(forecast_artifact)) {
            list(
                mu_hat = forecast_artifact$mu_hat,
                mu_eff = forecast_artifact$mu_eff,
                sigma_hat = forecast_artifact$sigma_hat,
                s_eff = forecast_artifact$s_eff,
                confidence = forecast_artifact$confidence,
                pi_t = forecast_artifact$pi_t,
                rho_rel = forecast_artifact$rho_rel,
                method = forecast_artifact$diag$method
            )
        } else {
            list()
        },
        portfolio_diag = pd,
        meta = meta,
        model_state_out = list(
            # Graph states
            prev_P_bar = if (!is.null(graph_artifact)) {
                graph_artifact$P_bar
            } else {
                (if (!is.null(model_state)) model_state$prev_P_bar else NULL)
            },
            prev_labels = if (!is.null(graph_artifact) &&
                !is.null(graph_artifact$clustering) &&
                !is.null(graph_artifact$clustering$labels)) {
                graph_artifact$clustering$labels
            } else {
                if (!is.null(model_state)) model_state$prev_labels else NULL
            },
            # Portfolio state
            prev_target = port_artifact$target_weights,
            # Feature history
            feature_history = feature_history_out,
            # Risk recursive states
            ewma_vol_state = risk_state$ewma_vol_state %||%
                (if (!is.null(model_state)) model_state$ewma_vol_state else NULL),
            factor_cov_state = risk_state$factor_cov_state %||%
                (if (!is.null(model_state)) model_state$factor_cov_state else NULL),
            resid_cov_state = risk_state$resid_cov_state %||%
                (if (!is.null(model_state)) model_state$resid_cov_state else NULL),
            B_prev = risk_state$B_prev %||%
                (if (!is.null(model_state)) model_state$B_prev else NULL),
            # Graph recursive states
            edge_stability = graph_state$edge_stability %||%
                (if (!is.null(model_state)) model_state$edge_stability else NULL),
            node_stability = graph_state$node_stability %||%
                (if (!is.null(model_state)) model_state$node_stability else NULL),
            prev_M = graph_state$prev_M %||%
                (if (!is.null(model_state)) model_state$prev_M else NULL),
            chi_t = graph_state$chi_t %||% 0,
            # Signal recursive states
            kalman_states = signal_state$kalman_states %||%
                (if (!is.null(model_state)) model_state$kalman_states else NULL),
            scalar_weights = signal_state$scalar_weights %||%
                (if (!is.null(model_state)) model_state$scalar_weights else NULL),
            # Forecast recursive states
            component_model_fits = forecast_state$component_model_fits %||%
                (if (!is.null(model_state)) model_state$component_model_fits else NULL),
            component_error_states = forecast_state$component_error_states %||%
                (if (!is.null(model_state)) model_state$component_error_states else NULL),
            forecast_step = forecast_state$forecast_step %||%
                (if (!is.null(model_state)) model_state$forecast_step else 0L)
        ),
        warnings = unique(warns)
    )

    me_validate_snapshot_artifact(res)
    res
}
