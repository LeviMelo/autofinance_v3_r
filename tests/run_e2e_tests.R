# ═══════════════════════════════════════════════════════════════════════════════
# End-to-End Tests — Full Architecture
# Model Engine (10 files) + Backtest Engine (7 files)
# ═══════════════════════════════════════════════════════════════════════════════

# ── Source model engine (full architecture) ──
source("model_engine/R/00_contracts_and_spec.R")
source("model_engine/R/01_data_adapter.R")
source("model_engine/R/02_risk_engine.R")
source("model_engine/R/03_graph_and_structure.R")
source("model_engine/R/04_signal_engine.R")
source("model_engine/R/05_feature_engine.R")
source("model_engine/R/06_state_and_gating.R")
source("model_engine/R/07_forecast_engine.R")
source("model_engine/R/08_portfolio_engine.R")
source("model_engine/R/09_snapshot_runner.R")

# ── Source backtest engine ──
source("backtest_engine/R/00_contracts_and_spec.R")
source("backtest_engine/R/01_data_context_and_clock.R")
source("backtest_engine/R/02_strategy_adapter_and_bridge.R")
source("backtest_engine/R/03_execution_and_costs.R")
source("backtest_engine/R/04_accounting.R")
source("backtest_engine/R/05_runner.R")
source("backtest_engine/R/06_analytics.R")

# ═══════════════════════════════════════════════════════════════════════════════
# Synthetic panel (10 correlated assets, 2+ years)
# ═══════════════════════════════════════════════════════════════════════════════

set.seed(42)
days <- as.Date("2020-01-01") + 0:750
n_days <- length(days)
syms <- paste0("SYM", 1:10)

market_ret <- rnorm(n_days, 0.0002, 0.01)
panel_list <- lapply(syms, function(s) {
    beta <- runif(1, 0.5, 1.5)
    ret <- beta * market_ret + rnorm(n_days, 0, 0.02)
    price <- 100 * exp(cumsum(ret))
    data.frame(
        symbol = s, refdate = days,
        open = price * exp(rnorm(n_days, 0, 0.001)),
        close = price,
        turnover = runif(n_days, 1e5, 1e6),
        qty = runif(n_days, 1000, 10000),
        asset_type = "equity",
        stringsAsFactors = FALSE
    )
})
dummy_panel <- do.call(rbind, panel_list)

# ═══════════════════════════════════════════════════════════════════════════════
# Test 1: Full Model Snapshot (graph + features + forecast + QP)
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n══════════════════════════════════════════════════════\n")
cat("Test 1: Full Architecture Model Snapshot\n")
cat("══════════════════════════════════════════════════════\n")

spec <- me_get_spec(list(
    risk = list(
        vol = list(lookback = 252L),
        pca = list(k = 3, lookback = 252L),
        resid = list(use_glasso = FALSE, lambda = 0.01),
        hrp = list()
    ),
    signals = list(
        kalman = list(lookback = 252L),
        tsmom  = list(horizon = 63)
    ),
    market_state = list(
        dispersion = list(lookback = 63L),
        eta        = list(lookback = 126L),
        vov        = list(lookback = 63L, vol_lookback = 21L)
    ),
    gating = list(w0 = c(kalman = 0, tsmom = 0, cash = -1)),
    portfolio = list(
        tilt = list(max_tilt = 2), caps = list(max_weight = 0.20),
        gamma = 1.0, alpha_scale = 1.0
    ),
    graph = list(activation_thr = 0.05, top_k = 5L, K_min = 2L, K_max = 5L),
    meta = list(retain_windows = TRUE, retain_matrices = TRUE)
))

as_of <- as.Date("2021-01-01")
snap <- me_run_snapshot(dummy_panel, as_of, spec)

stopifnot(!is.null(snap))
stopifnot(is.data.frame(snap$target_weights))
stopifnot(nrow(snap$target_weights) > 0)
stopifnot(abs(sum(snap$target_weights$weight_target) + snap$cash_weight - 1.0) < 1e-3)
stopifnot(all(snap$target_weights$weight_target >= 0))

cat(sprintf("  Risk universe: %d assets\n", length(snap$tradable_symbols)))
cat(sprintf(
    "  Gating: kalman=%.3f tsmom=%.3f cash=%.3f\n",
    snap$gating$w_kalman, snap$gating$w_tsmom, snap$gating$w_cash
))
cat(sprintf("  Features: %d\n", snap$meta$feature_info$n_features))
cat(sprintf("  Graph edges: %s\n", snap$meta$graph_info$n_edges %||% "NA"))
cat(sprintf("  Clusters: %s\n", snap$meta$graph_info$K_clusters %||% "NA"))
cat(sprintf("  Portfolio method: %s\n", snap$portfolio_diag$method))
cat(sprintf(
    "  Forecast confidence: %s\n",
    round(snap$forecast$confidence %||% 0, 3)
))
cat(sprintf(
    "  Weights sum: %.4f (risky) + %.4f (cash) = %.4f\n",
    sum(snap$target_weights$weight_target), snap$cash_weight,
    sum(snap$target_weights$weight_target) + snap$cash_weight
))
cat("  Targets:\n")
print(snap$target_weights)
if (length(snap$warnings) > 0) {
    cat("  Warnings:", paste(snap$warnings, collapse = "; "), "\n")
}
cat("  ✓ Full snapshot PASSED\n")

# ═══════════════════════════════════════════════════════════════════════════════
# Test 2: Graph pipeline standalone
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n══════════════════════════════════════════════════════\n")
cat("Test 2: Graph & Clustering Pipeline\n")
cat("══════════════════════════════════════════════════════\n")

adapter <- me_make_data_adapter(dummy_panel)
R_test <- adapter$returns_matrix(as_of, 252)
risk_test <- me_run_risk_engine(R_test, spec$risk)

# Need Glasso for precision matrix
spec_gl <- spec$risk
spec_gl$resid$use_glasso <- TRUE
spec_gl$resid$lambda <- 0.1
risk_gl <- me_run_risk_engine(R_test, spec_gl)

graph <- me_run_graph_pipeline(risk_gl, spec$graph)
cat(sprintf("  Edges: %d | Density: %.3f\n", graph$diag$n_edges, graph$diag$density))
cat(sprintf("  Clusters: %d (%s)\n", graph$diag$K_clusters, graph$diag$cluster_method))
cat(sprintf("  Labels: %s\n", paste(graph$clustering$labels, collapse = ",")))

# Test graph operators
ops <- graph$operators
s_test <- setNames(rnorm(ncol(ops$A)), colnames(ops$A))
peer <- me_graph_peer(ops$A, s_test)
rel <- me_graph_relative(ops$A, s_test)
ten <- me_graph_tension(ops$L, s_test)
shr <- me_graph_shrinkage(ops$L_norm, s_test, 0.1)
mix <- me_graph_mixed(ops$A, s_test, 0.5)

stopifnot(length(peer) == length(s_test))
stopifnot(length(rel) == length(s_test))
cat("  ✓ Graph pipeline PASSED\n")

# ═══════════════════════════════════════════════════════════════════════════════
# Test 3: Feature engine standalone
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n══════════════════════════════════════════════════════\n")
cat("Test 3: Feature Engine\n")
cat("══════════════════════════════════════════════════════\n")

P_test <- adapter$price_matrix(as_of, 252, "close")
sig <- me_run_signal_engine(P_test, R_test, risk_test$sigma_t,
    spec$signals,
    E_window = risk_test$E_t,
    B_t = risk_test$B_t, F_window = risk_test$F_t
)
scalars <- me_scalarize_signals(sig)
cat(sprintf(
    "  Signal scalars: s_mom(%d) s_kal(%d) s_fac(%s)\n",
    length(scalars$s_mom), length(scalars$s_kal),
    if (!is.null(scalars$s_fac)) length(scalars$s_fac) else "NULL"
))

sg <- me_run_state_and_gating(
    .slice_mat(R_test, 63), .slice_mat(R_test, 126), .slice_mat(R_test, 84),
    risk_test, spec$market_state, spec$gating, 21L
)

feat <- me_run_feature_engine(
    sig, risk_test, graph, sg,
    adapter, as_of, names(risk_test$w_hrp)
)
cat(sprintf("  Feature matrix: %dx%d\n", nrow(feat$X), ncol(feat$X)))
cat(sprintf(
    "  Groups: %s\n",
    paste(names(feat$feature_groups), collapse = ", ")
))
for (g in names(feat$feature_groups)) {
    cat(sprintf("    %s: %s\n", g, paste(feat$feature_groups[[g]], collapse = ", ")))
}
stopifnot(nrow(feat$X) > 0)
stopifnot(ncol(feat$X) > 0)
cat("  ✓ Feature engine PASSED\n")

# ═══════════════════════════════════════════════════════════════════════════════
# Test 4: Equal-Weight Backtest
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n══════════════════════════════════════════════════════\n")
cat("Test 4: Equal-Weight Backtest\n")
cat("══════════════════════════════════════════════════════\n")

bt_spec_ew <- bt_get_spec(list(
    clock = list(freq = "months"),
    execution = list(price_field = "open", exec_lag = 1L),
    costs = list(fee_rate = 0.0003, slippage_rate = 0.0010),
    accounting = list(initial_nav = 100000, initial_cash = 100000),
    runner = list(mode = "robust", verbose = FALSE)
))

bt_ew <- bt_run_backtest(
    dummy_panel[dummy_panel$refdate >= as.Date("2020-06-01") &
        dummy_panel$refdate <= as.Date("2021-06-01"), ],
    bt_strategy_equal_weight, list(), bt_spec_ew
)

stopifnot(nrow(bt_ew$nav_series) > 0)
analytics_ew <- bt_compute_analytics(bt_ew)
bt_print_summary(analytics_ew)
cat("  ✓ Equal-weight backtest PASSED\n")

# ═══════════════════════════════════════════════════════════════════════════════
# Test 5: Model-Engine Strategy Backtest (full pipeline)
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n══════════════════════════════════════════════════════\n")
cat("Test 5: Model-Engine Strategy Backtest (Full Pipeline)\n")
cat("══════════════════════════════════════════════════════\n")

bt_spec_me <- bt_get_spec(list(
    clock = list(freq = "months"),
    execution = list(price_field = "open", exec_lag = 1L),
    costs = list(fee_rate = 0.0003, slippage_rate = 0.0010),
    accounting = list(initial_nav = 100000, initial_cash = 100000),
    runner = list(mode = "robust", verbose = FALSE)
))

bt_me <- bt_run_backtest(
    dummy_panel[dummy_panel$refdate >= as.Date("2020-06-01") &
        dummy_panel$refdate <= as.Date("2021-06-01"), ],
    bt_strategy_from_model_engine, spec, bt_spec_me
)

stopifnot(nrow(bt_me$nav_series) > 0)
analytics_me <- bt_compute_analytics(bt_me)
bt_print_summary(analytics_me)
cat("  ✓ Model-engine backtest PASSED\n")

# ═══════════════════════════════════════════════════════════════════════════════
# Test 6: Contract Validation
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n══════════════════════════════════════════════════════\n")
cat("Test 6: Contract Validation\n")
cat("══════════════════════════════════════════════════════\n")

me_validate_spec(spec)
cat("  ✓ ModelSpec valid\n")
bt_validate_spec(bt_spec_me)
cat("  ✓ BacktestSpec valid\n")
me_validate_snapshot_artifact(snap)
cat("  ✓ Snapshot artifact valid\n")
bt_validate_run_artifact(bt_me)
cat("  ✓ Run artifact valid\n")

stopifnot(tryCatch(
    {
        me_validate_spec(list(data = list()))
        FALSE
    },
    error = function(e) TRUE
))
cat("  ✓ Invalid ModelSpec rejected\n")
stopifnot(tryCatch(
    {
        bt_validate_spec(list())
        FALSE
    },
    error = function(e) TRUE
))
cat("  ✓ Invalid BacktestSpec rejected\n")

# ═══════════════════════════════════════════════════════════════════════════════
cat("\n══════════════════════════════════════════════════════\n")
cat("ALL E2E TESTS PASSED ✓ (Full Architecture)\n")
cat("══════════════════════════════════════════════════════\n")
