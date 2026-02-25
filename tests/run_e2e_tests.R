# ═══════════════════════════════════════════════════════════════════════════════
# End-to-End Tests — Model Engine + Backtest Engine
# ═══════════════════════════════════════════════════════════════════════════════

# ── Source model engine ──
source("model_engine/R/00_contracts_and_spec.R")
source("model_engine/R/01_data_adapter.R")
source("model_engine/R/02_risk_engine.R")
source("model_engine/R/03_signal_engine.R")
source("model_engine/R/04_state_and_gating.R")
source("model_engine/R/05_portfolio_constructor.R")
source("model_engine/R/06_snapshot_runner.R")

# ── Source backtest engine ──
source("backtest_engine/R/00_contracts_and_spec.R")
source("backtest_engine/R/01_data_context_and_clock.R")
source("backtest_engine/R/02_strategy_adapter_and_bridge.R")
source("backtest_engine/R/03_execution_and_costs.R")
source("backtest_engine/R/04_accounting.R")
source("backtest_engine/R/05_runner.R")
source("backtest_engine/R/06_analytics.R")

# ═══════════════════════════════════════════════════════════════════════════════
# Test 1: Model Snapshot
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n══════════════════════════════════════════════════════\n")
cat("Test 1: Model Engine Snapshot\n")
cat("══════════════════════════════════════════════════════\n")

set.seed(42)
days <- as.Date("2020-01-01") + 0:750
n_days <- length(days)
syms <- paste0("SYM", 1:10)

# Simulate correlated returns via a common factor
market_ret <- rnorm(n_days, 0.0002, 0.01)
panel_list <- lapply(syms, function(s) {
    beta <- runif(1, 0.5, 1.5)
    ret <- beta * market_ret + rnorm(n_days, 0, 0.02)
    price <- 100 * exp(cumsum(ret))
    data.frame(
        symbol = s,
        refdate = days,
        open = price * exp(rnorm(n_days, 0, 0.001)),
        close = price,
        turnover = runif(n_days, 1e5, 1e6),
        qty = runif(n_days, 1000, 10000),
        asset_type = "equity",
        stringsAsFactors = FALSE
    )
})
dummy_panel <- do.call(rbind, panel_list)

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
    gating = list(
        w0 = c(kalman = 0, tsmom = 0, cash = -1)
    ),
    portfolio = list(
        tilt = list(max_tilt = 2),
        caps = list(max_weight = 0.20)
    )
))

as_of <- as.Date("2021-01-01")

snap <- me_run_snapshot(dummy_panel, as_of, spec)

stopifnot(!is.null(snap))
stopifnot(is.data.frame(snap$target_weights))
stopifnot(nrow(snap$target_weights) > 0)
stopifnot(abs(sum(snap$target_weights$weight_target) + snap$cash_weight - 1.0) < 1e-3)
stopifnot(all(snap$target_weights$weight_target >= 0))
stopifnot(all(snap$target_weights$weight_target <= 0.20 + 1e-4))

cat("  Target weights:\n")
print(snap$target_weights)
cat(sprintf("  Cash: %.4f\n", snap$cash_weight))
cat(sprintf("  Risk universe: %d assets\n", length(snap$tradable_symbols)))
cat(sprintf(
    "  Gating: kalman=%.3f tsmom=%.3f cash=%.3f\n",
    snap$gating$w_kalman, snap$gating$w_tsmom, snap$gating$w_cash
))
cat("  ✓ Snapshot test PASSED\n")

# ═══════════════════════════════════════════════════════════════════════════════
# Test 2: Equal-Weight Backtest
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n══════════════════════════════════════════════════════\n")
cat("Test 2: Equal-Weight Backtest\n")
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
    bt_strategy_equal_weight,
    strategy_spec = list(),
    bt_spec = bt_spec_ew
)

stopifnot(nrow(bt_ew$nav_series) > 0)
stopifnot(length(bt_ew$rebalance_log) > 0)
stopifnot(all(bt_ew$nav_series$nav > 0))

analytics_ew <- bt_compute_analytics(bt_ew)
cat(sprintf("  Rebalances: %d\n", analytics_ew$n_rebalances))
cat(sprintf("  Final NAV: %.0f\n", tail(bt_ew$nav_series$nav, 1)))
bt_print_summary(analytics_ew)
cat("  ✓ Equal-weight backtest PASSED\n")

# ═══════════════════════════════════════════════════════════════════════════════
# Test 3: Model-Engine Strategy Backtest
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n══════════════════════════════════════════════════════\n")
cat("Test 3: Model-Engine Strategy Backtest\n")
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
    bt_strategy_from_model_engine,
    strategy_spec = spec,
    bt_spec = bt_spec_me
)

stopifnot(nrow(bt_me$nav_series) > 0)
stopifnot(length(bt_me$rebalance_log) > 0)
stopifnot(all(bt_me$nav_series$nav > 0))

analytics_me <- bt_compute_analytics(bt_me)
cat(sprintf("  Rebalances: %d\n", analytics_me$n_rebalances))
cat(sprintf("  Final NAV: %.0f\n", tail(bt_me$nav_series$nav, 1)))
bt_print_summary(analytics_me)
cat("  ✓ Model-engine backtest PASSED\n")

# ═══════════════════════════════════════════════════════════════════════════════
# Test 4: Contract Validation
# ═══════════════════════════════════════════════════════════════════════════════
cat("\n══════════════════════════════════════════════════════\n")
cat("Test 4: Contract Validation\n")
cat("══════════════════════════════════════════════════════\n")

# Spec validation
me_validate_spec(spec)
cat("  ✓ ModelSpec validation passed\n")

bt_validate_spec(bt_spec_me)
cat("  ✓ BacktestSpec validation passed\n")

# Invalid spec should fail
ok <- tryCatch(
    {
        me_validate_spec(list(data = list()))
        FALSE
    },
    error = function(e) TRUE
)
stopifnot(ok)
cat("  ✓ Invalid ModelSpec correctly rejected\n")

ok <- tryCatch(
    {
        bt_validate_spec(list())
        FALSE
    },
    error = function(e) TRUE
)
stopifnot(ok)
cat("  ✓ Invalid BacktestSpec correctly rejected\n")

# Snapshot artifact validation
me_validate_snapshot_artifact(snap)
cat("  ✓ Snapshot artifact validation passed\n")

# Run artifact validation
bt_validate_run_artifact(bt_me)
cat("  ✓ Backtest run artifact validation passed\n")

# ═══════════════════════════════════════════════════════════════════════════════
cat("\n══════════════════════════════════════════════════════\n")
cat("ALL E2E TESTS PASSED ✓\n")
cat("══════════════════════════════════════════════════════\n")
