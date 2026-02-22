# Phase 0 Acceptance Tests
# Run from repository root

source("model_engine/R/00_contracts_and_spec.R")
source("model_engine/R/01_data_adapter.R")
source("model_engine/R/02_risk_engine.R")
source("model_engine/R/03_signal_engine.R")
source("model_engine/R/04_state_and_gating.R")
source("model_engine/R/05_portfolio_constructor.R")
source("model_engine/R/06_snapshot_runner.R")

source("backtest_engine/R/00_contracts_and_spec.R")
source("backtest_engine/R/01_data_and_clock.R")
source("backtest_engine/R/02_execution_and_costs.R")
source("backtest_engine/R/03_accounting.R")
source("backtest_engine/R/04_runner.R")
source("backtest_engine/R/05_analytics.R")

cat("\n=== Testing Model Engine ===\n")

# Dummy panel data
dummy_panel <- data.frame(
    symbol = rep(c("A", "B"), each = 300),
    refdate = as.Date("2020-01-01") + rep(0:299, 2),
    open = 10,
    high = 11,
    low = 9,
    close = runif(600, 9, 11),
    close_adj_final = runif(600, 9, 11),
    turnover = 1000,
    qty = 100,
    asset_type = "equity",
    stringsAsFactors = FALSE
)

spec <- me_get_spec()
as_of <- as.Date("2020-10-01")

snap <- me_run_snapshot(dummy_panel, as_of, spec)
cat("Snapshot generated successfully. Target weights:\n")
print(snap$target_weights)

cat("\n=== Testing No-Lookahead ===\n")
adapter <- me_make_data_adapter(dummy_panel)
pm <- adapter$price_matrix(as_of, lookback = 10)
if (max(as.Date(rownames(pm))) > as_of) stop("Leaked future data!")
cat("No-lookahead check passed.\n")

cat("\n=== Testing Backtest Engine ===\n")

strategy_fn <- function(decision_date, state, data_ctx, strategy_spec) {
    tgt_weights <- data.frame(
        symbol = c("A", "B"),
        weight_target = c(0.5, 0.5),
        stringsAsFactors = FALSE
    )
    list(
        decision_date = decision_date,
        tradable_symbols = c("A", "B"),
        target_weights = tgt_weights,
        cash_weight = 0.0,
        diag = list()
    )
}

bt_spec <- bt_get_spec()

# Run the backtest using dummy panel
bt_res <- bt_run_backtest(dummy_panel, strategy_fn, list(), bt_spec)

cat("Backtest generated successfully. Rebalance steps:", length(bt_res$rebalance_log), "\n")
cat("Final NAV:", tail(bt_res$nav_series$nav, 1), "\n")

cat("\nAll Phase 0 Tests Passed!\n")
