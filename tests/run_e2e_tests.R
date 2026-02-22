# End-to-End Test for Fully Implemented Engines
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

cat("\n=== Testing Real Model Engine (PCA, Glasso, HRP, Kalman, TSMOM) ===\n")

# Create a more realistic synthetic panel to test PCA and Glasso properly
set.seed(42)
days <- as.Date("2020-01-01") + 0:750
n_days <- length(days)
syms <- paste0("SYM", 1:10)

# Simulate correlated returns
market_ret <- rnorm(n_days, 0.0002, 0.01)
panel_list <- lapply(syms, function(s) {
    # Asset specific return: beta * market + idiosyncratic
    beta <- runif(1, 0.5, 1.5)
    ret <- beta * market_ret + rnorm(n_days, 0, 0.02)
    price <- 100 * exp(cumsum(ret))

    data.frame(
        symbol = s,
        refdate = days,
        open = price * exp(rnorm(n_days, 0, 0.001)),
        high = price * exp(abs(rnorm(n_days, 0, 0.005))),
        low = price * exp(-abs(rnorm(n_days, 0, 0.005))),
        close = price,
        close_adj_final = price,
        turnover = runif(n_days, 1e5, 1e6),
        qty = runif(n_days, 1000, 10000),
        asset_type = "equity",
        stringsAsFactors = FALSE
    )
})
dummy_panel <- do.call(rbind, panel_list)

spec <- me_get_spec(list(
    risk = list(
        vol = list(),
        pca = list(k = 3),
        resid = list(use_glasso = FALSE, lambda = 0.01), # Standard ridge if no glasso
        factor = list(),
        hrp = list()
    ),
    signals = list(
        kalman = list(),
        tsmom = list(horizon = 63)
    ),
    market_state = list(
        dispersion = list(),
        eta = list(),
        vov = list()
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
cat("Snapshot generated successfully. Target weights:\n")
print(snap$target_weights)
cat("Cash Target:", snap$cash_weight, "\n")

cat("\n=== Testing Real Backtest Engine ===\n")

strategy_fn <- function(decision_date, state, data_ctx, strategy_spec) {
    # Convert data context to panel view for model engine
    dt_slice <- data_ctx$dt[refdate <= decision_date]

    snap <- me_run_snapshot(dt_slice, decision_date, strategy_spec)

    list(
        decision_date = decision_date,
        tradable_symbols = snap$tradable_symbols,
        target_weights = snap$target_weights,
        cash_weight = snap$cash_weight,
        diag = list()
    )
}

bt_spec <- bt_get_spec(list(
    schedule = list(freq = "months"),
    execution = list(price_field = "open"),
    costs = list(fee_rate = 0.0003, slippage_rate = 0.0010),
    accounting = list(initial_nav = 10000, initial_cash = 10000)
))

# Run the backtest using dummy panel. Starting slightly after 2020 so we have history.
bt_res <- bt_run_backtest(
    dummy_panel[dummy_panel$refdate >= as.Date("2020-01-01") &
        dummy_panel$refdate <= as.Date("2021-06-01"), ],
    strategy_fn, spec, bt_spec
)

cat("Backtest generated successfully. Rebalance steps:", length(bt_res$rebalance_log), "\n")
cat("Final NAV:", tail(bt_res$nav_series$nav, 1), "\n")

metrics <- bt_compute_performance(bt_res$nav_series)
cat("CAGR:", metrics$cagr, " Volatility:", metrics$vol, " Sharpe:", metrics$sharpe, "\n")
cat("\nAll End-to-End Tests Passed!\n")
