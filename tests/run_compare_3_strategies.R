# run_compare_3_strategies.R
# Standalone: loads modules, builds/loads data, runs 3 strategies, prints summaries, saves plot + CSV.

options(stringsAsFactors = FALSE)

# ──────────────────────────────────────────────────────────────────────────────
# 0) Paths (assumes you run from project root)
# ──────────────────────────────────────────────────────────────────────────────
DATA_ENGINE_DIR <- file.path("data_engine", "R")
CAME_DIR <- file.path("model_engine_came", "R")
BT_DIR <- file.path("backtest_engine", "R")

# Output files
OUT_DIR <- "bt_outputs"
if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)
OUT_PNG <- file.path(OUT_DIR, "nav_comparison.png")
OUT_CSV <- file.path(OUT_DIR, "nav_comparison.csv")

# ──────────────────────────────────────────────────────────────────────────────
# 1) Load modules
# ──────────────────────────────────────────────────────────────────────────────

# ---- data_engine (optional, only needed if BUILD_DATA=TRUE) ----
BUILD_DATA <- TRUE # set FALSE if you already have a panel saved to PANEL_RDS
PANEL_RDS <- file.path(OUT_DIR, "panel_adj_model.rds")

if (BUILD_DATA) {
    de_files <- sort(list.files(DATA_ENGINE_DIR, pattern = "\\.R$", full.names = TRUE))
    for (f in de_files) source(f)
    cat("[OK] data_engine loaded\n")
}

# ---- model_engine_came ----
came_files <- c(
    "00_utils.R", "01_contracts.R", "02_state.R", "03_data_adapter.R", "04_risk_engine.R",
    "05_structure_engine.R", "06_signals.R", "07_features.R", "08_forecast.R", "09_optimizer.R",
    "10_runner.R", "99_smoke_tests.R"
)
for (f in came_files) source(file.path(CAME_DIR, f))
cat("[OK] model_engine_came loaded\n")

# ---- backtest_engine (patched versions) ----
bt_files <- c(
    "00_contracts_and_spec.R",
    "01_data_context_and_clock.R",
    "02_strategy_adapter_and_bridge.R",
    "03_execution_and_costs.R",
    "04_accounting.R",
    "05_runner.R",
    "06_analytics.R"
)
for (f in bt_files) source(file.path(BT_DIR, f))
cat("[OK] backtest_engine loaded\n")

# ──────────────────────────────────────────────────────────────────────────────
# 2) Build or load panel
# ──────────────────────────────────────────────────────────────────────────────
panel <- NULL

if (!BUILD_DATA) {
    if (!file.exists(PANEL_RDS)) stop("BUILD_DATA=FALSE but PANEL_RDS not found: ", PANEL_RDS)
    panel <- readRDS(PANEL_RDS)
} else {
    # ---- Configure your data_engine request (adjust as needed) ----
    start_date <- "2024-01-01"
    end_date <- "2025-12-31"

    # This matches your test idea; tweak as needed.
    res <- de_run_data_engine(
        start_date = start_date,
        end_date = end_date,
        overrides = list(
            include_types = c("equity", "fii", "etf", "bdr"),
            min_turnover = 5e5,
            min_days_traded_ratio = 0.75,
            ca_fetch_mode = "chart",
            ca_cache_mode = "batch"
        ),
        build_diag_features = FALSE
    )

    panel <- res$panel_adj_model
    if (!is.data.frame(panel)) stop("data_engine did not return a data.frame panel_adj_model")

    saveRDS(panel, PANEL_RDS)
    cat("[OK] panel built and saved to: ", PANEL_RDS, "\n")
}

cat("[INFO] panel rows:", nrow(panel), "\n")

# ──────────────────────────────────────────────────────────────────────────────
# 3) Choose benchmark symbol (IBOV proxy) available in panel
# ──────────────────────────────────────────────────────────────────────────────
# Real IBOV index often isn't directly tradable; common proxies:
# - BOVA11 (ETF tracking Ibovespa)
# - ^BVSP (Yahoo index symbol; depends on your data source)
# - IBOV (some providers)
BENCH_CANDIDATES <- c("BOVA11", "IBOV", "^BVSP", "BVSP", "IBOVESPA")

syms_all <- unique(as.character(panel$symbol))
bench_symbol <- BENCH_CANDIDATES[BENCH_CANDIDATES %in% syms_all][1]

if (is.na(bench_symbol) || is.null(bench_symbol)) {
    stop(
        "No benchmark symbol found in panel. Tried: ",
        paste(BENCH_CANDIDATES, collapse = ", "),
        "\nAvailable symbols example: ",
        paste(head(syms_all, 20), collapse = ", ")
    )
}

cat("[OK] benchmark symbol chosen:", bench_symbol, "\n")

# ──────────────────────────────────────────────────────────────────────────────
# 4) Define a benchmark strategy: buy & hold 100% in bench_symbol
# ──────────────────────────────────────────────────────────────────────────────
bt_strategy_buy_and_hold_symbol <- function(decision_date, data_context,
                                            portfolio_state, strategy_state_in,
                                            strategy_spec, runtime_ctx = list()) {
    decision_date <- as.Date(decision_date)
    sym <- strategy_spec$benchmark_symbol

    # if not visible in the strategy slice, we cannot target it
    sub <- data_context$dt[refdate == decision_date & symbol == sym]
    if (nrow(sub) == 0) {
        return(list(
            decision_date = decision_date,
            target_weights = data.frame(symbol = character(0), weight_target = numeric(0)),
            cash_weight = 1.0,
            warnings = paste0("Benchmark symbol not available on decision_date: ", sym),
            diagnostics = list(symbol = sym),
            strategy_state_out = strategy_state_in,
            meta = list(strategy = "benchmark", status = "WARMUP", ready_to_trade = FALSE, reason = "bench_not_in_slice")
        ))
    }

    list(
        decision_date = decision_date,
        target_weights = data.frame(symbol = sym, weight_target = 1.0),
        cash_weight = 0.0,
        warnings = character(0),
        diagnostics = list(symbol = sym),
        strategy_state_out = strategy_state_in,
        meta = list(strategy = "benchmark", status = "OK", ready_to_trade = TRUE, reason = "ok")
    )
}

# ──────────────────────────────────────────────────────────────────────────────
# 5) Backtester spec: universe selection + policies (top-K happens here)
# ──────────────────────────────────────────────────────────────────────────────
TOP_K <- 40L # <--- set to NULL for full base universe, or 40L / 100L / 300L etc.

bt_spec <- bt_get_spec(list(
    clock = list(freq = "months"),
    universe = list(
        lookback_days = 63L,
        min_days_traded_ratio = 0.75,
        min_median_traded_value = 5e5,
        include_types = c("equity", "fii", "etf", "bdr"),
        max_universe = TOP_K, # <--- TOP-K base universe selection
        min_price_history_days = 252L, # comparable rectangular history
        keep_holdings = TRUE
    ),
    policies = list(
        # comparable protocol: stay in cash until universe/model is ready
        warmup = list(action = "hold_cash", baseline_strategy = "all_cash"),
        cold_start = list(action = "hold_cash", baseline_strategy = "all_cash"),
        weight_tol = 1e-6
    ),
    runner = list(mode = "robust", verbose = TRUE)
))

# ──────────────────────────────────────────────────────────────────────────────
# 6) Strategy specs
# ──────────────────────────────────────────────────────────────────────────────
# CAME model spec (we keep it default; adapter will force strict=FALSE & cold_start_policy="skip" internally)
came_spec <- came_spec_default()

# Benchmark strategy spec
bench_spec <- list(benchmark_symbol = bench_symbol)

# Equal-weight needs no spec
eq_spec <- list()

# ──────────────────────────────────────────────────────────────────────────────
# 7) Run the three backtests
# ──────────────────────────────────────────────────────────────────────────────
cat("\n===== RUN: Equal-weight =====\n")
res_eq <- bt_run_backtest(
    data_bundle_or_panel = panel,
    strategy_fn = bt_strategy_equal_weight,
    strategy_spec = eq_spec,
    bt_spec = bt_spec
)
an_eq <- bt_compute_analytics(res_eq)
bt_print_summary(an_eq)

cat("\n===== RUN: Benchmark (buy & hold) =====\n")
res_bench <- bt_run_backtest(
    data_bundle_or_panel = panel,
    strategy_fn = bt_strategy_buy_and_hold_symbol,
    strategy_spec = bench_spec,
    bt_spec = bt_spec
)
an_bench <- bt_compute_analytics(res_bench)
bt_print_summary(an_bench)

cat("\n===== RUN: CAME =====\n")
res_came <- bt_run_backtest(
    data_bundle_or_panel = panel,
    strategy_fn = bt_strategy_from_model_engine,
    strategy_spec = came_spec,
    bt_spec = bt_spec
)
an_came <- bt_compute_analytics(res_came)
bt_print_summary(an_came)

# ──────────────────────────────────────────────────────────────────────────────
# 8) Combine NAV series and plot  (FIXED)
# ──────────────────────────────────────────────────────────────────────────────

nav_eq <- res_eq$nav_series
nav_bench <- res_bench$nav_series
nav_came <- res_came$nav_series

norm_nav <- function(df) {
    if (!is.data.frame(df) || nrow(df) == 0) {
        return(df)
    }
    df <- df[order(df$date), , drop = FALSE]
    df$nav_norm <- df$nav / df$nav[1]
    df
}

nav_eq <- norm_nav(nav_eq)
nav_bench <- norm_nav(nav_bench)
nav_came <- norm_nav(nav_came)

df_eq <- data.frame(date = nav_eq$date, EqualWeight = nav_eq$nav_norm)
df_bm <- data.frame(date = nav_bench$date, Benchmark = nav_bench$nav_norm)
df_cm <- data.frame(date = nav_came$date, CAME = nav_came$nav_norm)

nav_wide <- Reduce(
    function(x, y) merge(x, y, by = "date", all = TRUE),
    list(df_eq, df_bm, df_cm)
)
nav_wide <- nav_wide[order(nav_wide$date), , drop = FALSE]

# Save CSV
write.csv(nav_wide, OUT_CSV, row.names = FALSE)
cat("[OK] wrote:", OUT_CSV, "\n")

# Plot
png(OUT_PNG, width = 1400, height = 800)
par(mar = c(5, 5, 4, 2))
plot(nav_wide$date, nav_wide$EqualWeight,
    type = "l", lwd = 2,
    xlab = "Date", ylab = "NAV (normalized to 1)",
    main = sprintf(
        "Backtest Comparison (Top-K=%s, Benchmark=%s)",
        ifelse(is.null(TOP_K), "ALL", as.character(TOP_K)),
        bench_symbol
    )
)
lines(nav_wide$date, nav_wide$Benchmark, lwd = 2)
lines(nav_wide$date, nav_wide$CAME, lwd = 2)
legend("topleft",
    legend = c("EqualWeight", paste0("Benchmark_", bench_symbol), "CAME"),
    lwd = 2, bty = "n"
)
grid()
dev.off()
cat("[OK] wrote:", OUT_PNG, "\n")

# ──────────────────────────────────────────────────────────────────────────────
# 9) Print a compact final table
# ──────────────────────────────────────────────────────────────────────────────
extract_perf <- function(an) {
    p <- an$performance_full
    c(
        CAGR = p$cagr, Vol = p$vol, Sharpe = p$sharpe, MaxDD = p$max_dd,
        Start = as.character(p$start), End = as.character(p$end),
        ReadyExec = as.character(an$first_ready_exec_date %||% as.Date(NA))
    )
}

sum_tab <- rbind(
    EqualWeight = extract_perf(an_eq),
    Benchmark   = extract_perf(an_bench),
    CAME        = extract_perf(an_came)
)
cat("\n===== SUMMARY TABLE (FULL WINDOW) =====\n")
print(sum_tab)

cat("\nDone.\n")
