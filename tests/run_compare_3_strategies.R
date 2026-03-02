# run_compare_3_strategies.R
options(stringsAsFactors = FALSE)

DATA_ENGINE_DIR <- file.path("data_engine", "R")
CAME_DIR <- file.path("model_engine_came", "R")
BT_DIR <- file.path("backtest_engine", "R")

OUT_DIR <- "bt_outputs"
if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)
OUT_PNG <- file.path(OUT_DIR, "nav_comparison.png")
OUT_CSV <- file.path(OUT_DIR, "nav_comparison.csv")

BUILD_DATA <- TRUE
PANEL_RDS <- file.path(OUT_DIR, "panel_adj_model.rds")

# ──────────────────────────────────────────────────────────────────────────────
# 0) Single source of truth: scored window + base lookback
# ──────────────────────────────────────────────────────────────────────────────
BT_START <- "2024-03-01"
BT_END <- "2025-12-15"

LOOKBACK_DAYS <- 252L # your "lookback window size"
UNIV_LB_DAYS <- 63L
TOP_K <- 40L

# extra trading-day buffers (CA fetch window + safety + exec_lag)
PRE_BUFFER <- 20L
POST_BUFFER <- 10L

# ──────────────────────────────────────────────────────────────────────────────
# 1) Load modules
# ──────────────────────────────────────────────────────────────────────────────
if (BUILD_DATA) {
    de_files <- sort(list.files(DATA_ENGINE_DIR, pattern = "\\.R$", full.names = TRUE))
    for (f in de_files) source(f)
    cat("[OK] data_engine loaded\n")
}

came_files <- c(
    "00_utils.R", "01_contracts.R", "02_state.R", "03_data_adapter.R", "04_risk_engine.R",
    "05_structure_engine.R", "06_signals.R", "07_features.R", "08_forecast.R", "09_optimizer.R",
    "10_runner.R", "99_smoke_tests.R"
)
for (f in came_files) source(file.path(CAME_DIR, f))
cat("[OK] model_engine_came loaded\n")

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
# 2) Strategies (benchmark created later after we know symbol)
# ──────────────────────────────────────────────────────────────────────────────
attr(bt_strategy_equal_weight, "bt_requirements") <- list(
    model_id = "equal_weight",
    prehistory_days = as.integer(LOOKBACK_DAYS),
    stateful = FALSE,
    supports_replay = TRUE
)

# CAME spec + requirements (used to derive W_star BEFORE building data)
came_spec <- came_spec_default()
req_came <- bt_strategy_requirements(bt_strategy_from_model_engine, came_spec)

# Universe-driven requirement (backtester enforces investability rules strictly)
W_univ <- max(as.integer(UNIV_LB_DAYS), as.integer(LOOKBACK_DAYS))
W_star <- max(W_univ, as.integer(req_came$prehistory_days), as.integer(LOOKBACK_DAYS))

cat(sprintf("[INFO] W_star (required prehistory trading days) = %d\n", W_star))

# Enforce equal model-life prehistory across comparable strategies.
attr(bt_strategy_equal_weight, "bt_requirements") <- list(
    model_id = "equal_weight",
    prehistory_days = as.integer(W_star),
    stateful = FALSE,
    supports_replay = TRUE
)

# ──────────────────────────────────────────────────────────────────────────────
# 3) Build or load panel (data window derived from BT_START/BT_END + W_star)
# ──────────────────────────────────────────────────────────────────────────────
panel <- NULL

if (!BUILD_DATA) {
    if (!file.exists(PANEL_RDS)) stop("BUILD_DATA=FALSE but PANEL_RDS not found: ", PANEL_RDS)
    panel <- readRDS(PANEL_RDS)
} else {
    # ---- bizdays calendar init (REQUIRED when using bizdays:: without attaching) ----
    if (!requireNamespace("bizdays", quietly = TRUE)) stop("Package 'bizdays' required.")
    bizdays::load_builtin_calendars()

    CAL <- "Brazil/B3"
    if (!bizdays::has_calendars(CAL)) {
        stop(sprintf("bizdays calendar '%s' is not registered after load_builtin_calendars().", CAL))
    }

    A_req <- as.Date(BT_START)
    B_req <- as.Date(BT_END)
    if (is.na(A_req) || is.na(B_req)) stop("BT_START/BT_END must be YYYY-MM-DD.")

    # strict: do NOT roll. if user passes a non-bizday, error.
    if (!bizdays::is.bizday(A_req, CAL)) stop(sprintf("BT_START=%s is not a B3 business day.", A_req))
    if (!bizdays::is.bizday(B_req, CAL)) stop(sprintf("BT_END=%s is not a B3 business day.", B_req))

    safe_add_bizdays <- function(date, n, cal) {
        d <- as.Date(date)
        n <- as.integer(n)
        out <- bizdays::add.bizdays(d, n, cal)
        if (!is.na(out)) return(out)

        cal_obj <- tryCatch(bizdays::calendars()[[cal]], error = function(e) NULL)
        if (is.null(cal_obj)) {
            stop(sprintf("bizdays::add.bizdays returned NA and calendar '%s' metadata is unavailable.", cal))
        }

        cal_start <- as.Date(cal_obj$start.date)
        cal_end <- as.Date(cal_obj$end.date)

        if (n >= 0L) {
            if (is.na(cal_end)) stop(sprintf("Calendar '%s' has invalid end.date.", cal))
            edge <- bizdays::adjust.previous(cal_end, cal)
            if (is.na(edge) || edge < d) {
                stop(sprintf("Cannot offset %s by %+d bizdays in '%s' (calendar end=%s).", as.character(d), n, cal, as.character(cal_end)))
            }
            warning(sprintf("Clamping forward bizday offset to calendar end: requested %s %+d -> %s", as.character(d), n, as.character(edge)), call. = FALSE)
            return(edge)
        }

        if (is.na(cal_start)) stop(sprintf("Calendar '%s' has invalid start.date.", cal))
        edge <- bizdays::adjust.next(cal_start, cal)
        if (is.na(edge) || edge > d) {
            stop(sprintf("Cannot offset %s by %+d bizdays in '%s' (calendar start=%s).", as.character(d), n, cal, as.character(cal_start)))
        }
        warning(sprintf("Clamping backward bizday offset to calendar start: requested %s %+d -> %s", as.character(d), n, as.character(edge)), call. = FALSE)
        edge
    }

    need_pre <- as.integer(W_star + PRE_BUFFER)
    need_post <- as.integer(POST_BUFFER)

    DE_START <- safe_add_bizdays(A_req, -need_pre, CAL)
    DE_END <- safe_add_bizdays(B_req, need_post, CAL)

    if (is.na(DE_START) || is.na(DE_END)) {
        stop(sprintf("bizdays::add.bizdays returned NA (DE_START=%s, DE_END=%s).", DE_START, DE_END))
    }

    cat(sprintf("[INFO] data_engine build window: %s to %s\n", as.character(DE_START), as.character(DE_END)))

    # ---- build data with derived window ----
    res <- de_run_data_engine(
        start_date = as.character(DE_START),
        end_date = as.character(DE_END),
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
    if (!is.data.frame(panel)) stop("data_engine did not return panel_adj_model as data.frame")

    saveRDS(panel, PANEL_RDS)
    cat("[OK] panel built and saved to: ", PANEL_RDS, "\n")
}

cat("[INFO] panel rows:", nrow(panel), "\n")

# ──────────────────────────────────────────────────────────────────────────────
# 4) Choose benchmark symbol
# ──────────────────────────────────────────────────────────────────────────────
BENCH_CANDIDATES <- c("BOVA11", "IBOV", "^BVSP", "BVSP", "IBOVESPA")
syms_all <- unique(as.character(panel$symbol))
bench_symbol <- BENCH_CANDIDATES[BENCH_CANDIDATES %in% syms_all][1]
if (is.na(bench_symbol) || is.null(bench_symbol)) {
    stop("No benchmark symbol found. Tried: ", paste(BENCH_CANDIDATES, collapse = ", "))
}
cat("[OK] benchmark symbol chosen:", bench_symbol, "\n")

# Benchmark strategy (strict: must be available on decision date)
bt_strategy_buy_and_hold_symbol <- function(decision_date, data_context,
                                            portfolio_state, strategy_state_in,
                                            strategy_spec, runtime_ctx = list()) {
    decision_date <- as.Date(decision_date)
    sym <- strategy_spec$benchmark_symbol

    sub <- data_context$dt[refdate == decision_date & symbol == sym]
    if (nrow(sub) == 0) stop(sprintf("[bench_not_in_slice] %s not available on %s", sym, as.character(decision_date)))

    list(
        decision_date = decision_date,
        target_weights = data.frame(symbol = sym, weight_target = 1.0, stringsAsFactors = FALSE),
        cash_weight = 0.0,
        strategy_state_out = strategy_state_in,
        meta = list(strategy = "benchmark")
    )
}

attr(bt_strategy_buy_and_hold_symbol, "bt_requirements") <- list(
    model_id = "benchmark_bh",
    prehistory_days = as.integer(W_star),
    stateful = FALSE,
    supports_replay = TRUE
)

bench_spec <- list(benchmark_symbol = bench_symbol)
eq_spec <- list()

# ──────────────────────────────────────────────────────────────────────────────
# 5) Backtester spec (v3 strict)
# ──────────────────────────────────────────────────────────────────────────────
bt_spec <- bt_get_spec(list(
    clock = list(freq = "months"),
    universe = list(
        lookback_days = as.integer(UNIV_LB_DAYS),
        min_days_traded_ratio = 0.75,
        min_median_traded_value = 5e5,
        include_types = c("equity", "fii", "etf", "bdr"),
        max_universe = as.integer(TOP_K),
        min_price_history_days = as.integer(LOOKBACK_DAYS),
        keep_holdings = TRUE
    ),
    execution = list(price_field = "open", exec_lag = 1L),
    costs = list(fee_rate = 0.0003, slippage_rate = 0.0010),
    accounting = list(
        initial_nav = 100000,
        initial_cash = 100000,
        mark_field = "close",
        mark_missing_policy = "carry_last",
        max_stale_mark_days = NULL
    ),
    runner = list(mode = "strict", verbose = TRUE),
    analytics = list(rf_annual = 0.0)
))

# ──────────────────────────────────────────────────────────────────────────────
# 6) Run backtests (scored window only)
# ──────────────────────────────────────────────────────────────────────────────
cat("\n===== RUN: Equal-weight =====\n")
res_eq <- bt_run_backtest_range(panel, bt_strategy_equal_weight, eq_spec, BT_START, BT_END, bt_spec)
an_eq <- bt_compute_analytics(res_eq)
bt_print_summary(an_eq)

cat("\n===== RUN: Benchmark =====\n")
res_bm <- bt_run_backtest_range(panel, bt_strategy_buy_and_hold_symbol, bench_spec, BT_START, BT_END, bt_spec)
an_bm <- bt_compute_analytics(res_bm)
bt_print_summary(an_bm)

cat("\n===== RUN: CAME =====\n")
res_cm <- bt_run_backtest_range(panel, bt_strategy_from_model_engine, came_spec, BT_START, BT_END, bt_spec)
an_cm <- bt_compute_analytics(res_cm)
bt_print_summary(an_cm)

# ──────────────────────────────────────────────────────────────────────────────
# 7) Plot scored NAVs (daily)
# ──────────────────────────────────────────────────────────────────────────────
norm_nav <- function(df) {
    if (!is.data.frame(df) || nrow(df) < 1) stop("No scored NAV rows available for plotting.")
    df <- df[order(as.Date(df$date)), , drop = FALSE]
    df$nav_norm <- df$nav / df$nav[1]
    df
}

nav_eq <- norm_nav(res_eq$nav[!is.na(res_eq$nav$in_score) & as.logical(res_eq$nav$in_score), c("date", "nav")])
nav_bm <- norm_nav(res_bm$nav[!is.na(res_bm$nav$in_score) & as.logical(res_bm$nav$in_score), c("date", "nav")])
nav_cm <- norm_nav(res_cm$nav[!is.na(res_cm$nav$in_score) & as.logical(res_cm$nav$in_score), c("date", "nav")])

df_eq <- data.frame(date = as.Date(nav_eq$date), EqualWeight = as.numeric(nav_eq$nav_norm))
df_bm <- data.frame(date = as.Date(nav_bm$date), Benchmark = as.numeric(nav_bm$nav_norm))
df_cm <- data.frame(date = as.Date(nav_cm$date), CAME = as.numeric(nav_cm$nav_norm))

nav_wide <- Reduce(function(x, y) merge(x, y, by = "date", all = TRUE), list(df_eq, df_bm, df_cm))
nav_wide <- nav_wide[order(as.Date(nav_wide$date)), , drop = FALSE]

write.csv(nav_wide, OUT_CSV, row.names = FALSE)
cat("[OK] wrote:", OUT_CSV, "\n")

png(OUT_PNG, width = 1800, height = 980, res = 140)
old_par <- par(no.readonly = TRUE)
on.exit(par(old_par), add = TRUE)
par(mar = c(5.3, 6.0, 5.2, 2.2), bg = "#f8f9fb", fg = "#1b1f24", xaxs = "i", yaxs = "i")

cols <- c(EqualWeight = "#0f4c81", Benchmark = "#6c757d", CAME = "#d1495b")
yv <- as.matrix(nav_wide[, c("EqualWeight", "Benchmark", "CAME")])
yrng <- range(yv, na.rm = TRUE)
ypad <- 0.06 * (yrng[2] - yrng[1])
if (!is.finite(ypad) || ypad <= 0) ypad <- 0.05
ylim <- c(yrng[1] - ypad, yrng[2] + ypad)

plot(nav_wide$date, nav_wide$EqualWeight,
    type = "n", ylim = ylim,
    xaxt = "n", yaxt = "n",
    xlab = "", ylab = "",
    main = ""
)

usr <- par("usr")
y_ticks <- pretty(ylim, n = 8)
for (yt in y_ticks) {
    segments(usr[1], yt, usr[2], yt, col = "#d9dde3", lwd = 0.8)
}
x_ticks <- seq(min(nav_wide$date), max(nav_wide$date), by = "3 months")
axis(1, at = x_ticks, labels = format(x_ticks, "%b/%y"), las = 2, cex.axis = 0.86, col.axis = "#4a5560", tck = -0.015)
axis(2, at = y_ticks, labels = sprintf("%.2f", y_ticks), las = 1, cex.axis = 0.9, col.axis = "#4a5560", tck = -0.015)

title(main = "Backtest NAV Comparison", cex.main = 1.48, font.main = 2, col.main = "#101418")
mtext(sprintf("Score Window: %s to %s | Top-K: %s | Benchmark: %s",
    BT_START, BT_END, ifelse(is.null(TOP_K), "ALL", as.character(TOP_K)), bench_symbol
), side = 3, line = 1.2, cex = 0.95, col = "#4a5560")
mtext("Date", side = 1, line = 4.3, cex = 1.02, col = "#2f3a45")
mtext("Normalized NAV (start = 1.00)", side = 2, line = 4.1, cex = 1.02, col = "#2f3a45")

lines(nav_wide$date, nav_wide$EqualWeight, col = cols["EqualWeight"], lwd = 3.0)
lines(nav_wide$date, nav_wide$Benchmark, col = cols["Benchmark"], lwd = 2.8)
lines(nav_wide$date, nav_wide$CAME, col = cols["CAME"], lwd = 3.0)

last_i <- nrow(nav_wide)
points(nav_wide$date[last_i], nav_wide$EqualWeight[last_i], pch = 21, bg = cols["EqualWeight"], col = "white", cex = 1.2, lwd = 1.2)
points(nav_wide$date[last_i], nav_wide$Benchmark[last_i], pch = 21, bg = cols["Benchmark"], col = "white", cex = 1.2, lwd = 1.2)
points(nav_wide$date[last_i], nav_wide$CAME[last_i], pch = 21, bg = cols["CAME"], col = "white", cex = 1.2, lwd = 1.2)

legend("topleft",
    legend = c("Equal Weight", paste0("Benchmark (", bench_symbol, ")"), "CAME"),
    col = cols[c("EqualWeight", "Benchmark", "CAME")],
    lwd = c(3.0, 2.8, 3.0),
    bty = "n",
    cex = 0.95,
    inset = c(0.01, 0.01)
)

box(col = "#9aa4af", lwd = 0.9)
dev.off()
cat("[OK] wrote:", OUT_PNG, "\n")

cat("\nDone.\n")
