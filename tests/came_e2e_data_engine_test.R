# came_e2e_data_engine_test.R
# End-to-end smoke test of model_engine_came using real adjusted OHLCV from data_engine.

# --- Load data_engine --------------------------------------------------------
de_files <- sort(list.files("data_engine/R", pattern = "\\.R$", full.names = TRUE))
for (f in de_files) source(f)

# --- Load model_engine_came --------------------------------------------------
me_dir <- file.path("model_engine_came", "R")
source(file.path(me_dir, "00_utils.R"))
source(file.path(me_dir, "01_contracts.R"))
source(file.path(me_dir, "02_state.R"))
source(file.path(me_dir, "03_data_adapter.R"))
source(file.path(me_dir, "04_risk_engine.R"))
source(file.path(me_dir, "05_structure_engine.R"))
source(file.path(me_dir, "06_signals.R"))
source(file.path(me_dir, "07_features.R"))
source(file.path(me_dir, "08_forecast.R"))
source(file.path(me_dir, "09_optimizer.R"))
source(file.path(me_dir, "10_runner.R"))
source(file.path(me_dir, "99_smoke_tests.R"))

cat("--- Modules loaded ---\n")

# --- Build adjusted panel ----------------------------------------------------
# Choose a window big enough for lookback + H + warmups.
res <- de_run_data_engine(
    start_date = "2024-01-01",
    end_date = "2025-12-31",
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
stopifnot(is.data.frame(panel))
cat("panel rows:", nrow(panel), "\n")

# --- Reduce universe for speed (optional but recommended) --------------------
# Keep top N by median traded_value over last 63 days.
suppressWarnings({
    if (!requireNamespace("data.table", quietly = TRUE)) stop("Need data.table for this test.")
})
library(data.table)

dt <- as.data.table(panel)
dt[, refdate := as.Date(refdate)]
end_d <- max(dt$refdate, na.rm = TRUE)
last_dates <- sort(unique(dt$refdate))
last63 <- tail(last_dates, 63)

topN <- 40L
top_syms <- dt[refdate %in% last63,
    .(
        med_tv = median(traded_value, na.rm = TRUE),
        cov = .N / length(last63)
    ),
    by = symbol
][cov >= 0.75][order(-med_tv)][1:topN, symbol]

dt <- dt[symbol %in% top_syms]
dt <- dt[order(symbol, refdate)]

cat("symbols in test:", length(unique(dt$symbol)), "\n")

# --- Walk-forward run --------------------------------------------------------
spec <- came_spec_default()

# Allow cold-start to proceed during warmup (forecast/cold-start policy still applies)
spec$meta$strict <- FALSE

state <- came_state_init()
prev_target <- NULL

cal <- sort(unique(dt$refdate))

# Runner needs enough history for strict price matrices:
# lookback = max(L, max(mom_horizons), 63) + H + 5 + 1 (prices)
L <- as.integer(spec$risk$lookback)
H <- as.integer(spec$forecast$H)
need_days <- max(L, max(spec$signals$mom_horizons), 63L) + H + 5L + 1L

if (length(cal) < need_days + 20L) {
    stop(sprintf(
        "Not enough calendar days (%d) for required warmup (%d).",
        length(cal), need_days
    ))
}

start_idx <- need_days
dates_run <- cal[start_idx:length(cal)]

fit_seen <- FALSE
fit_first_date <- as.Date(NA)
n_ok <- 0L

# --- IMPORTANT FIX: iterate by index so d keeps Date class -------------------
for (i in seq_along(dates_run)) {
    d <- dates_run[i] # preserves Date class (for-loop over Date drops it)

    snap <- tryCatch(
        came_run_snapshot(
            data_bundle_or_panel = dt,
            as_of_date = d,
            spec = spec,
            state = state,
            prev_target = prev_target
        ),
        error = function(e) e
    )

    if (inherits(snap, "error")) {
        # keep original message (now includes [stage] if you patched came_run_stage)
        stop(paste0("Run failed on ", as.character(d), " -> ", conditionMessage(snap)))
    }

    # invariants
    came_smoke_check_snapshot(snap)

    state <- snap$state_out
    prev_target <- snap$weights
    n_ok <- n_ok + 1L

    # detect first real fit
    if (!fit_seen) {
        pn <- snap$forecast$diag$panel_n %||% NA_integer_
        if (is.finite(pn) && pn > 0) {
            fit_seen <- TRUE
            fit_first_date <- d
        }
    }

    if (n_ok %% 10L == 0L) cat("OK:", n_ok, " last:", as.character(d), "\n")
}

cat("\n--- WALK-FORWARD OK ---\n")
cat("snapshots:", n_ok, "\n")
cat("first fit date:", as.character(fit_first_date), "\n")
stopifnot(n_ok >= 10L)
stopifnot(isTRUE(fit_seen)) # must eventually fit with enough days
cat("--- E2E DATA_ENGINE TEST OK ---\n")
