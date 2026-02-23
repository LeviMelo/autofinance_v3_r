## ============================================================
## STRICT E2E TEST: data_engine -> model_engine
## - No synthetic fallback
## - Fails loudly on package gaps / cash fallback / empty artifacts
## ============================================================

options(error = NULL)
options(stringsAsFactors = FALSE)
options(repos = c(CRAN = "https://cloud.r-project.org"))

cat("=== START test_model_engine_e2e_strict ===\n")

# ------------------------------------------------------------
# 0) Helpers
# ------------------------------------------------------------
source_r_dir <- function(dir) {
    files <- sort(list.files(dir, pattern = "\\.R$", full.names = TRUE))
    if (!length(files)) stop("No .R files found in: ", dir, call. = FALSE)
    for (f in files) {
        message("Sourcing: ", f)
        source(f, local = .GlobalEnv)
    }
    invisible(files)
}

require_or_stop <- function(pkgs) {
    miss <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
    if (length(miss)) {
        stop(
            paste0(
                "Missing required packages: ", paste(miss, collapse = ", "), "\n",
                "Install them first:\n",
                "install.packages(c(", paste(sprintf('"%s"', miss), collapse = ", "), "), dependencies = TRUE)"
            ),
            call. = FALSE
        )
    }
    invisible(TRUE)
}

fail_if_warnings_match <- function(warns, patterns) {
    if (length(warns) == 0) {
        return(invisible(TRUE))
    }
    for (p in patterns) {
        hit <- grepl(p, warns, ignore.case = TRUE)
        if (any(hit)) {
            stop(
                "Fatal warning detected in snapshot output:\n- ",
                paste(warns[hit], collapse = "\n- "),
                call. = FALSE
            )
        }
    }
    invisible(TRUE)
}

# ------------------------------------------------------------
# 1) Package gate (strict)
# ------------------------------------------------------------
pkgs_needed <- c(
    # data_engine
    "data.table", "dplyr", "rb3", "bizdays", "digest",
    "jsonlite", "curl", "quantmod", "zoo",
    # model_engine
    "Matrix", "glasso"
)
require_or_stop(pkgs_needed)

# ------------------------------------------------------------
# 2) Source both engines
# ------------------------------------------------------------
source_r_dir("data_engine/R")
source_r_dir("model_engine/R")

# ------------------------------------------------------------
# 3) Build REAL data bundle (in-session)
# ------------------------------------------------------------
cat("\n=== Running data_engine (real) ===\n")

# NOTE:
# - We keep split validation ON (default) to test more of the real path.
# - You can set min_turnover higher/lower depending on your universe size.
# - This test expects data_engine to work and produce panel_adj_model.
data_bundle <- de_run_data_engine(
    years = c(2024L, 2025L),
    force_symbols = NULL,
    manual_events = NULL,
    overrides = list(
        include_types = c("equity", "fii", "etf", "bdr"),
        # Keep default-ish modeling candidates; tune if your machine/network is slow.
        # min_turnover = 5e5
        # ca_cache_mode = "batch"
        # ca_fetch_mode = "chart"
        # enable_split_gap_validation = TRUE
        logs_dir = "logs"
    ),
    build_diag_features = TRUE
)

if (is.null(data_bundle) || !is.list(data_bundle)) {
    stop("data_engine returned NULL / non-list.", call. = FALSE)
}
if (!"panel_adj_model" %in% names(data_bundle)) {
    stop("data_engine bundle missing panel_adj_model.", call. = FALSE)
}

panel_adj_model <- data_bundle$panel_adj_model

if (!is.data.frame(panel_adj_model)) stop("panel_adj_model is not a data.frame", call. = FALSE)
if (nrow(panel_adj_model) == 0) stop("panel_adj_model is empty", call. = FALSE)

req_panel_cols <- c("symbol", "refdate", "asset_type", "open", "high", "low", "close", "turnover", "qty", "adjustment_state")
miss_panel <- setdiff(req_panel_cols, names(panel_adj_model))
if (length(miss_panel)) stop("panel_adj_model missing cols: ", paste(miss_panel, collapse = ", "), call. = FALSE)

cat("panel_adj_model rows: ", nrow(panel_adj_model), "\n", sep = "")
cat("date range: ", as.character(min(panel_adj_model$refdate)), " -> ", as.character(max(panel_adj_model$refdate)), "\n", sep = "")
cat("asset types in panel: ", paste(sort(unique(panel_adj_model$asset_type)), collapse = ", "), "\n", sep = "")
cat("unique symbols: ", length(unique(panel_adj_model$symbol)), "\n", sep = "")

# ------------------------------------------------------------
# 4) Build model spec (PATCHED compatibility + explicit gating W)
# ------------------------------------------------------------
cat("\n=== Building model spec ===\n")

spec <- me_spec_default()

# PATCH: align model engine investability filter with data_engine output taxonomy
spec$data$allowed_types <- c("equity", "fii", "etf", "bdr")

# Keep the gating structure explicit / crystallized (even if static until calibrated)
spec$gating$W <- matrix(
    c(
        0, 0, 0, # kalman
        0, 0, 0, # tsmom
        0, 0, 0 # cash
    ),
    nrow = 3, byrow = TRUE,
    dimnames = list(c("kalman", "tsmom", "cash"), c("disp", "eta", "VoV"))
)

me_validate_spec(spec)

# ------------------------------------------------------------
# 5) Run snapshot on latest panel date
# ------------------------------------------------------------
as_of_date <- max(as.Date(panel_adj_model$refdate), na.rm = TRUE)
cat("\n=== Running model snapshot at ", as.character(as_of_date), " ===\n", sep = "")

res <- me_run_snapshot(
    data_bundle_or_panel = panel_adj_model,
    as_of_date = as_of_date,
    spec = spec
)

# ------------------------------------------------------------
# 6) HARD FAIL CONDITIONS (meaningful execution, not just shape)
# ------------------------------------------------------------
cat("\n=== Snapshot diagnostics ===\n")
cat("as_of_date: ", as.character(res$as_of_date), "\n", sep = "")
cat("tradable symbols: ", length(res$tradable_symbols), "\n", sep = "")
cat("target rows: ", nrow(res$target_weights), "\n", sep = "")
cat("cash weight: ", format(res$cash_weight, digits = 6), "\n", sep = "")

if (length(res$warnings)) {
    cat("warnings:\n")
    for (w in res$warnings) cat(" - ", w, "\n", sep = "")
} else {
    cat("warnings: <none>\n")
}

# Fail on risk-engine fallback / cash revert warnings
fail_if_warnings_match(
    res$warnings,
    c("Risk engine failed", "Reverting to cash position")
)

# Fail if no tradables (for a full 2-year equity panel this should not happen)
if (length(res$tradable_symbols) == 0) {
    stop("No tradable symbols were selected. Investigate investability filters/spec$data.", call. = FALSE)
}

# Fail if risk/signal/state/gating artifacts are empty (indicates hidden fallback)
if (length(res$risk) == 0) stop("Risk artifact is empty.", call. = FALSE)
if (length(res$signals) == 0) stop("Signal artifact is empty.", call. = FALSE)
if (length(res$market_state) == 0) stop("Market-state artifact is empty.", call. = FALSE)
if (length(res$gating) == 0) stop("Gating artifact is empty.", call. = FALSE)

# Fail if fully cash without explicit reason (you can relax this later if gating calibration intentionally does this)
if (isTRUE(all.equal(res$cash_weight, 1.0, tolerance = 1e-6))) {
    stop(
        paste0(
            "Snapshot collapsed to 100% cash. This is a hard fail in this strict test.\n",
            "Check: glasso installation, risk lookback coverage, gating logits/W, and dropped_assets."
        ),
        call. = FALSE
    )
}

# Fail if target is empty despite non-cash
if (nrow(res$target_weights) == 0) {
    stop("Target weights are empty despite non-cash allocation. Investigate portfolio constructor path.", call. = FALSE)
}

# Weight sum integrity
tot_w <- sum(res$target_weights$weight_target, na.rm = TRUE) + res$cash_weight
if (!is.finite(tot_w) || abs(tot_w - 1) > 1e-4) {
    stop(sprintf("Weight sum check failed: total = %.8f", tot_w), call. = FALSE)
}

# Negative weights not allowed in current design
if (any(res$target_weights$weight_target < -1e-10, na.rm = TRUE)) {
    stop("Negative target weights detected.", call. = FALSE)
}

# ------------------------------------------------------------
# 7) Verbose output (useful for debugging)
# ------------------------------------------------------------
cat("\n--- Gating ---\n")
print(res$gating)

cat("\n--- Market state ---\n")
print(res$market_state)

cat("\n--- Risk diag ---\n")
if (!is.null(res$risk$diag)) print(res$risk$diag)

if (!is.null(res$risk$w_hrp)) {
    cat("HRP universe size: ", length(res$risk$w_hrp), "\n", sep = "")
}

cat("\n--- Portfolio diag ---\n")
print(res$portfolio_diag)

cat("\n--- Top target weights ---\n")
print(utils::head(res$target_weights[order(-res$target_weights$weight_target), ], 20))

cat("\n✅ STRICT E2E test passed (real data_engine + model_engine).\n")
cat("=== END test_model_engine_e2e_strict ===\n")
