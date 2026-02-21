# /data_engine/R/01_config.R

de_config_default <- function() {
  list(
    years = { y <- as.integer(format(Sys.Date(), "%Y")); (y-1L):y },
    include_types = c("equity", "fii", "etf", "bdr"),
    
    cache_dir  = "data/cache",
    raw_dir    = "data/raw",
    logs_dir   = "logs",
    
    min_turnover = 5e5,
    min_days_traded_ratio = 0.8,
    ca_prefilter_liq_window_days = 63L,
    
    # Audit gap flags (diagnostic only now)
    ca_prefilter_jump_log_thr = 1.0, 
    ca_prefilter_gap_equity = -0.20, 
    ca_prefilter_gap_fii    = -0.12,
    ca_prefilter_gap_etf    = -0.15,
    ca_prefilter_gap_bdr    = -0.20,
    
    ca_fetch_mode = "chart", 
    ca_cache_mode = "batch", 
    enable_manual_events = TRUE,
    
    enable_split_gap_validation = TRUE,
    split_gap_tol_log = 0.35,
    split_gap_max_forward_days = 5L,
    split_gap_max_back_days = 3L,
    split_gap_use_open = TRUE,
    
    adj_residual_jump_tol_log = 1.0,
    feature_horizons_days = c(21L, 63L, 126L, 252L)
  )
}

de_validate_config <- function(cfg) {
  if (!cfg$ca_cache_mode %in% c("batch", "by_symbol", "none")) {
    stop("Invalid ca_cache_mode. Must be 'batch', 'by_symbol', or 'none'.")
  }
  if (!cfg$ca_fetch_mode %in% c("chart", "quantmod")) {
    stop("Invalid ca_fetch_mode. Must be 'chart' or 'quantmod'.")
  }
  invisible(TRUE)
}

de_get_config <- function(overrides = NULL) {
  cfg <- de_config_default()
  if (!is.null(overrides)) {
    for (nm in names(overrides)) cfg[[nm]] <- overrides[[nm]]
  }
  
  de_validate_config(cfg)
  
  dir.create(cfg$cache_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(cfg$raw_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(cfg$logs_dir, recursive = TRUE, showWarnings = FALSE)
  
  cfg
}