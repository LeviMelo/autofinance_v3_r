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
    # CA cache behavior:
    # - "batch": cache one validated CA table for the exact (symbols, from, to, fetch_mode) request hash
    # - "by_symbol": debug/inspection mode; caches per-symbol exact-window payloads
    # - "none": bypass CA cache (useful for debugging vendor fetch behavior)
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
  if (!is.list(cfg)) stop("cfg must be a list")
  
  # Helpers
  check_logical <- function(x) {
    length(x) == 1 && is.logical(x) && !is.na(x)
  }
  
  check_str <- function(x) {
    length(x) == 1 && is.character(x) && !is.na(x) && nzchar(trimws(x))
  }
  
  check_num <- function(x, min_val = -Inf, max_val = Inf) {
    length(x) == 1 && is.numeric(x) && !is.na(x) && is.finite(x) && x >= min_val && x <= max_val
  }
  
  check_int <- function(x, min_val = -Inf, max_val = Inf) {
    check_num(x, min_val, max_val) && (x %% 1 == 0)
  }
  
  # Enums
  if (length(cfg$ca_cache_mode) != 1 || !cfg$ca_cache_mode %in% c("batch", "by_symbol", "none")) {
    stop("Invalid ca_cache_mode. Must be 'batch', 'by_symbol', or 'none' (debug).")
  }
  if (length(cfg$ca_fetch_mode) != 1 || !cfg$ca_fetch_mode %in% c("chart", "quantmod")) {
    stop("Invalid ca_fetch_mode. Must be 'chart' or 'quantmod'.")
  }
  
  allowed_types <- c("equity", "fii", "etf", "bdr")
  if (length(cfg$include_types) == 0 || any(is.na(cfg$include_types)) || !all(cfg$include_types %in% allowed_types)) {
    stop("include_types is empty or contains invalid assets. Allowed: ", paste(allowed_types, collapse = ", "))
  }
  
  # Logical flags
  if (!check_logical(cfg$enable_manual_events)) stop("enable_manual_events must be TRUE/FALSE")
  if (!check_logical(cfg$enable_split_gap_validation)) stop("enable_split_gap_validation must be TRUE/FALSE")
  if (!check_logical(cfg$split_gap_use_open)) stop("split_gap_use_open must be TRUE/FALSE")
  
  # Directories
  if (!check_str(cfg$cache_dir)) stop("cache_dir must be a non-empty string")
  if (!check_str(cfg$raw_dir))   stop("raw_dir must be a non-empty string")
  if (!check_str(cfg$logs_dir))  stop("logs_dir must be a non-empty string")
  
  # Numeric boundaries
  if (!check_num(cfg$min_turnover, 0)) stop("min_turnover must be numeric >= 0")
  if (!check_num(cfg$min_days_traded_ratio, 0, 1)) stop("min_days_traded_ratio must be between 0 and 1")
  if (!check_int(cfg$ca_prefilter_liq_window_days, 1)) stop("ca_prefilter_liq_window_days must be integer >= 1")
  
  if (!check_num(cfg$ca_prefilter_jump_log_thr, 0)) stop("ca_prefilter_jump_log_thr must be > 0")
  if (!check_num(cfg$split_gap_tol_log, 0)) stop("split_gap_tol_log must be > 0")
  if (!check_num(cfg$adj_residual_jump_tol_log, 0)) stop("adj_residual_jump_tol_log must be > 0")
  
  # Gap thresholds (diagnostic gap cuts should be <= 0)
  if (!check_num(cfg$ca_prefilter_gap_equity, -Inf, 0)) stop("ca_prefilter_gap_equity must be <= 0")
  if (!check_num(cfg$ca_prefilter_gap_fii, -Inf, 0))    stop("ca_prefilter_gap_fii must be <= 0")
  if (!check_num(cfg$ca_prefilter_gap_etf, -Inf, 0))    stop("ca_prefilter_gap_etf must be <= 0")
  if (!check_num(cfg$ca_prefilter_gap_bdr, -Inf, 0))    stop("ca_prefilter_gap_bdr must be <= 0")
  
  # Integer windows
  if (!check_int(cfg$split_gap_max_forward_days, 0)) stop("split_gap_max_forward_days must be integer >= 0")
  if (!check_int(cfg$split_gap_max_back_days, 0))    stop("split_gap_max_back_days must be integer >= 0")
  
  # Feature horizons
  if (!is.numeric(cfg$feature_horizons_days) ||
      length(cfg$feature_horizons_days) == 0 ||
      any(is.na(cfg$feature_horizons_days)) ||
      any(!is.finite(cfg$feature_horizons_days)) ||
      any(cfg$feature_horizons_days <= 0) ||
      any(cfg$feature_horizons_days %% 1 != 0)) {
    stop("feature_horizons_days must be strictly positive finite integers")
  }
  
  # Optional default years (if present)
  if (!is.null(cfg$years)) {
    if (!is.numeric(cfg$years) ||
        length(cfg$years) == 0 ||
        any(is.na(cfg$years)) ||
        any(!is.finite(cfg$years)) ||
        any(cfg$years %% 1 != 0)) {
      stop("cfg$years must be finite integers when provided")
    }
  }
  
  invisible(TRUE)
}

de_get_config <- function(overrides = NULL) {
  cfg <- de_config_default()
  
  if (!is.null(overrides)) {
    if (!is.list(overrides)) stop("'overrides' must be a named list.")
    ov_names <- names(overrides)
    if (is.null(ov_names) || any(is.na(ov_names)) || any(!nzchar(trimws(ov_names)))) {
      stop("'overrides' must be a named list with non-empty names.")
    }
    
    unknown <- setdiff(ov_names, names(cfg))
    if (length(unknown)) {
      stop("Unknown config override key(s): ", paste(unknown, collapse = ", "), call. = FALSE)
    }
    
    for (nm in ov_names) cfg[[nm]] <- overrides[[nm]]
  }
  
  de_validate_config(cfg)
  
  # Canonicalize a few vectors after validation
  cfg$include_types <- unique(as.character(cfg$include_types))
  cfg$feature_horizons_days <- sort(unique(as.integer(cfg$feature_horizons_days)))
  if (!is.null(cfg$years)) cfg$years <- sort(unique(as.integer(cfg$years)))
  
  dir.create(cfg$cache_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(cfg$raw_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(cfg$logs_dir, recursive = TRUE, showWarnings = FALSE)
  
  cfg
}