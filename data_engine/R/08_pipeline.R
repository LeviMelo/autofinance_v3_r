# /data_engine/R/08_pipeline.R

de_run_data_engine <- function(years = NULL,
                               start_date = NULL, end_date = NULL,
                               force_symbols = NULL,
                               manual_events = NULL,
                               overrides = NULL,
                               build_diag_features = TRUE) {
  de_require(c("data.table", "dplyr"))

  # Input mode validation
  has_years <- !is.null(years)
  has_start <- !is.null(start_date)
  has_end <- !is.null(end_date)

  if (has_years && (has_start || has_end)) {
    stop("Provide either 'years' OR 'start_date/end_date', not both.", call. = FALSE)
  }
  if (xor(has_start, has_end)) {
    stop("Provide both 'start_date' and 'end_date' together.", call. = FALSE)
  }
  if (has_start && has_end) {
    if (as.Date(start_date) > as.Date(end_date)) {
      stop("'start_date' must be <= 'end_date'.", call. = FALSE)
    }
  }
  if (!has_years && !(has_start && has_end)) {
    stop("Must provide either 'years' or 'start_date/end_date'.", call. = FALSE)
  }
  if (length(build_diag_features) != 1 || is.na(build_diag_features) || !is.logical(build_diag_features)) {
    stop("'build_diag_features' must be TRUE/FALSE.", call. = FALSE)
  }

  cfg <- de_get_config(overrides)
  de_log_cfg(cfg)

  # 1) Build Universe
  de_log("DE_PIPE:", "Building raw universe...")
  if (has_years) {
    u_raw <- de_build_universe(years, cfg)
  } else {
    u_raw <- de_build_universe_window(start_date, end_date, cfg)
  }

  de_log("DE_PIPE:", "Liquidity semantics frozen: turnover=traded_value(BRL), qty=traded_units, n_trades=trade count.")

  # 2) Select CA Candidates (Modeling set)
  de_log("DE_PIPE:", "Selecting candidates...")
  prefilter_res <- de_select_ca_candidates(u_raw, cfg, force_symbols)
  cand <- prefilter_res$candidates
  de_log("DE_PIPE:", "Candidates strictly eligible for modeling: ", length(cand))

  # 3) Fetch Yahoo CA
  from_ca <- min(u_raw$refdate, na.rm = TRUE) - 10
  to_ca <- max(u_raw$refdate, na.rm = TRUE) + 10

  empty_ca <- data.table::data.table(
    symbol = character(),
    yahoo_symbol = character(),
    refdate = as.Date(character()),
    action_type = character(),
    value = numeric(),
    source = character()
  )

  ca_raw <- data.table::copy(empty_ca)
  ca_apply <- data.table::copy(empty_ca)
  sp_audit <- data.table::data.table()
  ca_quar <- data.table::data.table()

  if (length(cand)) {
    de_log("DE_PIPE:", "Fetching CA Registry...")
    ca_raw <- de_build_ca_registry(cand, from_ca, to_ca, cfg)

    # 4) Fix / Snap Splits
    if (nrow(ca_raw) && cfg$enable_split_gap_validation) {
      de_log("DE_PIPE:", "Validating splits vs raw gaps...")
      fix_res <- de_fix_yahoo_splits_by_raw_gap(ca_raw, u_raw, cfg)
      ca_apply <- fix_res$corp_actions_apply
      sp_audit <- fix_res$split_audit
      ca_quar <- fix_res$quarantine
    } else {
      ca_apply <- ca_raw
    }
  }

  # Keep CA outputs typed/contract-valid even in no-candidate path
  de_validate_contract(ca_raw, "corp_actions_raw")
  de_validate_contract(ca_apply, "corp_actions_raw")

  # 5) Build Normalized Events
  de_log("DE_PIPE:", "Building normalized events table...")
  ev_apply <- de_build_events(ca_apply, manual_events, cfg)

  # 6) Apply Adjustments & Finalize
  de_log("DE_PIPE:", "Applying factors and creating panel...")
  adj_res <- de_build_panel_adjusted(u_raw, ev_apply, cfg)

  # 7) Diagnostic Features (Optional)
  feat_diag <- NULL
  if (build_diag_features) {
    de_log("DE_PIPE:", "Computing diagnostic features...")
    feat_diag <- de_build_features_diag(adj_res$panel_adj_model, cfg)
  }

  de_log("DE_PIPE:", "Pipeline complete. Returning bundle.")

  list(
    universe_raw = u_raw,
    panel_adj_model = adj_res$panel_adj_model,
    panel_adj_debug = adj_res$panel_adj_debug,
    events_apply = ev_apply,
    corp_actions_raw = ca_raw,
    corp_actions_apply = ca_apply,
    corp_actions_quarantine = ca_quar,
    split_audit = sp_audit,
    prefilter_audit = prefilter_res$prefilter_audit,
    adjustments_timeline = adj_res$adjustments_timeline,
    residual_jump_audit = adj_res$residual_jump_audit,
    features_diag = feat_diag,
    meta = list(
      config = cfg,
      date_run = Sys.time(),
      n_universe = nrow(u_raw),
      n_candidates = length(cand)
    )
  )
}
