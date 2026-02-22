# /data_engine/R/07_diagnostics_features.R

de_compute_symbol_diag_features <- function(dt_sym, horizons_days) {
  dt <- data.table::as.data.table(dt_sym)
  n_obs <- nrow(dt)
  if (n_obs < 30L) return(NULL)
  
  out <- list(symbol = dt$symbol[1], end_refdate = dt$refdate[n_obs], n_obs = n_obs)
  
  dt[, close_prev := data.table::shift(close, 1L)]
  dt[, ret_cc_log := log(close / close_prev)]
  dt[, ret_cc_simple := (close / close_prev) - 1]
  
  for (h in horizons_days) {
    if (h > 1L && (n_obs - h) >= 1L) {
      out[[paste0("ret_", h, "d")]] <- (dt$close[n_obs] / dt$close[n_obs - h]) - 1
      rwin <- dt$ret_cc_log[max(1L, n_obs - h + 1L):n_obs]
      out[[paste0("vol_cc_", h, "d")]] <- stats::sd(rwin[is.finite(rwin)]) * sqrt(252)
    } else {
      out[[paste0("ret_", h, "d")]] <- NA_real_
      out[[paste0("vol_cc_", h, "d")]] <- NA_real_
    }
  }
  
  # Drawdown
  prices <- dt$close[is.finite(dt$close) & dt$close > 0]
  if (length(prices) > 2) {
    cm <- cummax(prices)
    dd <- prices / cm - 1
    out$max_dd <- min(dd)
    out$ulcer_index <- sqrt(mean((dd * 100)^2))
  } else {
    out$max_dd <- NA_real_
    out$ulcer_index <- NA_real_
  }
  
  # Amihud
  valid <- is.finite(dt$ret_cc_simple) & is.finite(dt$turnover) & dt$turnover > 0
  out$amihud <- if (any(valid)) mean(abs(dt$ret_cc_simple[valid]) / dt$turnover[valid], na.rm = TRUE) else NA_real_
  
  data.table::as.data.table(out)
}

de_build_features_diag <- function(panel_adj_model, cfg) {
  dt <- data.table::as.data.table(panel_adj_model)
  data.table::setorder(dt, symbol, refdate)
  
  if (!nrow(dt)) return(data.table::data.table())
  
  syms <- unique(dt$symbol)
  
  res <- lapply(syms, function(s) {
    sdt <- dt[symbol == s]
    max_req <- max(cfg$feature_horizons_days) + 1L
    if (nrow(sdt) > max_req) sdt <- sdt[(.N - max_req + 1):.N]
    de_compute_symbol_diag_features(sdt, cfg$feature_horizons_days)
  })
  
  out <- data.table::rbindlist(res[!vapply(res, is.null, logical(1))], fill = TRUE)
  if (is.null(out) || !nrow(out)) return(data.table::data.table())
  out
}