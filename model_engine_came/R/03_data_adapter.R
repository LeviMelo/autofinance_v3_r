# 03_data_adapter.R — causal data adapter, strict semantics

came_make_data_adapter <- function(data_bundle_or_panel) {
  came_require("data.table")
  dt <- NULL
  if (is.list(data_bundle_or_panel) && "panel_adj_model" %in% names(data_bundle_or_panel)) {
    dt <- data.table::as.data.table(data_bundle_or_panel$panel_adj_model)
  } else if (is.data.frame(data_bundle_or_panel)) {
    dt <- data.table::as.data.table(data_bundle_or_panel)
  } else {
    stop(came_error("data_input", "Input must be data.frame or list(panel_adj_model)."))
  }

  req <- c("symbol","refdate","open","close")
  miss <- setdiff(req, names(dt))
  came_assert(length(miss) == 0, "data_missing_cols", paste("Missing columns:", paste(miss, collapse=", ")))

  # canonical activity fields
  if (!"traded_value" %in% names(dt) && "turnover" %in% names(dt)) dt[, traded_value := turnover]
  if (!"traded_units" %in% names(dt) && "qty" %in% names(dt)) dt[, traded_units := qty]
  if (!"n_trades" %in% names(dt) && "ntrades" %in% names(dt)) dt[, n_trades := ntrades]

  # enforce Date
  if (!inherits(dt$refdate, "Date")) dt[, refdate := as.Date(refdate)]
  came_assert(!any(is.na(dt$refdate)), "data_refdate", "refdate must be coercible to Date")

  data.table::setkeyv(dt, c("symbol","refdate"))
  data.table::setindexv(dt, c("refdate", "symbol"))
  came_assert(anyDuplicated(dt, by=c("symbol","refdate")) == 0, "data_duplicates", "Duplicate (symbol, refdate) rows")

  if (!"asset_type" %in% names(dt)) dt[, asset_type := "equity"]
  cal_all <- sort(unique(dt$refdate))
  mat_cache <- new.env(parent = emptyenv())
  mat_cache_keys <- character(0)

  adapter <- list()

  adapter$calendar <- function() cal_all

  adapter$panel_upto <- function(as_of_date) dt[refdate <= as.Date(as_of_date)]

  .window_bounds <- function(as_of_date, lookback) {
    d <- as.Date(as_of_date)
    i <- match(d, cal_all)
    came_assert(!is.na(i), "data_asof_not_in_calendar", "as_of_date not in adapter calendar")
    j0 <- max(1L, as.integer(i) - as.integer(lookback) + 1L)
    list(start = cal_all[j0], end = cal_all[i])
  }

  adapter$matrix_field <- function(as_of_date, lookback, field, symbols = NULL, strict = TRUE) {
    sym_key <- if (is.null(symbols)) "*" else paste(sort(unique(as.character(symbols))), collapse = ",")
    k <- paste0(
      as.character(as.Date(as_of_date)), "|", as.integer(lookback), "|", field, "|",
      if (isTRUE(strict)) "1" else "0", "|", sym_key
    )
    if (exists(k, envir = mat_cache, inherits = FALSE)) {
      return(get(k, envir = mat_cache, inherits = FALSE))
    }

    wb <- .window_bounds(as_of_date, lookback)
    sub <- dt[refdate >= wb$start & refdate <= wb$end]
    if (!is.null(symbols)) {
      symbols <- unique(as.character(symbols))
      symbols <- symbols[!is.na(symbols) & nzchar(symbols)]
      sub <- sub[symbol %in% symbols]
    }
    came_assert(field %in% names(sub), "data_field_missing", paste("Field missing:", field))

    mat <- data.table::dcast(sub, refdate ~ symbol, value.var = field)
    out <- as.matrix(mat[, -1, with=FALSE])
    rownames(out) <- as.character(mat$refdate)
    if (strict) {
      keep <- colSums(is.finite(out)) == nrow(out)
      out <- out[, keep, drop=FALSE]
    }
    assign(k, out, envir = mat_cache)
    mat_cache_keys <<- c(mat_cache_keys, k)
    if (length(mat_cache_keys) > 256L) {
      old <- mat_cache_keys[1L]
      if (exists(old, envir = mat_cache, inherits = FALSE)) rm(list = old, envir = mat_cache)
      mat_cache_keys <<- mat_cache_keys[-1L]
    }
    out
  }

  adapter$prices <- function(as_of_date, lookback, symbols = NULL, strict = TRUE) {
    adapter$matrix_field(as_of_date, lookback, "close", symbols, strict)
  }

  adapter$returns <- function(as_of_date, lookback, symbols = NULL, strict = TRUE) {
    P <- adapter$prices(as_of_date, lookback + 1L, symbols, strict)
    came_assert(nrow(P) >= 2, "data_returns", "Not enough prices to compute returns")
    R <- diff(log(pmax(P, 1e-12)))
    R[!is.finite(R)] <- 0
    R
  }

  adapter$activity <- function(as_of_date, lookback, symbols = NULL, strict = FALSE) {
    fields <- c("traded_value","traded_units","n_trades")
    wb <- .window_bounds(as_of_date, lookback)
    sub <- dt[refdate >= wb$start & refdate <= wb$end]
    for (f in fields) {
      came_assert(
        f %in% names(sub), "data_activity_missing",
        paste("Missing required activity field:", f, "(architecture requires traded_value, traded_units, n_trades).")
      )
    }

    cal <- sort(unique(sub$refdate))
    dates <- tail(cal, lookback)
    sub <- sub[refdate %in% dates]
    if (!is.null(symbols)) {
      symbols <- unique(as.character(symbols))
      symbols <- symbols[!is.na(symbols) & nzchar(symbols)]
      sub <- sub[symbol %in% symbols]
    }

    mk <- function(field) {
      mat <- data.table::dcast(sub, refdate ~ symbol, value.var = field)
      out <- as.matrix(mat[, -1, with = FALSE])
      rownames(out) <- as.character(mat$refdate)
      if (strict) {
        keep <- colSums(is.finite(out)) == nrow(out)
        out <- out[, keep, drop = FALSE]
      }
      out
    }

    list(
      traded_value = mk("traded_value"),
      traded_units = mk("traded_units"),
      n_trades = mk("n_trades")
    )
  }

  adapter$investable_universe <- function(as_of_date, spec_data) {
    sub <- adapter$panel_upto(as_of_date)
    cal <- cal_all[cal_all <= as.Date(as_of_date)]
    lkb <- min(63L, length(cal))
    if (lkb < 5) return(character(0))
    wb <- .window_bounds(as_of_date, lkb)
    sub <- dt[refdate >= wb$start & refdate <= wb$end]

    allowed <- spec_data$allowed_types %||% c("equity")
    if ("asset_type" %in% names(sub)) sub <- sub[asset_type %in% allowed]

    # coverage + median traded value + last close
    min_cov <- spec_data$min_coverage_ratio %||% 0.90
    min_tv <- spec_data$min_median_traded_value %||% 1e5

    agg <- sub[, .(
      n_obs = .N,
      med_tv = median(traded_value, na.rm = TRUE),
      last_close = tail(close, 1)
    ), by = symbol]

    agg <- agg[n_obs >= lkb * min_cov & med_tv >= min_tv & is.finite(last_close) & last_close > 0]
    unique(as.character(agg$symbol))
  }

  adapter
}
