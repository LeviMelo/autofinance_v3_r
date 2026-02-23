# /data_engine/R/03_providers.R

# --- RB3 Provider ---
de_rb3_init <- function(cfg) {
  de_require("rb3")
  cache_dir <- file.path(cfg$cache_dir, "rb3")
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
  options(rb3.cachedir = normalizePath(cache_dir, winslash = "/", mustWork = FALSE))
  try(rb3::rb3_bootstrap(), silent = TRUE)
  invisible(TRUE)
}

de_rb3_fetch_yearly_lazy <- function(year, cfg, verbose = TRUE) {
  de_rb3_init(cfg)

  ok <- tryCatch(
    {
      rb3::fetch_marketdata("b3-cotahist-yearly", year = year, throttle = TRUE)
      TRUE
    },
    error = function(e) {
      de_log("DE_RB3:", "fetch_marketdata yearly failed for ", year, ": ", conditionMessage(e))
      FALSE
    }
  )

  if (!ok) {
    stop("rb3 yearly fetch failed for year=", year,
      ". Current engine expects rb3::fetch_marketdata() API.",
      call. = FALSE
    )
  }

  df <- rb3::cotahist_get("yearly")
  start_y <- as.Date(paste0(year, "-01-01"))
  end_y <- as.Date(paste0(year + 1L, "-01-01"))
  dplyr::filter(df, refdate >= start_y, refdate < end_y)
}

de_rb3_fetch_window_lazy <- function(start_date, end_date, cfg, verbose = TRUE) {
  de_rb3_init(cfg)
  dates <- de_make_bizdays_seq(start_date, end_date)

  ok <- tryCatch(
    {
      rb3::fetch_marketdata("b3-cotahist-daily", refdate = dates, throttle = TRUE)
      TRUE
    },
    error = function(e) {
      de_log("DE_RB3:", "fetch_marketdata daily window failed: ", conditionMessage(e))
      FALSE
    }
  )

  if (!ok) {
    stop("rb3 daily window fetch failed. Current engine expects rb3::fetch_marketdata() API.", call. = FALSE)
  }

  df <- rb3::cotahist_get("daily")
  dplyr::filter(df, refdate >= as.Date(start_date), refdate <= as.Date(end_date))
}

# --- Yahoo Provider ---
de_yahoo_symbol <- function(symbol) {
  s <- toupper(trimws(as.character(symbol)))
  if (is.na(s) || !nzchar(s)) {
    return(NA_character_)
  }
  if (grepl("\\.", s)) {
    return(s)
  }
  paste0(s, ".SA")
}

de_yahoo_with_retry <- function(fun, max_tries = 4L, base_sleep = 1.5, verbose = FALSE) {
  for (k in seq_len(max_tries)) {
    out <- tryCatch(fun(), error = function(e) e)
    if (!inherits(out, "error")) {
      return(out)
    }
    if (!grepl("429|Too Many Requests|rate limit", conditionMessage(out), ignore.case = TRUE)) {
      return(NULL)
    }
    if (k < max_tries) Sys.sleep(base_sleep * (2^(k - 1)) * runif(1, 0.8, 1.2))
  }
  NULL
}

de_yahoo_fetch_chart_events_one <- function(yahoo_symbol, from, to, verbose = FALSE) {
  if (!requireNamespace("data.table", quietly = TRUE)) stop("data.table required")
  empty_dt <- data.table::data.table(yahoo_symbol = character(), refdate = as.Date(character()), action_type = character(), value = numeric(), source = character())
  if (is.na(yahoo_symbol)) {
    return(empty_dt)
  }

  if (!requireNamespace("jsonlite", quietly = TRUE) || !requireNamespace("curl", quietly = TRUE)) {
    return(empty_dt)
  }

  p1 <- as.integer(as.POSIXct(as.Date(from), tz = "UTC"))
  p2 <- as.integer(as.POSIXct(as.Date(to) + 1, tz = "UTC"))
  url <- sprintf("https://query1.finance.yahoo.com/v8/finance/chart/%s?period1=%d&period2=%d&interval=1d&events=div%%7Csplits", utils::URLencode(yahoo_symbol, reserved = TRUE), p1, p2)

  h <- curl::new_handle()
  curl::handle_setheaders(h, "User-Agent" = "Mozilla/5.0", "Accept" = "application/json")

  fetch_once <- function() {
    resp <- curl::curl_fetch_memory(url, handle = h)
    if (resp$status_code == 429) stop("HTTP 429", call. = FALSE)
    if (resp$status_code == 404) {
      return(NULL)
    }
    if (resp$status_code >= 400) stop("HTTP ", resp$status_code, call. = FALSE)
    tryCatch(jsonlite::fromJSON(rawToChar(resp$content), simplifyVector = FALSE), error = function(e) NULL)
  }

  js <- de_yahoo_with_retry(fetch_once, max_tries = 4L)
  if (is.null(js) || !is.list(js)) {
    return(empty_dt)
  }

  res1 <- tryCatch(js$chart$result[[1]], error = function(e) NULL)
  ev <- tryCatch(res1$events, error = function(e) NULL)
  if (is.null(ev)) {
    return(empty_dt)
  }
  out_list <- list()

  if (!is.null(ev$dividends)) {
    out_list[["dividend"]] <- data.table::rbindlist(lapply(ev$dividends, function(d) {
      if (is.null(d$date) || is.null(d$amount)) {
        return(NULL)
      }
      data.table::data.table(yahoo_symbol = yahoo_symbol, refdate = as.Date(as.POSIXct(as.integer(d$date), origin = "1970-01-01", tz = "UTC")), action_type = "dividend", value = as.numeric(d$amount), source = "yahoo")
    }), fill = TRUE)
  }

  if (!is.null(ev$splits)) {
    out_list[["split"]] <- data.table::rbindlist(lapply(ev$splits, function(s) {
      if (is.null(s$date) || is.null(s$numerator) || is.null(s$denominator)) {
        return(NULL)
      }
      pf <- as.numeric(s$denominator) / as.numeric(s$numerator) # Price Factor orientation
      data.table::data.table(yahoo_symbol = yahoo_symbol, refdate = as.Date(as.POSIXct(as.integer(s$date), origin = "1970-01-01", tz = "UTC")), action_type = "split", value = pf, source = "yahoo")
    }), fill = TRUE)
  }

  out <- data.table::rbindlist(out_list, fill = TRUE)
  if (nrow(out) > 0) {
    return(out[is.finite(value) & value > 0 & !is.na(refdate)])
  }
  empty_dt
}

de_yahoo_fetch_splits_quantmod_one <- function(ysym, from, to) {
  de_require("quantmod")
  x <- de_yahoo_with_retry(function() quantmod::getSplits(ysym, from = from, to = to, auto.assign = FALSE))
  if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(as.numeric(x)))) {
    return(NULL)
  }

  # Note: quantmod getSplits already returns the price factor convention (e.g., 0.5 for 2:1)
  val <- as.numeric(zoo::coredata(x))
  dt <- data.table::data.table(yahoo_symbol = ysym, refdate = as.Date(zoo::index(x)), action_type = "split", value = val, source = "yahoo")
  dt[is.finite(value) & value > 0] # Ensure safe domain
}

de_yahoo_fetch_dividends_quantmod_one <- function(ysym, from, to) {
  de_require("quantmod")
  x <- de_yahoo_with_retry(function() quantmod::getDividends(ysym, from = from, to = to, auto.assign = FALSE, split.adjust = TRUE))
  if (is.null(x) || length(x) == 0) {
    return(NULL)
  }
  dt <- data.table::data.table(yahoo_symbol = ysym, refdate = as.Date(zoo::index(x)), action_type = "dividend", value = as.numeric(zoo::coredata(x)), source = "yahoo")
  dt[is.finite(value) & value >= 0]
}
