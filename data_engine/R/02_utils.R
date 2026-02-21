# /data_engine/R/02_utils.R

`%||%` <- function(x, y) if (!is.null(x)) x else y

de_require <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    stop("Missing required packages for data engine: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}

de_log <- function(prefix, ...) {
  message(paste0(prefix, " ", paste(..., collapse = "")))
}

de_log_cfg <- function(cfg) {
  de_log("DE_CFG:", "\n", paste(utils::capture.output(str(cfg)), collapse = "\n"))
}

de_weekdays_only <- function(dates) {
  # 0 = Sunday, 6 = Saturday in POSIXlt
  w <- as.POSIXlt(dates)$wday
  w %in% 1:5
}

de_make_bizdays_seq <- function(start_date, end_date, cal = "Brazil/B3") {
  start_date <- as.Date(start_date)
  end_date   <- as.Date(end_date)
  
  if (requireNamespace("bizdays", quietly = TRUE)) {
    out <- tryCatch(bizdays::bizseq(start_date, end_date, cal), error = function(e) NULL)
    if (!is.null(out) && length(out)) return(as.Date(out))
  }
  
  d <- seq.Date(start_date, end_date, by = "day")
  d[de_weekdays_only(d)]
}