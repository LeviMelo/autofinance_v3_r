#' @title Model Data Adapter
#' @description Wrap data bundle/panel securely into no-lookahead structures.

#' @export
me_make_data_adapter <- function(data_bundle_or_panel, aux = list()) {
    me_require("data.table")

    # Extract true panel
    if (is.list(data_bundle_or_panel) && "panel_adj_model" %in% names(data_bundle_or_panel)) {
        dt <- data.table::as.data.table(data_bundle_or_panel$panel_adj_model)
    } else if (is.data.frame(data_bundle_or_panel)) {
        dt <- data.table::as.data.table(data_bundle_or_panel)
    } else {
        stop("data_bundle_or_panel must be a data.frame or a bundle list containing 'panel_adj_model'")
    }

    req_cols <- c("symbol", "refdate", "close", "turnover", "qty", "asset_type")
    missing <- setdiff(req_cols, names(dt))
    if (length(missing) > 0) stop("Panel missing fields: ", paste(missing, collapse = ", "))

    # Canonicalize
    data.table::setkeyv(dt, c("symbol", "refdate"))
    if (anyDuplicated(dt, by = c("symbol", "refdate")) > 0) {
        stop("Panel contains duplicate (symbol, refdate) pairs")
    }

    adapter <- list()

    adapter$calendar <- function() {
        sort(unique(dt$refdate))
    }

    adapter$panel_upto <- function(as_of_date) {
        dt[refdate <= as_of_date]
    }

    adapter$price_matrix <- function(as_of_date, lookback, field = "close", symbols = NULL, strict = TRUE) {
        sub_dt <- adapter$panel_upto(as_of_date)
        if (nrow(sub_dt) == 0) {
            return(matrix(NA_real_, 0, 0))
        }
        cal_upto <- sort(unique(sub_dt$refdate))
        dates_to_keep <- tail(cal_upto, lookback)
        sub_dt <- sub_dt[refdate %in% dates_to_keep]

        if (!is.null(symbols)) sub_dt <- sub_dt[symbol %in% symbols]

        mat <- data.table::dcast(sub_dt, refdate ~ symbol, value.var = field)
        if (nrow(mat) == 0) {
            return(matrix(NA_real_, 0, 0))
        }

        mat_out <- as.matrix(mat[, -1, with = FALSE])
        rownames(mat_out) <- as.character(mat$refdate)

        if (strict) {
            keep <- colSums(!is.na(mat_out)) == nrow(mat_out)
            mat_out <- mat_out[, keep, drop = FALSE]
        }

        # Return with requested symbol order if provided and possible
        if (!is.null(symbols)) {
            found <- intersect(symbols, colnames(mat_out))
            mat_out <- mat_out[, found, drop = FALSE]
        }
        mat_out
    }

    adapter$returns_matrix <- function(as_of_date, lookback, field = "close", method = "log", symbols = NULL, strict = TRUE) {
        pm <- adapter$price_matrix(as_of_date, lookback + 1, field, symbols, strict)
        if (nrow(pm) < 2) {
            return(matrix(NA_real_, nrow = 0, ncol = ncol(pm), dimnames = list(NULL, colnames(pm))))
        }
        if (method == "log") {
            diff(log(pm))
        } else {
            diff(pm) / pm[-nrow(pm), ]
        }
    }

    adapter$execution_price <- function(exec_date, field = "open", symbols = NULL) {
        sub_dt <- dt[refdate == exec_date]
        if (!is.null(symbols)) sub_dt <- sub_dt[symbol %in% symbols]
        res <- sub_dt[[field]]
        names(res) <- sub_dt$symbol
        if (!is.null(symbols)) {
            out <- rep(NA_real_, length(symbols))
            names(out) <- symbols
            found <- intersect(symbols, names(res))
            out[found] <- res[found]
            return(out)
        }
        res
    }

    adapter$investability_snapshot <- function(as_of_date, spec_data) {
        sub_dt <- adapter$panel_upto(as_of_date)
        cal_upto <- sort(unique(sub_dt$refdate))

        # Needs at least 63 days of history to check coverage reasonably, otherwise take available
        lkb <- min(63L, length(cal_upto))
        if (lkb < 5) {
            return(character(0))
        }

        dates_to_keep <- tail(cal_upto, lkb)
        sub_dt <- sub_dt[refdate %in% dates_to_keep]

        min_cov <- spec_data$min_coverage_ratio %||% 0.90
        min_turnover <- spec_data$min_median_turnover %||% 1e5
        allowed_types <- spec_data$allowed_types %||% c("equity")

        # Filter by type first
        if (length(allowed_types) > 0 && "asset_type" %in% names(sub_dt)) {
            sub_dt <- sub_dt[asset_type %in% allowed_types]
        }

        # Aggregate over the lookback
        agg <- sub_dt[, .(
            n_obs = .N,
            med_turnover = median(turnover, na.rm = TRUE),
            last_price = tail(close, 1)
        ), by = symbol]

        # Filter
        agg <- agg[n_obs >= (lkb * min_cov) & med_turnover >= min_turnover & last_price > 0]
        agg$symbol
    }

    adapter
}
