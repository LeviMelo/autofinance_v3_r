#' @title Model Engine — Data Adapter
#' @description No-lookahead data access layer wrapping panel_adj_model.

#' @export
me_make_data_adapter <- function(data_bundle_or_panel, aux = list()) {
    me_require("data.table")

    # ── Extract panel ──
    if (is.list(data_bundle_or_panel) && "panel_adj_model" %in% names(data_bundle_or_panel)) {
        dt <- data.table::as.data.table(data_bundle_or_panel$panel_adj_model)
    } else if (is.data.frame(data_bundle_or_panel)) {
        dt <- data.table::as.data.table(data_bundle_or_panel)
    } else {
        stop("Input must be a data.frame or a bundle list containing 'panel_adj_model'")
    }

    # ── Validate required columns ──
    # Canonical: traded_value, traded_units, n_trades
    # Legacy aliases: turnover (= traded_value), qty (= traded_units)
    req_cols <- c("symbol", "refdate", "open", "close")
    missing <- setdiff(req_cols, names(dt))
    if (length(missing) > 0) {
        stop("Panel missing required columns: ", paste(missing, collapse = ", "))
    }

    # Ensure activity columns exist (prefer canonical, fallback to legacy)
    if (!"traded_value" %in% names(dt) && "turnover" %in% names(dt)) {
        dt[, traded_value := turnover]
    }
    if (!"traded_units" %in% names(dt) && "qty" %in% names(dt)) {
        dt[, traded_units := qty]
    }
    # Create legacy aliases if they don't exist
    if (!"turnover" %in% names(dt) && "traded_value" %in% names(dt)) {
        dt[, turnover := traded_value]
    }
    if (!"qty" %in% names(dt) && "traded_units" %in% names(dt)) {
        dt[, qty := traded_units]
    }

    # ── Canonical date type ──
    if (!inherits(dt$refdate, "Date")) {
        dt[, refdate := as.Date(refdate)]
        if (any(is.na(dt$refdate))) {
            stop("refdate must be coercible to Date")
        }
    }

    # ── Set key and dedupe check ──
    data.table::setkeyv(dt, c("symbol", "refdate"))
    if (anyDuplicated(dt, by = c("symbol", "refdate")) > 0) {
        stop("Panel contains duplicate (symbol, refdate) pairs")
    }

    # ── Default asset_type if missing ──
    if (!"asset_type" %in% names(dt)) dt[, asset_type := "equity"]

    # ══════════════════════════════════════════════════════════════════════════
    # Build adapter (closure-based interface)
    # ══════════════════════════════════════════════════════════════════════════
    adapter <- list()

    # ── Calendar ──
    adapter$calendar <- function() sort(unique(dt$refdate))

    # ── Panel up to date (strict no-lookahead) ──
    adapter$panel_upto <- function(as_of_date) dt[refdate <= as_of_date]

    # ── Price matrix ──
    adapter$price_matrix <- function(as_of_date, lookback, field = "close",
                                     symbols = NULL, strict = TRUE) {
        sub <- adapter$panel_upto(as_of_date)
        if (nrow(sub) == 0) {
            return(matrix(NA_real_, 0, 0))
        }

        cal <- sort(unique(sub$refdate))
        dates <- tail(cal, lookback)
        sub <- sub[refdate %in% dates]
        if (!is.null(symbols)) sub <- sub[symbol %in% symbols]

        mat <- data.table::dcast(sub, refdate ~ symbol, value.var = field)
        if (nrow(mat) == 0) {
            return(matrix(NA_real_, 0, 0))
        }

        out <- as.matrix(mat[, -1, with = FALSE])
        rownames(out) <- as.character(mat$refdate)

        if (strict) {
            keep <- colSums(!is.na(out)) == nrow(out)
            out <- out[, keep, drop = FALSE]
        }
        if (!is.null(symbols)) {
            found <- intersect(symbols, colnames(out))
            out <- out[, found, drop = FALSE]
        }
        out
    }

    # ── Returns matrix ──
    adapter$returns_matrix <- function(as_of_date, lookback, field = "close",
                                       method = "log", symbols = NULL,
                                       strict = TRUE) {
        pm <- adapter$price_matrix(as_of_date, lookback + 1, field, symbols, strict)
        if (nrow(pm) < 2) {
            return(matrix(NA_real_, 0, ncol(pm),
                dimnames = list(NULL, colnames(pm))
            ))
        }
        if (method == "log") diff(log(pm)) else diff(pm) / pm[-nrow(pm), ]
    }

    # ── Single-day cross-sectional return ──
    adapter$return_1d <- function(as_of_date, symbols = NULL) {
        pm <- adapter$price_matrix(as_of_date, 2, "close", symbols, strict = FALSE)
        if (nrow(pm) < 2) {
            v <- rep(NA_real_, max(1, ncol(pm)))
            if (ncol(pm) > 0) names(v) <- colnames(pm)
            return(v)
        }
        r <- log(pm[2, ] / pm[1, ])
        r[!is.finite(r)] <- NA_real_
        r
    }

    # ── Execution prices ──
    adapter$execution_price <- function(exec_date, field = "open", symbols = NULL) {
        sub <- dt[refdate == exec_date]
        if (!is.null(symbols)) sub <- sub[symbol %in% symbols]
        res <- sub[[field]]
        names(res) <- sub$symbol
        if (!is.null(symbols)) {
            out <- rep(NA_real_, length(symbols))
            names(out) <- symbols
            found <- intersect(symbols, names(res))
            out[found] <- res[found]
            return(out)
        }
        res
    }

    # ── Investability snapshot ──
    adapter$investability_snapshot <- function(as_of_date, spec_data) {
        sub <- adapter$panel_upto(as_of_date)
        cal <- sort(unique(sub$refdate))

        lkb <- min(63L, length(cal))
        if (lkb < 5) {
            return(character(0))
        }

        dates <- tail(cal, lkb)
        sub <- sub[refdate %in% dates]

        min_cov <- spec_data$min_coverage_ratio %||% 0.90
        min_turnover <- spec_data$min_median_turnover %||% 1e5
        allowed <- spec_data$allowed_types %||% c("equity")

        # Filter by asset type
        if (length(allowed) > 0 && "asset_type" %in% names(sub)) {
            sub <- sub[asset_type %in% allowed]
        }

        # Use canonical traded_value (with turnover fallback)
        tv_col <- if ("traded_value" %in% names(sub)) "traded_value" else "turnover"
        if (!tv_col %in% names(sub)) {
            return(character(0))
        }

        agg <- sub[, .(
            n_obs        = .N,
            med_turnover = median(get(tv_col), na.rm = TRUE),
            last_price   = tail(close, 1)
        ), by = symbol]

        agg <- agg[n_obs >= (lkb * min_cov) &
            med_turnover >= min_turnover &
            last_price > 0]
        agg$symbol
    }

    adapter
}
