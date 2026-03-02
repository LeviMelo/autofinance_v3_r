# backtest_engine/R/04_accounting.R
# Backtest Engine v3 — strict accounting (no last-price carry).

#' @export
bt_init_portfolio_state <- function(initial_nav, initial_cash = NULL) {
    if (is.null(initial_cash)) initial_cash <- initial_nav
    if (!is.finite(initial_nav) || initial_nav <= 0) .bt_stop("init_nav", "initial_nav must be > 0")
    if (!is.finite(initial_cash) || initial_cash < 0) .bt_stop("init_cash", "initial_cash must be >= 0")

    list(
        as_of_date = as.Date(NA),
        positions = setNames(numeric(0), character(0)), # shares
        cash = initial_cash,
        nav = initial_nav,
        last_mark_prices = setNames(numeric(0), character(0))
    )
}

#' @export
bt_apply_fills <- function(state, exec_report) {
    fills <- exec_report$fills
    if (!is.data.frame(fills)) .bt_stop("fills_type", "exec_report$fills must be data.frame")

    pos <- state$positions %||% setNames(numeric(0), character(0))

    if (nrow(fills) > 0) {
        for (i in seq_len(nrow(fills))) {
            sym <- as.character(fills$symbol[i])
            delta <- as.numeric(fills$shares_filled[i])
            if (is.na(sym) || !nzchar(sym)) .bt_stop("fill_symbol", "Invalid fill symbol")
            if (!is.finite(delta)) .bt_stop("fill_delta", paste("Non-finite shares_filled for", sym))
            pos[sym] <- .bt_scalar_or_default(pos[sym], 0) + delta
        }
        pos <- pos[is.finite(pos) & abs(pos) > 0]
        if (length(pos) > 0) {
            bad_names <- is.na(names(pos)) | !nzchar(names(pos))
            if (any(bad_names)) .bt_stop("fill_symbol", "Position state contains invalid symbol names after fills")
            pos <- pos[!bad_names]
        }
    }

    cd <- as.numeric(exec_report$cash_delta)
    if (!is.finite(cd)) .bt_stop("cash_delta", "cash_delta invalid")
    state$cash <- state$cash + cd
    if (!is.finite(state$cash)) .bt_stop("cash_invalid", "cash became invalid")
    if (state$cash < -1e-6) .bt_stop("cash_negative", sprintf("cash < 0 after fills (%.6f)", state$cash))

    state$positions <- pos
    state
}

#' @export
bt_mark_to_market <- function(state, mark_prices, mark_date) {
    d <- as.Date(mark_date)
    if (is.na(d)) .bt_stop("mtm_date", "Invalid mark_date")

    pos <- state$positions %||% setNames(numeric(0), character(0))
    if (length(pos) > 0) {
        bad_names <- is.na(names(pos)) | !nzchar(names(pos))
        if (any(bad_names)) .bt_stop("mtm_symbol_invalid", "Position state contains invalid symbols")
        if (any(!is.finite(as.numeric(pos)))) .bt_stop("mtm_position_invalid", "Position state contains non-finite share quantities")
    }
    if (length(pos) == 0) {
        state$nav <- state$cash
        state$as_of_date <- d
        state$last_mark_prices <- setNames(numeric(0), character(0))
        if (!is.finite(state$nav) || state$nav <= 0) .bt_stop("nav_invalid", "NAV invalid in cash-only state")
        return(state)
    }

    syms <- names(pos)
    px <- mark_prices[syms]
    if (any(!is.finite(px) | px <= 0)) {
        bad <- syms[!is.finite(px) | px <= 0]
        .bt_stop("mtm_price_missing", paste("Missing/invalid mark prices on", as.character(d), "for:", paste(bad, collapse = ", ")))
    }

    nav <- sum(as.numeric(pos) * as.numeric(px)) + state$cash
    if (!is.finite(nav) || nav <= 0) .bt_stop("nav_invalid", paste("NAV invalid on", as.character(d)))
    state$nav <- nav
    state$as_of_date <- d
    state$last_mark_prices <- setNames(as.numeric(px), syms)
    state
}

#' @export
bt_compute_weights <- function(state, mark_prices = NULL) {
    pos <- state$positions %||% setNames(numeric(0), character(0))
    if (length(pos) > 0) {
        bad_names <- is.na(names(pos)) | !nzchar(names(pos))
        if (any(bad_names)) .bt_stop("weights_symbol_invalid", "Position state contains invalid symbols")
    }
    if (length(pos) == 0 || !is.finite(state$nav) || state$nav <= 0) {
        return(setNames(numeric(0), character(0)))
    }
    if (is.null(mark_prices)) {
        mark_prices <- state$last_mark_prices %||% NULL
        if (is.null(mark_prices) || length(mark_prices) == 0) return(setNames(numeric(0), character(0)))
    }

    syms <- names(pos)
    px <- mark_prices[syms]
    if (any(!is.finite(px) | px <= 0)) .bt_stop("weights_price_missing", "Missing mark prices for weight computation")
    v <- as.numeric(pos) * as.numeric(px)
    w <- v / state$nav
    names(w) <- syms
    w
}

#' @export
bt_compute_turnover <- function(w_before, w_after) {
    syms <- unique(c(names(w_before) %||% character(0), names(w_after) %||% character(0)))
    if (length(syms) == 0) {
        return(0)
    }

    wb <- setNames(rep(0, length(syms)), syms)
    wa <- setNames(rep(0, length(syms)), syms)

    if (length(w_before) > 0) wb[names(w_before)] <- as.numeric(w_before)
    if (length(w_after) > 0) wa[names(w_after)] <- as.numeric(w_after)

    sum(abs(wa - wb)) / 2
}
