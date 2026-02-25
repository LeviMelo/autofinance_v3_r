#' @title Backtest Engine — Accounting
#' @description Portfolio state management: positions (shares), cash, NAV, MTM.

# ── Initialize portfolio state ────────────────────────────────────────────────

#' @export
bt_init_portfolio_state <- function(initial_nav, initial_cash = NULL) {
    if (is.null(initial_cash)) initial_cash <- initial_nav
    list(
        as_of_date  = as.Date(NA),
        positions   = setNames(numeric(0), character(0)), # shares by symbol
        cash        = initial_cash,
        nav         = initial_nav,
        mark_prices = setNames(numeric(0), character(0))
    )
}

# ── Apply fills to state ──────────────────────────────────────────────────────

#' @export
bt_apply_fills <- function(state, exec_report) {
    fills <- exec_report$fills
    if (nrow(fills) == 0) {
        return(state)
    }

    pos <- state$positions

    for (i in seq_len(nrow(fills))) {
        sym <- fills$symbol[i]
        delta <- fills$shares_filled[i]
        if (is.na(delta) || !is.finite(delta)) next

        if (sym %in% names(pos)) {
            pos[sym] <- pos[sym] + delta
        } else {
            pos[sym] <- delta
        }
    }

    # Remove zero positions
    pos <- pos[pos != 0]

    # Update cash
    state$cash <- state$cash + exec_report$cash_delta
    state$positions <- pos
    state
}

# ── Mark-to-market ────────────────────────────────────────────────────────────

#' @export
bt_mark_to_market <- function(state, mark_prices, mark_date) {
    pos <- state$positions
    if (length(pos) == 0) {
        state$nav <- state$cash
        state$as_of_date <- mark_date
        state$mark_prices <- mark_prices
        return(state)
    }

    # Align prices to positions
    syms <- names(pos)
    mp <- rep(NA_real_, length(syms))
    names(mp) <- syms
    found <- intersect(syms, names(mark_prices))
    mp[found] <- mark_prices[found]

    # Use last known price for missing
    if (!is.null(state$mark_prices) && length(state$mark_prices) > 0) {
        still_missing <- syms[is.na(mp)]
        for (s in still_missing) {
            if (s %in% names(state$mark_prices) && is.finite(state$mark_prices[s])) {
                mp[s] <- state$mark_prices[s]
            }
        }
    }

    # Compute NAV
    pos_value <- sum(pos * mp, na.rm = TRUE)
    state$nav <- pos_value + state$cash
    state$as_of_date <- mark_date
    state$mark_prices <- mp
    state
}

# ── Compute current weights ───────────────────────────────────────────────────

#' @export
bt_compute_weights <- function(state) {
    if (state$nav <= 0 || length(state$positions) == 0) {
        return(setNames(numeric(0), character(0)))
    }

    values <- state$positions * state$mark_prices[names(state$positions)]
    values[!is.finite(values)] <- 0
    values / state$nav
}

# ── Compute turnover ──────────────────────────────────────────────────────────

#' @export
bt_compute_turnover <- function(weights_before, weights_after) {
    all_syms <- unique(c(names(weights_before), names(weights_after)))
    if (length(all_syms) == 0) {
        return(0)
    }

    wb <- rep(0, length(all_syms))
    names(wb) <- all_syms
    wa <- rep(0, length(all_syms))
    names(wa) <- all_syms

    wb[names(weights_before)] <- weights_before
    wa[names(weights_after)] <- weights_after

    sum(abs(wa - wb)) / 2
}
