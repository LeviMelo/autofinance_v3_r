#' @title Backtest Accounting
#' @description Portfolio state transitions, NAV, PnL, turnover, cash.

#' @export
bt_init_portfolio_state <- function(initial_nav = 1.0, initial_cash = 1.0) {
    list(
        nav = initial_nav,
        cash = initial_cash,
        positions = data.frame(symbol = character(0), qty = numeric(0), price = numeric(0), stringsAsFactors = FALSE)
    )
}

#' @export
bt_update_after_execution <- function(state, orders, exec_prices, costs) {
    new_nav <- state$nav

    pos <- state$positions

    if (nrow(orders) > 0) {
        # Calculate realized cash flows from trades
        trade_cashflow <- -sum(orders$trade_notional, na.rm = TRUE)

        # Update positions
        # We rebuild the position table completely
        syms_all <- unique(c(pos$symbol, orders$symbol))

        new_qty <- rep(0, length(syms_all))
        names(new_qty) <- syms_all
        new_qty[pos$symbol] <- pos$qty

        # Apply trades
        for (i in seq_len(nrow(orders))) {
            s <- orders$symbol[i]
            new_qty[s] <- orders$tgt_qty[i]
        }

        # Filter out empty
        keep <- new_qty > 0
        syms_keep <- syms_all[keep]
        new_qty <- new_qty[keep]

        prices <- rep(0, length(syms_keep))
        names(prices) <- syms_keep
        prices[intersect(syms_keep, orders$symbol)] <- orders$price[match(intersect(syms_keep, orders$symbol), orders$symbol)]

        # Inherit old price for unaffected assets
        unaffected <- setdiff(syms_keep, orders$symbol)
        if (length(unaffected) > 0) {
            prices[unaffected] <- pos$price[match(unaffected, pos$symbol)]
        }

        new_positions <- data.frame(
            symbol = syms_keep,
            qty = unname(new_qty),
            price = unname(prices),
            stringsAsFactors = FALSE
        )

        new_cash <- state$cash + trade_cashflow - costs
        notional <- sum(new_positions$qty * new_positions$price, na.rm = TRUE)
        new_nav <- notional + new_cash
    } else {
        new_positions <- state$positions
        new_cash <- state$cash - costs
        new_nav <- state$nav - costs
    }

    list(
        nav = new_nav,
        cash = new_cash,
        positions = new_positions
    )
}

#' @export
bt_mark_to_market <- function(state, mark_prices, date) {
    if (nrow(state$positions) == 0) {
        return(state)
    }
    p <- mark_prices[state$positions$symbol]

    # Keep prior price if no mark price available today
    p[is.na(p)] <- state$positions$price[is.na(p)]

    state$positions$price <- p
    state$nav <- sum(state$positions$qty * p, na.rm = TRUE) + state$cash
    state
}

#' @export
bt_compute_turnover <- function(pre_weights, post_target_weights) {
    # Sum of absolute difference in weights
    all_syms <- unique(c(names(pre_weights), post_target_weights$symbol))
    w0 <- rep(0, length(all_syms))
    names(w0) <- all_syms
    w1 <- rep(0, length(all_syms))
    names(w1) <- all_syms

    w0[names(pre_weights)] <- pre_weights
    w1[post_target_weights$symbol] <- post_target_weights$weight_target

    sum(abs(w1 - w0)) / 2.0
}

#' @export
bt_position_weights <- function(state, mark_prices) {
    if (nrow(state$positions) == 0) {
        return(numeric(0))
    }
    p <- mark_prices[state$positions$symbol]
    p[is.na(p)] <- state$positions$price[is.na(p)]
    w <- (state$positions$qty * p) / state$nav
    names(w) <- state$positions$symbol
    w
}
