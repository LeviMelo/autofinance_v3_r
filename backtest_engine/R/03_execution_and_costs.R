# backtest_engine/R/03_execution_and_costs.R
# Backtest Engine v3 — strict execution (no missing prices), cash-budget enforced.

#' @export
bt_compute_target_shares <- function(target_weights, nav_basis, exec_prices) {
    if (!is.data.frame(target_weights)) .bt_stop("target_weights_type", "target_weights must be data.frame")
    if (!is.finite(nav_basis) || nav_basis <= 0) .bt_stop("nav_basis", "nav_basis must be finite > 0")

    if (nrow(target_weights) == 0) {
        return(setNames(numeric(0), character(0)))
    }

    syms <- as.character(target_weights$symbol)
    w <- as.numeric(target_weights$weight_target)
    if (any(is.na(syms) | !nzchar(syms))) .bt_stop("target_symbol", "Invalid symbol in target_weights")
    if (anyDuplicated(syms) > 0) .bt_stop("target_symbol_dup", "Duplicate symbols in target_weights")
    if (any(!is.finite(w)) || any(w < 0)) .bt_stop("target_weight", "Invalid weight in target_weights")

    shares <- setNames(rep(0, length(syms)), syms)
    for (s in syms) {
        p <- exec_prices[s]
        if (!is.finite(p) || p <= 0) .bt_stop("exec_price_missing", paste("Missing/invalid exec price for", s))
        notional <- w[syms == s][1] * nav_basis
        shares[s] <- floor(notional / p)
    }
    shares
}

#' @export
bt_generate_orders <- function(current_positions, target_shares) {
    cur_syms <- .bt_clean_symbols(names(current_positions) %||% character(0))
    tgt_syms <- .bt_clean_symbols(names(target_shares) %||% character(0))
    all_syms <- unique(c(cur_syms, tgt_syms))
    if (length(all_syms) == 0) {
        return(data.frame(symbol = character(0), shares_delta = numeric(0), direction = character(0), stringsAsFactors = FALSE))
    }

    cur <- setNames(rep(0, length(all_syms)), all_syms)
    tgt <- setNames(rep(0, length(all_syms)), all_syms)

    if (length(cur_syms) > 0) {
        cur[cur_syms] <- as.numeric(current_positions[cur_syms])
        cur[!is.finite(cur)] <- 0
    }
    if (length(tgt_syms) > 0) {
        tgt[tgt_syms] <- as.numeric(target_shares[tgt_syms])
        tgt[!is.finite(tgt)] <- 0
    }

    delta <- tgt - cur
    active <- which(delta != 0)
    if (length(active) == 0) {
        return(data.frame(symbol = character(0), shares_delta = numeric(0), direction = character(0), stringsAsFactors = FALSE))
    }

    data.frame(
        symbol = all_syms[active],
        shares_delta = as.numeric(delta[active]),
        direction = ifelse(delta[active] > 0, "buy", "sell"),
        stringsAsFactors = FALSE
    )
}

#' @export
bt_compute_costs <- function(fills, spec_costs) {
    fee_rate <- spec_costs$fee_rate %||% 0
    slip_rate <- spec_costs$slippage_rate %||% 0
    if (!is.finite(fee_rate) || fee_rate < 0) .bt_stop("fee_rate", "fee_rate must be >= 0")
    if (!is.finite(slip_rate) || slip_rate < 0) .bt_stop("slip_rate", "slippage_rate must be >= 0")

    if (!is.data.frame(fills) || nrow(fills) == 0) {
        return(list(
            total_cost = 0, fee_total = 0, slippage_total = 0,
            per_symbol = data.frame(symbol = character(0), fee = numeric(0), slippage = numeric(0))
        ))
    }

    abs_notional <- abs(as.numeric(fills$notional))
    abs_notional[!is.finite(abs_notional)] <- 0

    fees <- abs_notional * fee_rate
    slips <- abs_notional * slip_rate

    list(
        total_cost = sum(fees + slips),
        fee_total = sum(fees),
        slippage_total = sum(slips),
        per_symbol = data.frame(symbol = fills$symbol, fee = fees, slippage = slips, stringsAsFactors = FALSE)
    )
}

.bt_price_lookup_strict <- function(exec_prices, syms) {
    if (any(is.na(syms) | !nzchar(syms))) {
        bad <- syms[is.na(syms) | !nzchar(syms)]
        .bt_stop("exec_symbol_invalid", paste("Invalid execution symbol(s):", paste(unique(bad), collapse = ", ")))
    }
    px <- exec_prices[syms]
    if (any(!is.finite(px) | px <= 0)) {
        bad <- syms[!is.finite(px) | px <= 0]
        .bt_stop("exec_price_missing", paste("Missing/invalid exec price for:", paste(unique(bad), collapse = ", ")))
    }
    px
}

#' @export
bt_execute_rebalance <- function(portfolio_state, proposal, exec_prices, spec_costs) {
    # nav basis is mandatory (set by runner at decision time)
    nav_basis <- proposal$meta$nav_basis %||% NULL
    if (is.null(nav_basis) || !is.finite(nav_basis) || nav_basis <= 0) {
        .bt_stop("nav_basis_missing", "proposal$meta$nav_basis is required (runner must set it)")
    }

    # apply locks (optional)
    locked <- proposal$locked_symbols %||% character(0)
    locked <- unique(as.character(locked))
    locked <- locked[!is.na(locked) & nzchar(locked)]
    skipped_symbols <- character(0)

    # compute target shares
    target_shares <- bt_compute_target_shares(proposal$target_weights, nav_basis, exec_prices)

    if (length(locked) > 0) {
        cur <- portfolio_state$positions %||% setNames(numeric(0), character(0))
        for (s in locked) {
            if (s %in% names(cur)) target_shares[s] <- cur[s]
        }
    }

    # build orders
    orders <- bt_generate_orders(portfolio_state$positions, target_shares)
    if (nrow(orders) == 0) {
        return(list(
            orders = orders,
            fills = data.frame(symbol = character(0), shares_filled = numeric(0), fill_price = numeric(0), notional = numeric(0), direction = character(0)),
            costs = bt_compute_costs(data.frame(), spec_costs),
            cash_delta = 0,
            target_shares = target_shares,
            skipped_symbols = skipped_symbols
        ))
    }

    # use available prices only; orders without a valid fill price are skipped
    ord_syms <- as.character(orders$symbol)
    px_all <- exec_prices[ord_syms]
    ok_px <- is.finite(px_all) & px_all > 0

    if (any(!ok_px)) {
        miss_syms <- unique(ord_syms[!ok_px])
        skipped_symbols <- unique(c(skipped_symbols, miss_syms))
        cur <- portfolio_state$positions %||% setNames(numeric(0), character(0))
        for (s in miss_syms) {
            target_shares[s] <- .bt_scalar_or_default(cur[s], 0)
        }

        keep <- ok_px
        orders <- orders[keep, , drop = FALSE]
        px <- as.numeric(px_all[keep])
        names(px) <- ord_syms[keep]
    } else {
        px <- as.numeric(px_all)
        names(px) <- ord_syms
    }

    if (nrow(orders) == 0) {
        return(list(
            orders = orders,
            fills = data.frame(symbol = character(0), shares_filled = numeric(0), fill_price = numeric(0), notional = numeric(0), direction = character(0)),
            costs = bt_compute_costs(data.frame(), spec_costs),
            cash_delta = 0,
            target_shares = target_shares,
            skipped_symbols = skipped_symbols
        ))
    }

    notional <- as.numeric(orders$shares_delta) * px

    fee_rate <- spec_costs$fee_rate %||% 0
    slip_rate <- spec_costs$slippage_rate %||% 0
    cost_rate <- fee_rate + slip_rate

    buy_notional <- sum(pmax(notional, 0), na.rm = TRUE)
    sell_notional <- sum(pmax(-notional, 0), na.rm = TRUE)

    # solve cash constraint for buys: B*(1+c) <= cash + S*(1-c)
    cash0 <- portfolio_state$cash
    if (!is.finite(cash0)) .bt_stop("cash_invalid", "portfolio_state$cash invalid")

    max_buy <- (cash0 + sell_notional * (1 - cost_rate)) / (1 + cost_rate)
    max_buy <- max(0, max_buy)

    if (buy_notional > max_buy + 1e-9) {
        if (buy_notional <= 0) .bt_stop("buy_notional", "Unexpected buy_notional <= 0 in scaling path")
        scale <- max_buy / buy_notional
        scale <- max(0, min(1, scale))

        # scale buy deltas only (integer shares)
        cur <- portfolio_state$positions %||% setNames(numeric(0), character(0))
        for (i in seq_len(nrow(orders))) {
            if (orders$shares_delta[i] > 0) {
                s <- orders$symbol[i]
                cur_pos <- .bt_scalar_or_default(cur[s], 0)
                tgt <- .bt_scalar_or_default(target_shares[s], 0)
                delta <- tgt - cur_pos
                target_shares[s] <- cur_pos + floor(delta * scale)
            }
        }
        orders <- bt_generate_orders(portfolio_state$positions, target_shares)
        if (nrow(orders) > 0) {
            ord_syms <- as.character(orders$symbol)
            px_all <- exec_prices[ord_syms]
            ok_px <- is.finite(px_all) & px_all > 0
            if (any(!ok_px)) {
                miss_syms <- unique(ord_syms[!ok_px])
                skipped_symbols <- unique(c(skipped_symbols, miss_syms))
                cur <- portfolio_state$positions %||% setNames(numeric(0), character(0))
                for (s in miss_syms) {
                    target_shares[s] <- .bt_scalar_or_default(cur[s], 0)
                }
                keep <- ok_px
                orders <- orders[keep, , drop = FALSE]
                px <- as.numeric(px_all[keep])
                names(px) <- ord_syms[keep]
            } else {
                px <- as.numeric(px_all)
                names(px) <- ord_syms
            }
        } else {
            px <- setNames(numeric(0), character(0))
        }
        if (nrow(orders) == 0) {
            return(list(
                orders = orders,
                fills = data.frame(symbol = character(0), shares_filled = numeric(0), fill_price = numeric(0), notional = numeric(0), direction = character(0)),
                costs = bt_compute_costs(data.frame(), spec_costs),
                cash_delta = 0,
                target_shares = target_shares,
                skipped_symbols = skipped_symbols
            ))
        }
        notional <- as.numeric(orders$shares_delta) * px
    }

    fills <- data.frame(
        symbol = orders$symbol,
        shares_filled = as.numeric(orders$shares_delta),
        fill_price = as.numeric(px),
        notional = as.numeric(notional),
        direction = orders$direction,
        stringsAsFactors = FALSE
    )

    costs <- bt_compute_costs(fills, spec_costs)
    cash_delta <- -sum(fills$notional, na.rm = TRUE) - costs$total_cost
    cash_after <- cash0 + cash_delta

    if (!is.finite(cash_after)) .bt_stop("cash_after_invalid", "cash_after invalid")
    if (cash_after < -1e-6) .bt_stop("cash_negative", sprintf("Execution violates cash constraint (cash_after=%.6f)", cash_after))

    list(
        orders = orders,
        fills = fills,
        costs = costs,
        cash_delta = cash_delta,
        target_shares = target_shares,
        skipped_symbols = skipped_symbols
    )
}
