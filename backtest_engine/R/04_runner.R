#' @title Backtest Runner
#' @description The walk-forward simulation loop.

#' @export
bt_run_backtest <- function(data_bundle_or_panel, strategy_fn, strategy_spec, bt_spec = NULL) {
  bt_spec <- bt_get_spec(bt_spec)
  data_ctx <- bt_make_data_context(data_bundle_or_panel)

  cal <- bt_calendar(data_ctx)
  rebal_dates <- bt_rebalance_dates(cal, bt_spec$schedule)

  state <- bt_init_portfolio_state(
    initial_nav = bt_spec$accounting$initial_nav,
    initial_cash = bt_spec$accounting$initial_cash
  )

  nav_history <- list()
  rebal_log <- list()
  target_log <- list()
  fills_log <- list()
  cost_log <- list()

  for (i in seq_along(rebal_dates)) {
    decision_date <- rebal_dates[i]
    exec_date <- bt_next_exec_date(cal, decision_date, bt_spec$execution)
    if (length(exec_date) == 0 || is.na(exec_date)) break

    # 1. Call strategy (must only use data <= decision_date)
    tgt_artifact <- strategy_fn(decision_date, state, data_ctx, strategy_spec)

    # 2. Get exec prices
    exec_prices <- bt_get_exec_prices(data_ctx, exec_date, bt_spec$execution$price_field, tgt_artifact$tradable_symbols)

    # 3. Generate orders and apply costs
    orders <- bt_generate_orders(state$positions, state$nav, tgt_artifact$target_weights, exec_prices, bt_spec$execution)
    costs <- bt_apply_costs(orders, exec_prices, bt_spec$costs)

    # 4. Execute and update state
    exec_res <- bt_execute_rebalance(state, tgt_artifact, exec_prices, bt_spec$execution, bt_spec$costs)
    state <- bt_update_after_execution(state, orders, exec_prices, costs)

    # Logs
    nav_history[[as.character(exec_date)]] <- state$nav
    rebal_log[[as.character(exec_date)]] <- list(decision_date = decision_date, exec_date = exec_date)
    target_log[[as.character(exec_date)]] <- tgt_artifact$target_weights
    fills_log[[as.character(exec_date)]] <- orders
    cost_log[[as.character(exec_date)]] <- costs
  }

  nav_series <- unlist(nav_history)
  if (length(nav_series) > 0) {
    nav_df <- data.frame(date = as.Date(names(nav_series)), nav = unname(nav_series))
  } else {
    nav_df <- data.frame(date = as.Date(character(0)), nav = numeric(0))
  }

  res <- list(
    nav_series = nav_df,
    rebalance_log = rebal_log,
    target_log = target_log,
    fills_log = fills_log,
    cost_log = cost_log
  )

  bt_validate_run_artifact(res)
  res
}
