# 02_state.R — universe mapping Π_t and state initialization

came_pi_vector <- function(v, new_univ, init_val = 0) {
  out <- setNames(rep(init_val, length(new_univ)), new_univ)
  if (!is.null(v) && length(v) > 0) {
    common <- intersect(names(v), new_univ)
    if (length(common) > 0) out[common] <- v[common]
  }
  out
}

came_pi_matrix <- function(M, new_univ, init_diag = 1e-4) {
  p <- length(new_univ)
  out <- matrix(0, p, p, dimnames = list(new_univ, new_univ))
  if (!is.null(M) && is.matrix(M)) {
    rn <- rownames(M) %||% colnames(M)
    if (!is.null(rn)) {
      common <- intersect(rn, new_univ)
      if (length(common) > 0) out[common, common] <- M[common, common, drop = FALSE]
    }
  }
  diag(out) <- ifelse(diag(out) == 0, init_diag, diag(out))
  came_symmetrize(out)
}

came_pi_list <- function(lst, new_univ, init_fn = function() NULL) {
  out <- setNames(vector("list", length(new_univ)), new_univ)
  if (is.null(lst) || !is.list(lst)) {
    for (nm in new_univ) out[[nm]] <- init_fn()
    return(out)
  }
  common <- intersect(names(lst), new_univ)
  for (nm in common) out[[nm]] <- lst[[nm]]
  for (nm in setdiff(new_univ, common)) out[[nm]] <- init_fn()
  out
}

came_state_init <- function() {
  list(
    risk = list(sigma2 = NULL, Sigma_f = NULL, S_e = NULL, B_prev = NULL),
    structure = list(P_bar = NULL, M_prev = NULL, edge_stab = NULL, labels = NULL),
    signals = list(kalman = NULL, omega = NULL),
    forecast = list(models = NULL, error_buckets = NULL, hist = NULL, step = 0L),
    history = list(schema = "came_history_v1", snapshots = list()),
    prev_target = NULL
  )
}

came_history_append <- function(state, as_of_date, X, keep = 300L) {
  st <- state
  if (is.null(st$history) || !is.list(st$history$snapshots)) st$history <- list(schema="came_history_v1", snapshots=list())
  snaps <- st$history$snapshots
  snaps[[length(snaps) + 1L]] <- list(date = as.Date(as_of_date), X = X)

  # dedupe by date, keep last
  dates <- vapply(snaps, function(s) as.character(s$date), character(1))
  if (anyDuplicated(dates)) {
    keep_idx <- !duplicated(dates, fromLast = TRUE)
    snaps <- snaps[keep_idx]
  }
  if (length(snaps) > keep) snaps <- tail(snaps, keep)
  st$history$snapshots <- snaps
  st
}

came_extract_prev_weights <- function(prev_target_df) {
  if (is.null(prev_target_df)) return(NULL)
  if (is.data.frame(prev_target_df) && all(c("symbol","weight_target") %in% names(prev_target_df))) {
    w <- setNames(prev_target_df$weight_target, prev_target_df$symbol)
    w[!is.finite(w)] <- 0
    return(w)
  }
  NULL
}
