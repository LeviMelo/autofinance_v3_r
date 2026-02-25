# 99_smoke_tests.R — minimal invariant checks (not a full test suite)

came_smoke_check_snapshot <- function(snapshot) {
  came_snapshot_validate(snapshot)

  # symmetry checks
  S <- snapshot$risk$Sigma_H
  came_assert(max(abs(S - t(S))) < 1e-6, "smoke_sigma_sym", "Sigma_H not symmetric")
  # PSD check (allow tiny negatives)
  ev <- eigen(S, symmetric=TRUE, only.values=TRUE)$values
  came_assert(min(ev) > -1e-6, "smoke_sigma_psd", "Sigma_H not PSD")

  # graph checks
  M <- snapshot$structure$M
  came_assert(is.matrix(M) && max(abs(M - t(M))) == 0, "smoke_graph_sym", "Graph mask not symmetric")
  came_assert(all(diag(M) == 0), "smoke_graph_diag", "Graph mask diagonal not zero")

  # gating
  pi <- snapshot$forecast$pi
  came_assert(abs(sum(pi) - 1.0) < 1e-6, "smoke_pi", "pi does not sum to 1")

  invisible(TRUE)
}
