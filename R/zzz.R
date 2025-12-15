core <- c("bayesplot", "loo", "posterior", "projpred", "shinystan")
backends <- c("brms", "cmdstanr", "rstan", "rstanarm")
stanflow_pkgs <- c(core, backends)

.onAttach <- function(...) {
  wrapped_startup(core_attach_message())
  wrapped_startup(backends_attach_message())

  conflicts <- stanflow_conflicts()

  if (length(conflicts) > 0) {
    wrapped_startup(stanflow_conflict_message(conflicts))
  }
}
