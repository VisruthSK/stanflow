core <- c("bayesplot", "loo", "posterior", "projpred", "shinystan")
backends <- c("brms", "cmdstanr", "rstan", "rstanarm")
stanflow_pkgs <- c(core, backends)
if (!exists(".stan_citation_pkgs", inherits = FALSE)) {
  .stan_citation_pkgs <- new.env(parent = emptyenv())
}
if (!exists(".stan_citation_funs", inherits = FALSE)) {
  .stan_citation_funs <- new.env(parent = .stan_citation_pkgs)
}
