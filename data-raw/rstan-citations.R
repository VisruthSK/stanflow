assign_rstan <- function(funs, entries) {
  for (fun in funs) {
    .stan_citation_funs[[paste0("rstan::", fun)]] <- entries
  }
}

assign_rstan(
  c(
    "ess_bulk",
    "ess_tail",
    "stan_ess",
    "stan_mcse",
    "Rhat",
    "stan_rhat",
    "monitor"
  ),
  vehtari2019_rhat
)
