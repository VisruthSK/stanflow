assign_rstan <- function(funs, entries) {
  for (fun in funs) {
    .stan_citation_funs[[paste0("rstan::", fun)]] <- entries
  }
}

assign_rstan(
  c(
    "conv_quantile",
    "ess_bulk",
    "ess_mean",
    "ess_quantile",
    "ess_rfun",
    "ess_sd",
    "ess_tail",
    "mcse_mean",
    "mcse_quantile",
    "mcse_sd",
    "monitor",
    "Rhat",
    "rhat_rfun"
  ),
  vehtari2019_rhat
)
