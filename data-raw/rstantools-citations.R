assign_rstantools <- function(funs, entries) {
  for (fun in funs) {
    .stan_citation_funs[[paste0("rstantools::", fun)]] <- entries
  }
}

assign_rstantools(
  "bayes_R2",
  gelman2019_bayes_r2
)

assign_rstantools(
  c(
    "loo_linpred",
    "loo_epred",
    "loo_predict",
    "loo_predictive_interval",
    "loo_pit"
  ),
  czado2009_pit
)
