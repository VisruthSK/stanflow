assign_cmdstanr <- function(funs, entries) {
  for (fun in funs) {
    .stan_citation_funs[[paste0("cmdstanr::", fun)]] <- entries
  }
}

assign_cmdstanr(
  c("lp", "lp_approx"),
  yao2018_vi
)
