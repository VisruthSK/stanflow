assign_posterior <- function(funs, entries) {
  for (fun in funs) {
    .stan_citation_funs[[paste0("posterior::", fun)]] <- entries
  }
}

assign_posterior(
  c("dissent", "print.rvar"),
  tastle2007_dissent
)

assign_posterior(
  "entropy",
  wilcox1967_variation
)

assign_posterior(
  "example_draws",
  BDA
)

assign_posterior(
  c("ess_mean", "mcse_mean"),
  BDA
)

assign_posterior(
  "rhat_basic",
  BDA
)

assign_posterior(
  "ess_basic",
  c(BDA, vehtari2021_ess_comparison)
)

assign_posterior(
  c("ess_bulk", "ess_tail"),
  vehtari2021_ess_comparison
)

assign_posterior(
  "mcse_sd",
  kenney1951_stats
)

assign_posterior(
  c(
    "pareto_diags",
    "pareto_khat",
    "pareto_smooth",
    "ps_tail",
    "pareto_khat_threshold",
    "pareto_min_ss",
    "pareto_convergence_rate"
  ),
  vehtari2024_psis
)

assign_posterior(
  "gpdfit",
  zhang2009_gpd
)

assign_posterior(
  "resample_draws",
  kitagawa1996_mc_filter
)

assign_posterior(
  "rhat_nested",
  margossian2023_nested_rhat
)

assign_posterior(
  "rstar",
  lambert2020_rstar
)

assign_posterior(
  "thin_draws",
  sailynoja2022_uniformity
)
