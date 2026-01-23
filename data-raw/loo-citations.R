assign_loo <- function(funs, entries) {
  for (fun in funs) {
    .stan_citation_funs[[paste0("loo::", fun)]] <- entries
  }
}

assign_loo(
  c(
    "compare",
    "pareto_k_table",
    "pareto_k_ids",
    "pareto_k_values",
    "pareto_k_influence_values",
    "psis_n_eff_values",
    "mcse_loo",
    "plot.psis_loo",
    "plot.loo",
    "plot.psis",
    "psis_approximate_posterior",
    "psis",
    "psislw",
    "sis",
    "loo"
  ),
  c(vehtari2017_loo, vehtari2024_psis)
)

assign_loo(
  c("loo_compare"),
  c(vehtari2017_loo, vehtari2024_psis, sivula2022_uncertainty, mclatchie2023_bias)
)

assign_loo(
  c("loo_model_weights"),
  c(vehtari2017_loo, vehtari2024_psis, yao2018_stacking)
)

assign_loo(
  c("loo_approximate_posterior", "loo_subsample"),
  c(magnusson2019_large_data, magnusson2020_large_data)
)

assign_loo(
  c("loo_moment_match", "loo_moment_match.default", "loo_moment_match_split"),
  paananen2021_moment_matching
)

assign_loo(
  c("extract_log_lik"),
  c(stan_cpp_2017, rstan_2017)
)

assign_loo(
  c("crps", "scrps", "loo_crps", "loo_scrps"),
  c(bolin2023_scoring, gneiting2007_scoring)
)

assign_loo(
  c("tis"),
  ionides2008_tis
)

assign_loo(
  c("gpdfit"),
  zhang2009_gpd
)

assign_loo(
  c("waic"),
  c(watanabe2010_waic, vehtari2017_loo, vehtari2024_psis)
)
