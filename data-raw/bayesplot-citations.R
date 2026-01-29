assign_bayesplot <- function(funs, entries) {
  for (fun in funs) {
    .stan_citation_funs[[paste0("bayesplot::", fun)]] <- entries
  }
}

assign_bayesplot(
  c(
    "mcmc_nuts_acceptance",
    "mcmc_nuts_divergence",
    "mcmc_nuts_stepsize",
    "mcmc_nuts_treedepth",
    "mcmc_nuts_energy"
  ),
  c(betancourt2017, betancourt_girolami2013, hoffman2014_nuts, stan_users_guide)
)

assign_bayesplot(
  c(
    "mcmc_rhat",
    "mcmc_rhat_hist",
    "mcmc_rhat_data",
    "mcmc_neff",
    "mcmc_neff_hist",
    "mcmc_neff_data",
    "mcmc_acf",
    "mcmc_acf_bar"
  ),
  c(stan_users_guide, gelman_rubin1992)
)

assign_bayesplot(
  c("mcmc_parcoord", "mcmc_parcoord_data", "parcoord_style_np"),
  hartikainen2017_divergences
)

assign_bayesplot(
  c(
    "mcmc_trace",
    "mcmc_trace_highlight",
    "trace_style_np",
    "mcmc_rank_overlay",
    "mcmc_rank_hist",
    "mcmc_rank_ecdf",
    "mcmc_trace_data"
  ),
  c(vehtari2019_rhat, sailynoja2021)
)

assign_bayesplot(
  c("ppc_km_overlay", "ppc_km_overlay_grouped"),
  c(BDA, kaplan)
)

assign_bayesplot(
  c("ppc_bars", "ppc_bars_grouped", "ppc_rootogram", "ppc_bars_data"),
  kleiber2016_rootogram
)

assign_bayesplot(
  c(
    "ppc_data",
    "ppc_dens_overlay",
    "ppc_dens_overlay_grouped",
    "ppc_ecdf_overlay",
    "ppc_ecdf_overlay_grouped",
    "ppc_dens",
    "ppc_hist",
    "ppc_freqpoly",
    "ppc_freqpoly_grouped",
    "ppc_boxplot",
    "ppc_dots",
    "ppc_violin_grouped",
    "ppc_pit_ecdf",
    "ppc_pit_ecdf_grouped"
  ),
  c(sailynoja2021, BDA)
)

assign_bayesplot(
  c(
    "ppc_error_hist",
    "ppc_error_hist_grouped",
    "ppc_error_scatter",
    "ppc_error_scatter_avg",
    "ppc_error_scatter_avg_grouped",
    "ppc_error_scatter_avg_vs_x",
    "ppc_error_binned",
    "ppc_error_data",
    "ppc_intervals",
    "ppc_intervals_grouped",
    "ppc_ribbon",
    "ppc_ribbon_grouped",
    "ppc_intervals_data",
    "ppc_ribbon_data",
    "ppc_scatter",
    "ppc_scatter_avg",
    "ppc_scatter_avg_grouped",
    "ppc_scatter_data",
    "ppc_scatter_avg_data",
    "ppc_stat",
    "ppc_stat_grouped",
    "ppc_stat_freqpoly",
    "ppc_stat_freqpoly_grouped",
    "ppc_stat_2d",
    "ppc_stat_data"
  ),
  BDA
)

assign_bayesplot(
  c(
    "ppc_loo_pit_overlay",
    "ppc_loo_pit_data",
    "ppc_loo_pit_qq",
    "ppc_loo_pit_ecdf",
    "ppc_loo_pit",
    "ppc_loo_intervals",
    "ppc_loo_ribbon"
  ),
  c(BDA, vehtari2017_loo, boneva1971_spline)
)
