assign_brms <- function(funs, entries) {
  for (fun in funs) {
    .stan_citation_funs[[paste0("brms::", fun)]] <- entries
  }
}

assign_brms(
  "horseshoe",
  c(carvalho2009_horseshoe, piironen2017_hyperprior, piironen2017_horseshoe)
)

assign_brms(
  "R2D2",
  c(zhang2020_r2d2, aguilar2022_r2d2m2)
)

assign_brms(
  "lasso",
  park2008_lasso
)

assign_brms(
  "brmsfamily",
  kosmidis_zeileis2024_beta
)

assign_brms(
  c("s", "t2"),
  pedersen2019_gam
)

assign_brms(
  "mo",
  burkner_charpentier2020_monotonic
)

assign_brms(
  "bayes_R2",
  gelman2019_bayes_r2
)

assign_brms(
  c("loo", "waic", "loo.brmsfit", "waic.brmsfit"),
  c(vehtari2017_loo, gelman_hwang_vehtari2014, watanabe2010_waic)
)

assign_brms(
  c("loo_moment_match", "loo_moment_match.brmsfit", "loo_moment_match.loo"),
  paananen2021_moment_matching
)

assign_brms(
  c("loo_R2", "loo_R2.brmsfit"),
  vehtari_lampinen2002
)

assign_brms(
  "bayes_R2.brmsfit",
  gelman2019_bayes_r2
)

assign_brms(
  "summary.brmsfit",
  vehtari2021_rhat
)
