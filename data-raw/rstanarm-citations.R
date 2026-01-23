assign_rstanarm <- function(funs, entries) {
  for (fun in funs) {
    .stan_citation_funs[[paste0("rstanarm::", fun)]] <- entries
  }
}

assign_rstanarm(
  c("launch_shinystan.stanreg"),
  muth2018
)

assign_rstanarm(
  c("kfold.stanreg"),
  c(vehtari2017_loo, yao2018_stacking)
)

assign_rstanarm(
  c("loo_predict.stanreg", "loo.stanreg"),
  c(vehtari2017_loo, yao2018_stacking)
)

assign_rstanarm(
  c("pp_check.stanreg"),
  BDA
)

assign_rstanarm(
  c("posterior_interval.stanreg"),
  c(gelman_carlin_2014, morey2016_ci)
)

assign_rstanarm(
  c("stan_glm", "stan_glmer"),
  c(gelman_hill_2007, muth2018)
)

assign_rstanarm(
  c(
    "normal",
    "student_t",
    "cauchy",
    "hs",
    "hs_plus",
    "laplace",
    "lasso",
    "product_normal",
    "exponential",
    "decov",
    "lkj",
    "dirichlet",
    "R2",
    "default_prior_intercept",
    "default_prior_coef"
  ),
  c(BDA, gelman2008_prior, piironen2017_horseshoe, stan_users_guide)
)

assign_rstanarm(
  c("bayes_R2.stanreg", "loo_R2.stanreg"),
  gelman2019_bayes_r2
)

assign_rstanarm(
  c("posterior_survfit"),
  rizopoulos2011
)

assign_rstanarm(
  c("pp_validate"),
  cook2006
)

assign_rstanarm(
  c("stan_betareg"),
  ferrari2004
)

assign_rstanarm(
  c("stan_gamm4"),
  crainiceanu2005
)

assign_rstanarm(
  c("stan_lm"),
  lewandowski2009
)

assign_rstanarm(
  c("stan_polr"),
  nagler1994
)
