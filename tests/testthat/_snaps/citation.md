# stan_scan_usage warns about multiple ambiguous calls in strict mode

    couldn't reliably detect which packages these functions are from: `ess_bulk()`, `rhat()`. Please namespace them (`pkg::function()`) and re-run stan_cite().

# print.stan_scan_usage shows functions with no packages

    -- Stan usage --------------------------------------------- stan_scan_usage() --
    Packages: <none>
    Functions (2):
      loo (1): `loo()`
      posterior (1): `as_draws()`

# print.stan_scan_usage shows many packages with no functions

    -- Stan usage --------------------------------------------- stan_scan_usage() --
    Packages (8):
      bayesplot, brms, cmdstanr, loo, posterior, projpred, rstan, shinystan
    Functions: <none>

# print.stan_scan_usage shows many functions for one package

    -- Stan usage --------------------------------------------- stan_scan_usage() --
    Packages (1):
      posterior
    Functions (5):
      posterior (5): `as_draws()`, `as_draws_df()`, `ess_bulk()`, `rhat()`, `summarise_draws()`

# print.stan_scan_usage shows many functions across packages

    -- Stan usage --------------------------------------------- stan_scan_usage() --
    Packages (4):
      bayesplot, loo, posterior, rstan
    Functions (8):
      bayesplot (2): `mcmc_trace()`, `pp_check()`
      loo (2): `loo()`, `loo_compare()`
      posterior (2): `as_draws()`, `summarise_draws()`
      rstan (2): `rstan_options()`, `stan_model()`

