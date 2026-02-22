library(cmdstanr)

model <- cmdstan_model("model.stan")
fit <- model$sample(data = list(N = 10, y = rnorm(10)))
model$print()
model$exe_file()

fit$draws(format = "df")
fit$diagnostic_summary()
fit$summary()

model$pathfinder(data = list(N = 10, y = rnorm(10)), draws = 100)

cmdstanr::cmdstan_model("model2.stan")
cmdstanr::write_stan_json(list(N = 5, y = rnorm(5)), "data.json")

posterior::as_draws_cmdstanr(fit)
posterior::subset_draws(fit, 1:10)

projpred::cv_varsel(1)
loo::loo(1)
