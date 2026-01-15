library(cmdstanr)

cmdstan_model("model.stan")
cmdstanr::cmdstan_model("model2.stan")

projpred::cv_varsel(1)
loo::loo(1)
