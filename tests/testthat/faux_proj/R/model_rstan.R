library(rstan)
library(rstanarm)
library(shinystan)

stan_model("model.stan")
extract(1)
logit(0.5)
launch_shinystan(1)
