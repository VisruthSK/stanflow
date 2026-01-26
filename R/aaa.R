# These are the managed packages. Note that `{rstantools}` isn't here, but is considered by `stan_cite()`--it won't be installed, but it will be cited.
core <- c("bayesplot", "loo", "posterior", "projpred", "shinystan")
backends <- c("brms", "cmdstanr", "rstan", "rstanarm")
stanflow_pkgs <- c(core, backends) # used internally
