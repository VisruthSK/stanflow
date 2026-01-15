library(posterior)

draws <- as_draws(1)
summarise_draws(draws)
rhat(draws)
ess_bulk(draws)
