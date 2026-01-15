library(posterior)
library(dplyr)

data("penguins", package = "palmerpenguins")

summary_tbl <- penguins |>
  group_by(species) |>
  summarise(
    mass_mean = mean(body_mass_g, na.rm = TRUE),
    mass_sd = sd(body_mass_g, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

fake_draws <- as_draws(list(mu = rnorm(100), sigma = rexp(100)))

summarise_draws(fake_draws)
as_draws_df(fake_draws)
as_draws_matrix(fake_draws)

rhat(fake_draws)
ess_bulk(fake_draws)
ess_tail(fake_draws)
mcse_mean(fake_draws)

loo::loo(fake_draws)
loo::loo_compare(fake_draws, fake_draws)
projpred::cv_varsel(fake_draws)
