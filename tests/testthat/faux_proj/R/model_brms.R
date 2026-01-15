library(brms)
library(posterior)
library(dplyr)
library(tidyr)
library(ggplot2)
library(purrr)
library(stringr)

data("penguins", package = "palmerpenguins")

penguins_clean <- penguins |>
  filter(!is.na(bill_length_mm), !is.na(bill_depth_mm)) |>
  mutate(
    log_mass = log(body_mass_g),
    sex = factor(sex),
    island = factor(island),
    species = factor(species)
  ) |>
  drop_na(sex, island)

summary_tbl <- penguins_clean |>
  group_by(species, island) |>
  summarise(
    avg_mass = mean(body_mass_g),
    sd_mass = sd(body_mass_g),
    n = n(),
    .groups = "drop"
  ) |>
  mutate(tag = str_c(species, island, sep = "-"))

long_tbl <- summary_tbl |>
  pivot_longer(
    cols = c(avg_mass, sd_mass),
    names_to = "stat",
    values_to = "value"
  )

p <- ggplot(penguins_clean, aes(bill_length_mm, bill_depth_mm, color = species)) +
  geom_point(alpha = 0.6) +
  facet_wrap(~ island)

bf_mass <- bf(
  log_mass ~ bill_length_mm * bill_depth_mm + sex + (1 + bill_length_mm | island)
)

bf_flen <- bf(flipper_length_mm ~ bill_length_mm + species + (1 | island))

priors <- c(
  set_prior("normal(0, 1)", class = "b"),
  set_prior("normal(0, 1)", class = "Intercept"),
  set_prior("exponential(1)", class = "sd")
)

fit <- brm(
  bf_mass,
  data = penguins_clean,
  prior = priors,
  chains = 2,
  cores = 2,
  iter = 500,
  seed = 123
)

fit_flen <- brm(
  bf_flen,
  data = penguins_clean,
  prior = priors,
  chains = 2,
  cores = 2,
  iter = 400,
  seed = 456
)

fit_update <- update(fit, newdata = penguins_clean, recompile = FALSE)

mix_family <- mixture(gaussian, student)
fit_mix <- brm(
  bf_mass,
  data = penguins_clean,
  family = mix_family,
  chains = 2,
  cores = 2,
  iter = 300
)

# Draws processing
as_draws(fit)
posterior::as_draws_df(fit)
posterior::summarise_draws(fit)
brms:::as_draws(fit_mix)
posterior::rhat(fit)
posterior::ess_bulk(fit)
posterior::ess_tail(fit)

brms::mixture(0.4)
get_prior(bf_mass, data = penguins_clean)
conditional_effects(fit, effects = "bill_length_mm")
pp_check(fit)
