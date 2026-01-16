library(tidymodels)
library(dplyr)
library(stringr)

data("penguins", package = "palmerpenguins")

penguins_tbl <- penguins |>
  filter(!is.na(body_mass_g), !is.na(sex)) |>
  mutate(
    island = factor(island),
    species = factor(species),
    sex = factor(sex)
  )

set.seed(123)
sp <- initial_split(penguins_tbl, strata = species)
train_tbl <- training(sp)

rec <- recipe(
  body_mass_g ~ bill_length_mm +
    bill_depth_mm +
    flipper_length_mm +
    sex +
    island,
  data = train_tbl
) |>
  step_impute_mean(all_numeric_predictors()) |>
  step_dummy(all_nominal_predictors()) |>
  step_normalize(all_numeric_predictors()) |>
  step_zv(all_predictors()) |>
  step_interact(terms = ~ starts_with("bill"):starts_with("flipper"))

rf_spec <- rand_forest(mtry = 3, trees = 200) |>
  set_engine("ranger") |>
  set_mode("regression")

lin_spec <- linear_reg(penalty = tune(), mixture = tune()) |>
  set_engine("glmnet") |>
  set_mode("regression")

wf_rf <- workflow() |>
  add_model(rf_spec) |>
  add_recipe(rec)

wf_lin <- workflow() |>
  add_model(lin_spec) |>
  add_recipe(rec)

folds <- vfold_cv(train_tbl, v = 3)

param_grid <- grid_regular(
  penalty(range = c(-6, 0)),
  mixture(range = c(0, 1)),
  levels = c(penalty = 5, mixture = 3)
)

res <- tune_grid(wf_lin, resamples = folds, grid = param_grid)
metrics <- collect_metrics(res)

best <- select_best(res, "rmse")
final_wf <- finalize_workflow(wf_lin, best)
final_fit <- fit(final_wf, train_tbl)

# Resampling and workflowsets
wf_set <- workflow_set(
  preproc = list(base = rec),
  models = list(rf = rf_spec, lin = lin_spec)
)
wf_res <- workflow_map(wf_set, resamples = folds, grid = 3)
wf_metrics <- collect_metrics(wf_res)

# Intentionally unqualified functions that should not map to Stan
mixture(0.1)
logit(0.2)
