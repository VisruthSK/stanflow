test_that("stan_logo output remains stable", {
  testthat::local_reproducible_output(width = 80)
  expect_snapshot_output(stan_logo())
})

test_that("stanflow_logo output remains stable", {
  testthat::local_reproducible_output(width = 80)
  expect_snapshot_output(stanflow_logo())
})
