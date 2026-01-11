test_that("stan_logo output remains stable", {
  expect_snapshot_output(stan_logo())
})

test_that("stanflow_logo output remains stable", {
  expect_snapshot_output(stanflow_logo())
})
