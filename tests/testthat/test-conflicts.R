test_that("confirm_conflict detects actual conflicting functions", {
  env_a <- "package:stanflow_conflict_test_a"
  env_b <- "package:stanflow_conflict_test_b"

  attach(list(conflicted_fun = function() "a"), name = env_a)
  attach(list(conflicted_fun = function() "b"), name = env_b)
  on.exit(detach(env_b, character.only = TRUE), add = TRUE)
  on.exit(detach(env_a, character.only = TRUE), add = TRUE)

  expect_equal(
    confirm_conflict(c(env_a, env_b), "conflicted_fun"),
    c(env_a, env_b)
  )
})

test_that("confirm_conflict ignores identical implementations", {
  env_a <- "package:stanflow_conflict_shared_a"
  env_b <- "package:stanflow_conflict_shared_b"
  shared_fun <- function() TRUE

  attach(list(conflicted_fun = shared_fun), name = env_a)
  attach(list(conflicted_fun = shared_fun), name = env_b)
  on.exit(detach(env_b, character.only = TRUE), add = TRUE)
  on.exit(detach(env_a, character.only = TRUE), add = TRUE)

  expect_null(confirm_conflict(c(env_a, env_b), "conflicted_fun"))
})

test_that("stanflow_conflict_message renders deterministic output", {
  testthat::local_reproducible_output(width = 60)
  conflicts <- structure(
    list(
      filter = c("package:stanflow", "package:dplyr", "package:stats"),
      select = c("package:stanflow", "package:dplyr")
    ),
    class = "stanflow_conflicts"
  )

  expect_snapshot_output(cat(stanflow_conflict_message(conflicts)))
})
