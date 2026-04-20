test_that("confirm_conflict detects actual conflicting functions", {
  env_a <- "package:stanflow_conflict_test_a"
  env_b <- "package:stanflow_conflict_test_b"

  withr::local_environment(
    list2env(list(conflicted_fun = function() "a"), parent = emptyenv()),
    name = env_a
  )
  withr::local_environment(
    list2env(list(conflicted_fun = function() "b"), parent = emptyenv()),
    name = env_b
  )

  expect_equal(
    confirm_conflict(c(env_a, env_b), "conflicted_fun"),
    c(env_a, env_b)
  )
})

test_that("confirm_conflict ignores identical implementations", {
  env_a <- "package:stanflow_conflict_shared_a"
  env_b <- "package:stanflow_conflict_shared_b"
  shared_fun <- function() TRUE

  withr::local_environment(
    list2env(list(conflicted_fun = shared_fun), parent = emptyenv()),
    name = env_a
  )
  withr::local_environment(
    list2env(list(conflicted_fun = shared_fun), parent = emptyenv()),
    name = env_b
  )

  expect_null(confirm_conflict(c(env_a, env_b), "conflicted_fun"))
})

test_that("stanflow_conflict_message renders deterministic output", {
  conflicts <- structure(
    list(
      filter = c("package:stanflow", "package:dplyr", "package:stats"),
      select = c("package:stanflow", "package:dplyr")
    ),
    class = "stanflow_conflicts"
  )

  expect_snapshot_output(cat(stanflow_conflict_message(conflicts)))
})

test_that("stanflow_conflict_message returns NULL for empty conflicts", {
  expect_null(stanflow_conflict_message(structure(
    list(),
    class = "stanflow_conflicts"
  )))
})

test_that("stanflow_conflicts filters by requested packages", {
  env_target <- "package:projpred"
  env_other <- "package:stanflow_conflict_only_peer"

  withr::local_environment(
    list2env(list(conflicted_fun = function() "target"), parent = emptyenv()),
    name = env_target
  )
  withr::local_environment(
    list2env(list(conflicted_fun = function() "other"), parent = emptyenv()),
    name = env_other
  )

  conflicts <- with_mocked_bindings(
    stanflow_pkgs = c("projpred", "stanflow_conflict_only_peer"),
    stanflow_conflicts(only = "projpred"),
    .package = "stanflow"
  )
  expect_named(conflicts, "conflicted_fun")
  expect_setequal(conflicts[[1]], c(env_target, env_other))
})

test_that("print.stanflow_conflicts returns input invisibly", {
  conflicts <- structure(
    list(
      compare = c("package:stanflow", "package:stats")
    ),
    class = "stanflow_conflicts"
  )

  withr::local_output_sink(withr::local_tempfile())
  res <- print(conflicts)
  expect_identical(res, conflicts)
})

test_that("confirm_conflict ignores non-function objects", {
  env_a <- "package:stanflow_conflict_nonfun_a"
  env_b <- "package:stanflow_conflict_nonfun_b"

  withr::local_environment(
    list2env(list(conflicted_fun = function() "a"), parent = emptyenv()),
    name = env_a
  )
  withr::local_environment(
    list2env(list(conflicted_fun = 1), parent = emptyenv()),
    name = env_b
  )

  expect_null(confirm_conflict(c(env_a, env_b), "conflicted_fun"))
})
