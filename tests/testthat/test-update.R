run_update_with <- function(deps_df, update_fun, check = NULL) {
  stopifnot(is.function(update_fun))
  with_mocked_bindings(
    stanflow_deps = function(recursive, dev) {
      if (!is.null(check)) check(recursive, dev)
      deps_df
    },
    update_fun()
  )
}

test_that("stanflow_update reports when nothing is behind", {
  testthat::local_reproducible_output(width = 60)
  old_repos <- options("repos")
  on.exit(options(old_repos), add = TRUE)
  options(repos = c(CRAN = "https://cloud.r-project.org"))

  empty <- data.frame(
    package = character(),
    remote = character(),
    local = character(),
    behind = logical()
  )

  expect_snapshot_output(
    run_update_with(empty, function() stanflow_update())
  )
})

test_that("stanflow_update lists behind packages", {
  testthat::local_reproducible_output(width = 60)
  old_repos <- options("repos")
  on.exit(options(old_repos), add = TRUE)
  options(repos = c(CRAN = "https://cloud.r-project.org"))

  behind <- data.frame(
    package = c("cmdstanr", "posterior"),
    remote = c("1.2.0", "1.6.0"),
    local = c("1.1.0", "1.5.0"),
    behind = c(TRUE, TRUE),
    stringsAsFactors = FALSE
  )

  expect_snapshot_output(
    run_update_with(behind, function() stanflow_update())
  )

  result <- run_update_with(
    behind,
    function() {
      utils::capture.output(out <- stanflow_update())
      out
    }
  )

  expect_identical(result, behind)
})

test_that("stanflow_update surfaces transitive dependencies (loo -> matrixStats)", {
  testthat::local_reproducible_output(width = 60)
  old_repos <- options("repos")
  on.exit(options(old_repos), add = TRUE)
  options(repos = c(CRAN = "https://cloud.r-project.org"))

  recursive <- data.frame(
    package = c("cmdstanr", "posterior", "loo", "matrixStats"),
    remote = c("1.2.0", "1.6.0", "2.8.0", "1.3.0"),
    local = c("1.1.0", "1.5.0", "2.8.0", "1.2.8"),
    behind = c(TRUE, TRUE, FALSE, TRUE),
    stringsAsFactors = FALSE
  )

  expect_snapshot_output(
    run_update_with(
      recursive,
      function() stanflow_update(recursive = TRUE),
      check = function(recursive_flag, dev_flag) {
        expect_true(recursive_flag)
        expect_false(dev_flag)
      }
    )
  )

  result <- run_update_with(
    recursive,
    function() {
      utils::capture.output(out <- stanflow_update(recursive = TRUE))
      out
    },
    check = function(recursive_flag, dev_flag) {
      expect_true(recursive_flag)
      expect_false(dev_flag)
    }
  )

  expect_identical(result, recursive[recursive$behind, , drop = FALSE])
})

test_that("stanflow_update uses Stan universe when dev = TRUE", {
  testthat::local_reproducible_output(width = 60)
  old_repos <- options("repos")
  on.exit(options(old_repos), add = TRUE)
  options(repos = c(CRAN = "https://cloud.r-project.org"))

  behind <- data.frame(
    package = c("cmdstanr", "posterior"),
    remote = c("1.2.0", "1.6.0"),
    local = c("1.1.0", "1.5.0"),
    behind = c(TRUE, TRUE),
    stringsAsFactors = FALSE
  )

  expect_snapshot_output(
    run_update_with(
      behind,
      function() stanflow_update(dev = TRUE),
      check = function(recursive_flag, dev_flag) {
        expect_false(recursive_flag)
        expect_true(dev_flag)
      }
    )
  )

  result <- run_update_with(
    behind,
    function() {
      utils::capture.output(out <- stanflow_update(dev = TRUE))
      out
    },
    check = function(recursive_flag, dev_flag) {
      expect_false(recursive_flag)
      expect_true(dev_flag)
    }
  )

  expect_identical(result, behind)
})
