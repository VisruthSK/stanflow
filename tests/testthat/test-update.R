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
    with_mocked_bindings(
      stanflow_deps = function(recursive, dev) empty,
      stanflow_update()
    )
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
    with_mocked_bindings(
      stanflow_deps = function(recursive, dev) behind,
      stanflow_update()
    )
  )

  result <- with_mocked_bindings(
    stanflow_deps = function(recursive, dev) behind,
    {
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
    with_mocked_bindings(
      stanflow_deps = function(recursive_flag, dev) {
        expect_true(recursive_flag)
        recursive
      },
      stanflow_update(recursive = TRUE)
    )
  )

  result <- with_mocked_bindings(
    stanflow_deps = function(recursive_flag, dev) recursive,
    {
      utils::capture.output(out <- stanflow_update(recursive = TRUE))
      out
    }
  )

  expect_identical(result, recursive[recursive$behind, , drop = FALSE])
})
