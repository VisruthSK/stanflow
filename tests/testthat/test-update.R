run_update_with <- function(deps_df, update_fun, check = NULL) {
  stopifnot(is.function(update_fun))
  withr::local_options(list(stanflow.testing = TRUE))
  with_mocked_bindings(
    stanflow_deps = function(recursive, dev, check_updates = TRUE) {
      if (!is.null(check)) {
        check(recursive, dev)
      }
      deps_df
    },
    with_mocked_bindings(
      install.packages = function(...) invisible(NULL),
      update_fun(),
      .package = "utils"
    ),
    .package = "stanflow"
  )
}

test_that("stanflow_update reports when nothing is behind", {
  testthat::local_reproducible_output(width = 60)
  withr::local_options(list(
    repos = c(CRAN = "https://cloud.r-project.org")
  ))

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
  withr::local_options(list(
    repos = c(CRAN = "https://cloud.r-project.org")
  ))

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

test_that("stanflow_deps computes remote/local state", {
  fake_available <- matrix(
    c("1.2.0", "1.6.0", "2.8.0"),
    nrow = 3,
    dimnames = list(c("cmdstanr", "posterior", "base"), "Version")
  )

  local_mocked_bindings(
    stan_repos = function(dev) "https://fake.repo",
    is_installed = function(pkg) pkg == "cmdstanr",
    .package = "stanflow"
  )
  local_mocked_bindings(
    available.packages = function(repos) fake_available,
    packageVersion = function(pkg) package_version("1.1.0"),
    .package = "utils"
  )
  local_mocked_bindings(
    package_dependencies = function(pkgs, db, recursive) {
      list(stanflow = c("cmdstanr", "posterior", "base"))
    },
    .package = "tools"
  )

  deps <- stanflow_deps(recursive = TRUE, dev = FALSE)

  expect_equal(deps$package, c("cmdstanr", "posterior"))
  expect_equal(deps$local, c("1.1.0", "0"))
  expect_true(all(deps$behind))
})

test_that("stanflow_deps aborts when package metadata cannot be downloaded", {
  local_mocked_bindings(
    stan_repos = function(dev) "https://fake.repo",
    .package = "stanflow"
  )
  local_mocked_bindings(
    available.packages = function(...) stop("no network"),
    .package = "utils"
  )

  expect_error(
    stanflow_deps(check_updates = TRUE),
    "Unable to reach repositories"
  )
})

test_that("stanflow_deps handles missing repo versions", {
  fake_available <- matrix(
    c(NA, "1.6.0"),
    nrow = 2,
    dimnames = list(c("cmdstanr", "posterior"), "Version")
  )

  local_mocked_bindings(
    stan_repos = function(dev) "https://fake.repo",
    is_installed = function(pkg) FALSE,
    .package = "stanflow"
  )
  local_mocked_bindings(
    available.packages = function(repos) fake_available,
    packageVersion = function(pkg) package_version("0"),
    .package = "utils"
  )
  local_mocked_bindings(
    package_dependencies = function(pkgs, db, recursive) {
      list(stanflow = c("cmdstanr", "posterior"))
    },
    .package = "tools"
  )

  deps <- stanflow_deps(recursive = FALSE, dev = TRUE)

  expect_false(deps$behind[1])
  expect_true(deps$behind[2])
  expect_true(is.na(deps$remote[1]))
})

test_that("stanflow_deps builds from description when check_updates = FALSE", {
  dep_calls <- list(recursive = NULL)

  local_mocked_bindings(
    packageDescription = function(pkg) {
      list(Depends = "R", Imports = "foo, bar (>= 1.0)", Suggests = "baz")
    },
    installed.packages = function() matrix(character(), nrow = 0),
    packageVersion = function(pkg) package_version("1.0.0"),
    .package = "utils"
  )
  local_mocked_bindings(
    package_dependencies = function(pkgs, db, recursive) {
      dep_calls$recursive <<- recursive
      list(foo = "qux")
    },
    .package = "tools"
  )
  local_mocked_bindings(
    is_installed = function(pkg) pkg == "foo",
    .package = "stanflow"
  )

  deps <- stanflow_deps(recursive = TRUE, check_updates = FALSE)

  expect_true(dep_calls$recursive)
  expect_true(all(is.na(deps$remote)))
  expect_false(any(deps$behind))
  expect_true(all(c("foo", "bar", "baz", "qux") %in% deps$package))
})

test_that("stanflow_deps falls back when dependencies are empty", {
  fake_available <- matrix(
    c("1.0.0"),
    nrow = 1,
    dimnames = list(c("foo"), "Version")
  )
  called <- list(primary = FALSE, fallback = FALSE)

  local_mocked_bindings(
    stan_repos = function(dev) "https://fake.repo",
    .package = "stanflow"
  )
  local_mocked_bindings(
    available.packages = function(repos) fake_available,
    packageDescription = function(pkg) {
      list(Depends = "R", Imports = "foo", Suggests = "")
    },
    .package = "utils"
  )
  local_mocked_bindings(
    package_dependencies = function(pkgs, db, recursive) {
      if (identical(pkgs, "stanflow")) {
        called$primary <<- TRUE
        return(list(stanflow = character()))
      }
      called$fallback <<- TRUE
      list(stanflow = "foo")
    },
    .package = "tools"
  )
  local_mocked_bindings(
    is_installed = function(pkg) FALSE,
    .package = "stanflow"
  )

  deps <- stanflow_deps(recursive = FALSE, check_updates = TRUE)

  expect_true(called$primary)
  expect_true(called$fallback)
  expect_equal(deps$package, "foo")
})

test_that("stanflow_update aborts in non-interactive sessions", {
  skip_if(interactive())
  withr::local_options(list(stanflow.testing = FALSE))
  expect_error(stanflow_update(), "must be run interactively")
})

test_that("stanflow_update surfaces transitive dependencies (loo -> matrixStats)", {
  testthat::local_reproducible_output(width = 60)
  withr::local_options(list(
    repos = c(CRAN = "https://cloud.r-project.org")
  ))

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

test_that("stanflow_update reports packages that need reinstall after warnings", {
  testthat::local_reproducible_output(width = 60)
  withr::local_options(list(
    repos = c(CRAN = "https://cloud.r-project.org"),
    stanflow.testing = TRUE
  ))

  behind <- data.frame(
    package = c("cmdstanr", "posterior"),
    remote = c("1.2.0", "1.6.0"),
    local = c("1.1.0", "1.5.0"),
    behind = c(TRUE, TRUE),
    stringsAsFactors = FALSE
  )

  output <- with_mocked_bindings(
    stanflow_deps = function(...) behind,
    with_mocked_bindings(
      install.packages = function(...) {
        warning("cannot remove prior installation of package 'cmdstanr'")
        warning("some other warning")
        warning("cannot remove prior installation of package 'cmdstanr'")
        warning("cannot remove prior installation of package 'posterior'")
      },
      capture.output(suppressWarnings(stanflow_update())),
      .package = "utils"
    ),
    .package = "stanflow"
  )

  expect_snapshot_output(cat(output, sep = "\n"))
})

test_that("stanflow_update uses Stan universe when dev = TRUE", {
  testthat::local_reproducible_output(width = 60)
  withr::local_options(list(
    repos = c(CRAN = "https://cloud.r-project.org")
  ))

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
