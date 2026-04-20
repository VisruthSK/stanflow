test_that("stan_repos respects dev flag", {
  custom_repos <- c(CRAN = "https://cran.r-project.org")
  withr::local_options(list(repos = custom_repos))

  expect_equal(
    stan_repos(dev = FALSE),
    c(Multiverse = "https://community.r-multiverse.org", custom_repos)
  )

  expect_equal(
    stan_repos(dev = TRUE),
    c(StanRUniverse = "https://stan-dev.r-universe.dev", custom_repos)
  )
})

test_that("invert swaps nested lists into lookup lists", {
  x <- list(
    rstan = c("draws", "samples"),
    cmdstanr = c("draws")
  )

  expect_equal(
    invert(x),
    list(
      draws = c("rstan", "cmdstanr"),
      samples = "rstan"
    )
  )
})

test_that("invert handles empty inputs", {
  expect_equal(invert(list()), list())
})

test_that("find_unloaded filters attached packages", {
  pkgs <- c("stats", "definitely.not.a.pkg")
  expect_equal(.find_unloaded(pkgs), "definitely.not.a.pkg")
})

test_that("is_attached checks the current search path", {
  expect_true(is_attached("stats"))
  expect_false(is_attached("definitely.not.a.pkg"))
})

test_that("is_interactive_session honors override options", {
  withr::local_options(list(stanflow.force_interactive = TRUE))
  expect_true(is_interactive_session())

  withr::local_options(list(stanflow.force_interactive = FALSE))
  expect_false(is_interactive_session())
})

test_that("wrapped_startup handles NULL and quiet option", {
  expect_null(wrapped_startup(NULL))

  withr::local_options(list(stanflow.quiet = TRUE))
  expect_null(wrapped_startup("ignored"))
})

test_that("wrapped_startup emits startup messages when enabled", {
  withr::local_options(list(stanflow.quiet = FALSE))
  expect_message(wrapped_startup("hello from stanflow"), "hello from stanflow")
})

test_that("local_cli_quiet suppresses cli messages within caller", {
  capture <- function(quiet) {
    f <- function() {
      local_cli_quiet(quiet)
      cli::cli_alert_info("hello from cli")
      invisible(NULL)
    }
    capture_messages(f())
  }

  expect_equal(capture(TRUE), character())
  expect_match(capture(FALSE), "hello from cli")
})

test_that("local_cli_quiet restores cli output after caller exits", {
  f <- function() {
    local_cli_quiet(TRUE)
    cli::cli_alert_info("silenced")
    invisible(NULL)
  }
  capture_messages(f())

  out <- capture_messages(cli::cli_alert_info("audible"))
  expect_match(out, "audible")
})

test_that(".same_library uses the package library path when loaded", {
  captured <- list(lib.loc = NULL, character.only = NULL, warn.conflicts = NULL)

  local_mocked_bindings(
    library = function(pkg, lib.loc, character.only, warn.conflicts, ...) {
      captured$lib.loc <<- lib.loc
      captured$character.only <<- character.only
      captured$warn.conflicts <<- warn.conflicts
      NULL
    },
    loadedNamespaces = function() c("stats", "utils"),
    .package = "base"
  )

  .same_library("stats")

  expect_true(length(captured$lib.loc) == 1)
  expect_identical(
    captured$lib.loc,
    dirname(getNamespaceInfo("stats", "path"))
  )
  expect_true(isTRUE(captured$character.only))
  expect_false(captured$warn.conflicts)
})
