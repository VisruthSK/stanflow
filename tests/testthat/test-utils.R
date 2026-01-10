test_that("stan_repos respects dev flag", {
  custom_repos <- c(CRAN = "https://cran.r-project.org")
  withr::local_options(list(repos = custom_repos))

  expect_equal(
    stan_repos(dev = FALSE),
    c("https://community.r-multiverse.org", custom_repos)
  )

  expect_equal(
    stan_repos(dev = TRUE),
    c("https://stan-dev.r-universe.dev", custom_repos)
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
  expect_equal(find_unloaded(pkgs), "definitely.not.a.pkg")
})

test_that("is_attached checks the current search path", {
  expect_true(is_attached("stats"))
  expect_false(is_attached("definitely.not.a.pkg"))
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
