test_that("stan_repos respects dev flag", {
  old_repos <- options("repos")
  on.exit(options(old_repos), add = TRUE)

  custom_repos <- c(CRAN = "https://cran.r-project.org")
  options(repos = custom_repos)

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

test_that("find_unloaded filters attached packages", {
  pkgs <- c("stats", "definitely.not.a.pkg")
  expect_equal(find_unloaded(pkgs), "definitely.not.a.pkg")
})

test_that("is_attached checks the current search path", {
  expect_true(is_attached("stats"))
  expect_false(is_attached("definitely.not.a.pkg"))
})
