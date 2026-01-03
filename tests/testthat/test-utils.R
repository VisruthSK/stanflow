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

test_that("invert returns empty list for empty input", {
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

  old_quiet <- options("stanflow.quiet")
  on.exit(options(old_quiet), add = TRUE)
  options(stanflow.quiet = TRUE)
  expect_null(wrapped_startup("ignored"))
})

test_that("wrapped_startup emits startup messages when enabled", {
  old_quiet <- options("stanflow.quiet")
  on.exit(options(old_quiet), add = TRUE)
  options(stanflow.quiet = FALSE)
  expect_message(wrapped_startup("hello from stanflow"), "hello from stanflow")
})

test_that("grepl_fixed uses regex for special patterns", {
  expect_true(grepl_fixed("a.b", "acb"))
  expect_false(grepl_fixed("a+b", "a+b"))
})

test_that("grepl_fixed uses fixed matching for plain patterns", {
  expect_true(grepl_fixed("plain", "some plain text"))
  expect_false(grepl_fixed("plain", "nothing here"))
})

test_that("is_installed reports presence via find.package", {
  local_mocked_bindings(
    find.package = function(...) "path",
    .package = "base"
  )
  expect_true(is_installed("foo"))

  local_mocked_bindings(
    find.package = function(...) character(),
    .package = "base"
  )
  expect_false(is_installed("foo"))
})

test_that("pkg_version returns version only when installed", {
  local_mocked_bindings(
    is_installed = function(...) TRUE,
    .package = "stanflow"
  )
  local_mocked_bindings(
    packageVersion = function(...) package_version("1.2.3"),
    .package = "utils"
  )
  expect_equal(pkg_version("foo"), "1.2.3")

  local_mocked_bindings(
    is_installed = function(...) FALSE,
    .package = "stanflow"
  )
  expect_equal(pkg_version("foo"), NA_character_)
})

test_that("add_planned appends command strings", {
  expect_equal(
    add_planned(c("a"), "b", "c"),
    c("a", "b", "c")
  )
})
