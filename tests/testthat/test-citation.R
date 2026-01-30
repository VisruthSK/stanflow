write_file <- \(path, lines) {
  writeLines(lines, path, useBytes = TRUE)
  path
}

test_that("stan_cite returns bibtex or bibentry", {
  tmp <- withr::local_tempdir()
  path <- write_file(
    file.path(tmp, "script.R"),
    c(
      "library(posterior)",
      "posterior::as_draws(1)"
    )
  )

  pkg_env <- getFromNamespace(".stan_citation_pkgs", "stanflow")
  fun_env <- getFromNamespace(".stan_citation_funs", "stanflow")
  pkg_snapshot <- as.list(pkg_env, all.names = TRUE)
  fun_snapshot <- as.list(fun_env, all.names = TRUE)

  withr::defer({
    rm(list = ls(pkg_env, all.names = TRUE), envir = pkg_env)
    if (length(pkg_snapshot)) {
      list2env(pkg_snapshot, envir = pkg_env)
    }
    rm(list = ls(fun_env, all.names = TRUE), envir = fun_env)
    if (length(fun_snapshot)) {
      list2env(fun_snapshot, envir = fun_env)
    }
  })

  rm(list = ls(pkg_env, all.names = TRUE), envir = pkg_env)
  rm(list = ls(fun_env, all.names = TRUE), envir = fun_env)
  pkg_env$posterior <- utils::bibentry(
    "Manual",
    key = "posterior",
    title = "Posterior",
    author = "A",
    year = "2020"
  )
  fun_env$`posterior::as_draws` <- utils::bibentry(
    "Manual",
    key = "posterior-as_draws",
    title = "As Draws",
    author = "B",
    year = "2021"
  )

  bibtex <- stan_cite(path)
  expect_true(is.character(bibtex))
  expect_true(any(grepl("Posterior", bibtex, fixed = TRUE)))
  expect_true(any(grepl("As Draws", bibtex, fixed = TRUE)))

  bibentry <- stan_cite(path, format = "bibentry")
  expect_true(inherits(bibentry, "bibentry"))
  expect_true(any(grepl("Posterior", utils::toBibtex(bibentry), fixed = TRUE)))
})

test_that("stan_cite always cites stanflow and R", {
  tmp <- withr::local_tempdir()
  path <- write_file(file.path(tmp, "plain.R"), "1 + 1")

  citations <- stan_cite(path, format = "bibtex")

  expect_true(any(grepl("stanflow", citations, fixed = TRUE)))
  expect_true(
    any(
      grepl(
        "R: A Language and Environment for Statistical Computing",
        citations,
        fixed = TRUE
      )
    )
  )
})

test_that("stan_cite returns empty when no citations are found", {
  tmp <- withr::local_tempdir()
  path <- write_file(file.path(tmp, "plain.R"), "1 + 1")

  local_mocked_bindings(
    Filter = function(...) list(),
    .package = "base"
  )

  out <- stan_cite(path, format = "bibtex")
  expect_identical(out, character())
})

test_that("all package citations exist", {
  expected <- getFromNamespace(".stan_pkgs", "stanflow")

  tmp <- withr::local_tempdir()
  path <- write_file(
    file.path(tmp, "all_pkgs.R"),
    c(
      "library(bayesplot)",
      "library(brms)",
      "library(cmdstanr)",
      "library(loo)",
      "library(posterior)",
      "library(projpred)",
      "library(rstan)",
      "library(rstanarm)",
      "library(rstantools)",
      "library(shinystan)"
    )
  )

  citations <- stan_cite(path, format = "bibtex")
  expect_true(any(grepl("stanflow", citations, fixed = TRUE)))
  for (pkg in expected) {
    expect_true(any(grepl(pkg, citations, fixed = TRUE)))
  }
})
