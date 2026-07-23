write_file <- \(path, lines) {
  writeLines(lines, path, useBytes = TRUE)
  path
}

local_env_binding <- function(name, value, env) {
  caller <- parent.frame()
  had_binding <- exists(name, envir = env, inherits = FALSE)
  old_value <- if (had_binding) {
    get(name, envir = env, inherits = FALSE)
  } else {
    NULL
  }

  assign(name, value, envir = env)
  withr::defer(
    {
      if (had_binding) {
        assign(name, old_value, envir = env)
      } else {
        base::remove(list = name, envir = env)
      }
    },
    envir = caller
  )
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
  local_env_binding(
    "posterior",
    utils::bibentry(
      "Manual",
      key = "posterior",
      title = "Posterior",
      author = "A",
      year = "2020"
    ),
    pkg_env
  )
  local_env_binding(
    "posterior::as_draws",
    utils::bibentry(
      "Manual",
      key = "posterior-as_draws",
      title = "As Draws",
      author = "B",
      year = "2021"
    ),
    fun_env
  )

  bibtex <- stan_cite(path, quiet = TRUE)
  expect_true(is.character(bibtex))
  expect_true(any(grepl("Posterior", bibtex, fixed = TRUE)))
  expect_true(any(grepl("As Draws", bibtex, fixed = TRUE)))

  bibentry <- stan_cite(path, format = "bibentry", quiet = TRUE)
  expect_true(inherits(bibentry, "bibentry"))
  expect_true(any(grepl("Posterior", utils::toBibtex(bibentry), fixed = TRUE)))
})

test_that("stan_cite always cites stanflow and R", {
  tmp <- withr::local_tempdir()
  path <- write_file(file.path(tmp, "plain.R"), "1 + 1")

  citations <- stan_cite(path, format = "bibtex", quiet = TRUE)

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

test_that("stan_cite expands stanflow to its core packages when scanning", {
  tmp <- withr::local_tempdir()
  path <- write_file(
    file.path(tmp, "stanflow.R"),
    c(
      "library(stanflow)",
      "loo(matrix(1))"
    )
  )

  citations <- stan_cite(path, format = "bibtex", quiet = TRUE)

  expect_true(any(grepl("stanflow", citations, fixed = TRUE)))
  expect_true(any(grepl("loo", citations, fixed = TRUE)))
})

test_that("stan_cite returns empty when no citations are found", {
  tmp <- withr::local_tempdir()
  path <- write_file(file.path(tmp, "plain.R"), "1 + 1")

  local_mocked_bindings(
    Filter = function(...) list(),
    .package = "base"
  )

  out <- stan_cite(path, format = "bibtex", quiet = TRUE)
  expect_identical(out, character())

  out_bibentry <- stan_cite(path, format = "bibentry", quiet = TRUE)
  expect_null(out_bibentry)
})

test_that("stan_cite quiet suppresses cli messages", {
  tmp <- withr::local_tempdir()
  path <- write_file(file.path(tmp, "plain.R"), "1 + 1")

  noisy <- capture_messages(
    stan_cite(path, format = "bibtex", quiet = FALSE)
  )
  expect_true(length(noisy) > 0)

  silent <- capture_messages(
    stan_cite(path, format = "bibtex", quiet = TRUE)
  )
  expect_equal(silent, character())
})

test_that("stan_cite defaults to stanflow.quiet option", {
  tmp <- withr::local_tempdir()
  path <- write_file(file.path(tmp, "plain.R"), "1 + 1")

  withr::local_options(list(stanflow.quiet = TRUE))
  silent <- capture_messages(stan_cite(path, format = "bibtex"))
  expect_equal(silent, character())

  withr::local_options(list(stanflow.quiet = FALSE))
  noisy <- capture_messages(stan_cite(path, format = "bibtex"))
  expect_true(length(noisy) > 0)
})

test_that("all package citations exist", {
  expected <- getFromNamespace(".stan_pkgs", "stanflow")
  installed <- expected[
    vapply(expected, requireNamespace, logical(1), quietly = TRUE)
  ]

  tmp <- withr::local_tempdir()
  path <- write_file(
    file.path(tmp, "all_pkgs.R"),
    paste0("library(", installed, ")")
  )

  citations <- stan_cite(path, format = "bibtex", quiet = TRUE)
  expect_true(any(grepl("stanflow", citations, fixed = TRUE)))
  for (pkg in installed) {
    expect_true(any(grepl(pkg, citations, fixed = TRUE)))
  }
})

test_that("package citations include generated paper entries", {
  pkg_cite <- getFromNamespace(".pkg_cite", "stanflow")

  bayesplot_bibtex <- utils::toBibtex(pkg_cite("bayesplot"))
  posterior_bibtex <- utils::toBibtex(pkg_cite("posterior"))

  expect_true(any(grepl("@Article\\{gabry-2019-vis,", bayesplot_bibtex)))
  expect_true(any(grepl("@Article\\{vehtari-2021-rhat,", posterior_bibtex)))
})

test_that("cmdstanr function citations follow cmdstanr docs", {
  skip_if_not_installed("cmdstanr")

  tmp <- withr::local_tempdir()
  path <- write_file(
    file.path(tmp, "cmdstanr.R"),
    c(
      "library(cmdstanr)",
      "mod <- cmdstan_model('model.stan')",
      "fit <- mod$sample(data = list(N = 10, y = rnorm(10)))",
      "fit$summary()",
      "fit$lp_approx()",
      "fit$loo(moment_match = TRUE)",
      "mod$optimize(data = list(N = 10, y = rnorm(10)))",
      "mod$laplace(mode = fit, draws = 100)",
      "mod$variational(data = list(N = 10, y = rnorm(10)), draws = 100)",
      "mod$pathfinder(data = list(N = 10, y = rnorm(10)), draws = 100)"
    )
  )

  citations <- stan_cite(path, format = "bibtex", quiet = TRUE)

  expect_true(any(grepl("The No-U-Turn Sampler", citations, fixed = TRUE)))
  expect_true(any(grepl(
    "Automatic differentiation variational inference",
    citations,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    "Pathfinder: parallel quasi-Newton variational inference",
    citations,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    "Yes, but did it work?: Evaluating variational inference",
    citations,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    "Rank-normalization, folding, and localization",
    citations,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    "Comparison of MCMC effective sample size estimators",
    citations,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    "Practical Bayesian model evaluation using leave-one-out cross-validation and WAIC",
    citations,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    "Pareto smoothed importance sampling",
    citations,
    fixed = TRUE
  )))
  expect_true(any(grepl(
    "Implicitly adaptive importance sampling",
    citations,
    fixed = TRUE
  )))
  expect_true(any(grepl("Stan Reference Manual", citations, fixed = TRUE)))
})
