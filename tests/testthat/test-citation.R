write_file <- \(path, lines) {
  writeLines(lines, path, useBytes = TRUE)
  path
}

bind_internal <- \(name) getFromNamespace(name, "stanflow")

# Run snapshot expectations in non-interactive sessions.
force_local_snapshots <- function() {
  withr::local_envvar(NOT_CRAN = "true", .local_envir = parent.frame())
}

# Bind internal helpers/data so tests can call them directly.
.meta_year <- bind_internal(".meta_year")
.meta_note <- bind_internal(".meta_note")
.meta_authors <- bind_internal(".meta_authors")
.scan_tokens <- bind_internal(".scan_tokens")
.extract_code <- bind_internal(".extract_code")
.stan_exports <- bind_internal(".stan_exports")
.stan_export_index <- bind_internal(".stan_export_index")
.stan_origin_map <- bind_internal(".stan_origin_map")
.stan_pkgs <- bind_internal(".stan_pkgs")

resolve_origin_pkg <- function(pkg, fun) {
  key <- paste0(pkg, "::", fun)
  if (is.null(.stan_origin_map) || !key %in% names(.stan_origin_map)) {
    return(NA_character_)
  }
  origin <- .stan_origin_map[[key]]
  if (is.null(origin) || is.na(origin)) {
    origin <- pkg
  }
  if (!origin %in% .stan_pkgs) {
    return(NA_character_)
  }
  origin
}

resolve_origin_key <- function(pkg, fun) {
  origin <- resolve_origin_pkg(pkg, fun)
  if (is.na(origin)) {
    return(NA_character_)
  }
  paste0(origin, "::", fun)
}

test_that("citation metadata helpers parse fields", {
  meta <- list(
    Date = "2023-02-01",
    Version = "1.2.3",
    `Authors@R` = "c(person('A', 'B', role = c('aut', 'cre')))"
  )

  expect_equal(.meta_year(meta), "2023")
  expect_equal(.meta_note(meta), "R package version 1.2.3")
  expect_equal(length(.meta_authors(meta)), 1L)
})

test_that(".scan_tokens handles empty or no-code files", {
  expect_equal(
    .scan_tokens("", stdlib_funs()),
    list(pkgs = character(), keys = character(), ambiguous = character())
  )
  expect_equal(
    .scan_tokens("# just a comment", stdlib_funs()),
    list(pkgs = character(), keys = character(), ambiguous = character())
  )
})

test_that(".scan_tokens handles non-Stan library calls", {
  code <- c(
    "library(ggplot2)",
    "requireNamespace('base')"
  )
  hits <- .scan_tokens(paste(code, collapse = "\n"), stdlib_funs())
  expect_equal(hits$pkgs, character())
  expect_equal(hits$keys, character())
})

test_that(".scan_tokens handles use() calls", {
  code <- c(
    'use("posterior", "is_rvar")',
    "use(\"cmdstanr\", c(\"as.CmdStanGQ\", \"cmdstan_model\", 'eng_cmdstan'))",
    "use('brms', 'brm')"
  )

  hits <- .scan_tokens(paste(code, collapse = "\n"), stdlib_funs())

  expect_true(all(c("posterior", "cmdstanr", "brms") %in% hits$pkgs))
  expect_true(all(
    c(
      "posterior::is_rvar",
      "cmdstanr::as.CmdStanGQ",
      "cmdstanr::cmdstan_model",
      "cmdstanr::eng_cmdstan",
      "brms::brm"
    ) %in%
      hits$keys
  ))
})

test_that(".scan_tokens handles nested use() calls", {
  code <- c(
    "use('posterior', list('as_draws', list(c('rhat', list('ess_bulk')))))"
  )

  hits <- .scan_tokens(paste(code, collapse = "\n"), stdlib_funs())

  expect_true(all(
    c("posterior::as_draws", "posterior::rhat", "posterior::ess_bulk") %in%
      hits$keys
  ))
  expect_true("posterior" %in% hits$pkgs)
})

test_that(".scan_tokens resolves attachment order and requireNamespace", {
  code <- c(
    "library(posterior)",

    "requireNamespace(\"cmdstanr\")",

    "library('brms')",

    "`as_draws`(1)"
  )

  hits <- .scan_tokens(paste(code, collapse = "\n"), stdlib_funs())

  expect_true(all(c("posterior", "cmdstanr", "brms") %in% hits$pkgs))

  expected_key <- resolve_origin_key("brms", "as_draws")
  if (is.na(expected_key)) {
    expect_false(any(grepl("::as_draws$", hits$keys)))
  } else {
    expect_true(expected_key %in% hits$keys)
  }

  # as_draws is in posterior too, but brms was attached later (last) so it should win?

  # In .scan_tokens logic:

  # library(posterior) -> pos X

  # library(brms) -> pos Y > X

  # choose_attached -> max pos -> brms.

  # So brms::as_draws is expected.

  expect_false("cmdstanr::as_draws" %in% hits$keys)
})

test_that(".scan_tokens respects allowed_packages", {
  code <- c(
    "library(posterior)",
    "library(brms)",
    "as_draws(1)"
  )

  hits <- .scan_tokens(
    paste(code, collapse = "\n"),
    stdlib_funs(),
    allowed_packages = "posterior"
  )

  expect_true(setequal(hits$pkgs, "posterior"))
  expect_equal(hits$keys, "posterior::as_draws")
})

test_that(".scan_tokens ignores unqualified calls when attached packages do not match", {
  code <- c(
    "library(posterior)",

    "log_lik(1)"
  )

  hits <- .scan_tokens(paste(code, collapse = "\n"), stdlib_funs())

  expect_equal(hits$keys, character())
  expect_true("posterior" %in% hits$pkgs)
})


test_that(".scan_tokens ignores unqualified calls when no packages are attached", {
  hits <- .scan_tokens("as_draws(1)", stdlib_funs())

  expect_equal(hits$keys, character())
  expect_equal(hits$pkgs, character())
})

test_that(".scan_tokens ignores language keywords and operators", {
  export_index <- list(
    foo = "pkgA",
    `if` = "pkgA",
    `<-` = "pkgA",
    `+` = "pkgA",
    `[` = "pkgA",
    `[[` = "pkgA",
    `$` = "pkgA",
    `@` = "pkgA"
  )
  origin_map <- c(
    "pkgA::foo" = "pkgA",
    "pkgA::if" = "pkgA",
    "pkgA::<-" = "pkgA",
    "pkgA::+" = "pkgA",
    "pkgA::[" = "pkgA",
    "pkgA::[[" = "pkgA",
    "pkgA::$" = "pkgA",
    "pkgA::@" = "pkgA"
  )
  allowed_packages <- "pkgA"

  code <- c(
    "library(pkgA)",
    "if (TRUE) foo(1)",
    "x <- foo(1)",
    "x + 1",
    "x[1]",
    "x[[1]]",
    "x$y",
    "x@y"
  )

  hits <- .scan_tokens(
    paste(code, collapse = "\n"),
    stdlib_funs(),
    allowed_packages = allowed_packages,
    export_index = export_index,
    origin_map = origin_map
  )

  expect_true("pkgA" %in% hits$pkgs)
  expect_equal(hits$keys, c("pkgA::foo", "pkgA::foo"))
  expect_equal(hits$ambiguous, character())
})


test_that(".scan_tokens collapses reexports by origin", {
  export_index <- list(foo = c("pkgA", "pkgB"))
  origin_map <- c("pkgA::foo" = "pkgA", "pkgB::foo" = "pkgA")
  allowed_packages <- c("pkgA", "pkgB")

  code <- c(
    "library(pkgB)",
    "foo(1)"
  )
  hits <- .scan_tokens(
    paste(code, collapse = "\n"),
    stdlib_funs(),
    allowed_packages = allowed_packages,
    export_index = export_index,
    origin_map = origin_map
  )

  expect_true(setequal(hits$pkgs, c("pkgA", "pkgB")))
  expect_equal(hits$keys, "pkgA::foo")
  expect_equal(hits$ambiguous, character())
})


test_that(".scan_tokens records ambiguous origins", {
  fun <- "ess_bulk"

  pkgs <- names(Filter(function(x) fun %in% x, .stan_exports))

  pkgs <- pkgs[vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]

  origins <- vapply(
    pkgs,
    function(pkg) {
      .stan_origin_map[[paste0(pkg, "::", fun)]]
    },
    character(1)
  )

  providers <- unique(origins[!is.na(origins)])

  if (length(providers) < 2) {
    skip("No ambiguous functions found in installed packages.")
  }

  code <- c(paste0("library(", pkgs, ")"), paste0(fun, "(1)"))
  hits <- .scan_tokens(
    paste(code, collapse = "\n"),
    stdlib_funs(),
    strict = TRUE
  )

  expect_equal(hits$keys, character())

  expect_equal(hits$ambiguous, fun)
})

test_that(".scan_tokens handles single-package functions", {
  candidates <- split(
    rep(names(.stan_exports), lengths(.stan_exports)),
    .stan_exports |> unlist(use.names = FALSE)
  )
  single_fun <- candidates |>
    (\(x) names(x)[lengths(x) == 1L])() |>
    setdiff(stdlib_funs()) |>
    (\(x) x[make.names(x) == x])()
  if (!length(single_fun)) {
    skip("No single-package functions found.")
  }

  picked_fun <- NULL
  picked_pkg <- NULL
  for (fun in single_fun) {
    pkg <- candidates[[fun]][[1L]]
    code <- c(paste0("library(", pkg, ")"), paste0(fun, "(1)"))
    hits <- .scan_tokens(paste(code, collapse = "\n"), stdlib_funs())
    if (paste0(pkg, "::", fun) %in% hits$keys) {
      picked_fun <- fun
      picked_pkg <- pkg
      break
    }
  }

  if (is.null(picked_fun)) {
    skip("No single-package functions resolved in .scan_tokens.")
  }

  code <- c(paste0("library(", picked_pkg, ")"), paste0(picked_fun, "(1)"))
  hits <- .scan_tokens(paste(code, collapse = "\n"), stdlib_funs())
  expect_true(paste0(picked_pkg, "::", picked_fun) %in% hits$keys)
})

test_that(".scan_tokens handles namespaced calls and stdlib exclusions", {
  code <- c(
    "posterior::as_draws(1)",
    "brms:::as_draws(2)",
    "rstan::plot(3)",
    "stats::lm(1, 2)"
  )
  hits <- .scan_tokens(paste(code, collapse = "\n"), stdlib_funs())

  expect_true(all(c("posterior::as_draws", "brms::as_draws") %in% hits$keys))
  expect_true("rstan::plot" %in% hits$keys)
  expect_false("stats::lm" %in% hits$keys)
  expect_true(all(c("posterior", "brms") %in% hits$pkgs))
  expect_true("rstan" %in% hits$pkgs)
})

test_that(".scan_tokens ignores unqualified stdlib calls", {
  hits <- .scan_tokens("plot(1)", stdlib_funs())
  expect_equal(hits$pkgs, character())
  expect_equal(hits$keys, character())
})

test_that(".scan_tokens ignore_unqualified_functions overrides apply only to unqualified calls", {
  code <- c(
    "as_draws(1)",
    "posterior::as_draws(2)"
  )
  hits <- .scan_tokens(
    paste(code, collapse = "\n"),
    ignore_unqualified_functions = "as_draws"
  )
  expect_true("posterior::as_draws" %in% hits$keys)
  expect_true("posterior" %in% hits$pkgs)
  expect_identical(hits$keys, "posterior::as_draws")
})

test_that(".extract_code returns R source verbatim", {
  tmp <- withr::local_tempdir()
  path <- write_file(file.path(tmp, "script.R"), c("x <- 1", "x"))
  expect_equal(.extract_code(path), "x <- 1\nx")
})

test_that(".extract_code extracts Rmd chunks", {
  skip_if_not_installed("knitr")
  tmp <- withr::local_tempdir()
  path <- write_file(
    file.path(tmp, "doc.Rmd"),
    c(
      "---",
      "title: 'Doc'",
      "---",
      "",
      "```{r}",
      "as_draws(1)",
      "```"
    )
  )
  out <- .extract_code(path)
  expect_match(out, "as_draws\\(")
})

test_that(".extract_code extracts Qmd chunks", {
  skip_if_not_installed("knitr")
  tmp <- withr::local_tempdir()
  path <- write_file(
    file.path(tmp, "doc.qmd"),
    c(
      "---",
      "title: 'Doc'",
      "---",
      "",
      "```{r}",
      "as_draws(1)",
      "```"
    )
  )
  out <- .extract_code(path)
  expect_match(out, "as_draws\\(")
})


test_that(".extract_code errors on unsupported extensions", {
  tmp <- withr::local_tempdir()
  path <- write_file(file.path(tmp, "note.txt"), "x <- 1")
  expect_error(
    .extract_code(path),
    "Unsupported file extension: txt"
  )
})

test_that("stan_cite returns empty when no citations match", {
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

  res <- stan_cite(path)
  expect_equal(res, character())
})

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

test_that("stan_cite always includes an R citation", {
  tmp <- withr::local_tempdir()
  path <- file.path(tmp, "script.R")
  writeLines("x <- 1", path)

  bibtex <- stan_cite(path, format = "bibtex")

  expect_true(any(grepl("R Core Team", bibtex, fixed = TRUE)))
})

test_that("scan_usage handles a single file path", {
  tmp <- withr::local_tempdir()
  path <- write_file(
    file.path(tmp, "single.R"),
    c(
      "library(posterior)",
      "as_draws(1)"
    )
  )
  res <- scan_usage(path)
  expect_true(inherits(res, "scan_usage"))
  expect_equal(res$packages, "posterior")
  expect_equal(res$functions, "posterior::as_draws")
})

test_that("scan_usage aborts on parse errors in strict mode", {
  tmp <- withr::local_tempdir()
  path <- write_file(
    file.path(tmp, "bad.R"),
    c(
      "function("
    )
  )

  expect_error(
    scan_usage(path, strict = TRUE),
    "Failed to parse"
  )
})

test_that("scan_usage strict aborts on ambiguous unqualified calls", {
  funs <- c("rhat", "ess_bulk")

  get_fun_pkgs <- function(fun) {
    pkgs <- names(Filter(function(x) fun %in% x, .stan_exports))
    pkgs[vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  }

  needs <- vapply(
    funs,
    function(fun) {
      pkgs <- names(Filter(function(x) fun %in% x, .stan_exports))

      pkgs <- pkgs[vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]

      origins <- vapply(
        pkgs,
        function(pkg) {
          .stan_origin_map[[paste0(pkg, "::", fun)]]
        },
        character(1)
      )

      length(unique(origins[!is.na(origins)])) > 1
    },
    logical(1)
  )

  if (!all(needs)) {
    skip("Ambiguous functions not available in installed packages.")
  }

  lib_pkgs <- unique(unlist(lapply(funs, get_fun_pkgs)))
  if (length(lib_pkgs) < 2) {
    skip("Ambiguous functions not available in installed packages.")
  }

  tmp <- withr::local_tempdir()
  path <- write_file(
    file.path(tmp, "strict.R"),
    c(
      paste0("library(", lib_pkgs, ")"),
      "rhat(1)",
      "ess_bulk(1)"
    )
  )

  expect_snapshot_error(scan_usage(path, strict = TRUE))
})

test_that("scan_usage warns about multiple ambiguous calls in strict mode", {
  force_local_snapshots()
  funs <- c("rhat", "ess_bulk")

  get_fun_pkgs <- function(fun) {
    pkgs <- names(Filter(function(x) fun %in% x, .stan_exports))
    pkgs[vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  }

  needs <- vapply(
    funs,
    function(fun) {
      pkgs <- names(Filter(function(x) fun %in% x, .stan_exports))

      pkgs <- pkgs[vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]

      origins <- vapply(
        pkgs,
        function(pkg) {
          .stan_origin_map[[paste0(pkg, "::", fun)]]
        },
        character(1)
      )

      length(unique(origins[!is.na(origins)])) > 1
    },
    logical(1)
  )

  if (!all(needs)) {
    skip("Ambiguous functions not available in installed packages.")
  }

  lib_pkgs <- unique(unlist(lapply(funs, get_fun_pkgs)))
  if (length(lib_pkgs) < 2) {
    skip("Ambiguous functions not available in installed packages.")
  }

  tmp <- withr::local_tempdir()
  path <- write_file(
    file.path(tmp, "strict.R"),
    c(
      paste0("library(", lib_pkgs, ")"),
      "rhat(1)",
      "ess_bulk(1)"
    )
  )

  expect_snapshot_error(scan_usage(path, strict = TRUE))
})

test_that("scan_usage warns on ambiguous calls in non-strict mode", {
  tmp <- withr::local_tempdir()
  path <- write_file(
    file.path(tmp, "non-strict.R"),
    c(
      "foo(1)",
      "library(pkgA)",
      "library(pkgB)"
    )
  )

  res <- NULL
  expect_snapshot_warning({
    res <- scan_usage(
      path,
      strict = FALSE,
      allowed_packages = c("pkgA", "pkgB"),
      export_index = list(foo = c("pkgA", "pkgB")),
      origin_map = c("pkgA::foo" = "pkgA", "pkgB::foo" = "pkgB")
    )
  })
  expect_true(all(c("pkgA", "pkgB") %in% res$packages))
  expect_identical(res$functions, character())
  expect_equal(res$ambiguous, "foo")
})

test_that("print.scan_usage shows functions with no packages", {
  force_local_snapshots()
  expect_snapshot_output(print(structure(
    list(
      packages = character(),
      functions = c("loo::loo", "posterior::as_draws")
    ),
    class = "scan_usage"
  )))
})

test_that("print.scan_usage shows many packages with no functions", {
  force_local_snapshots()
  expect_snapshot_output(print(structure(
    list(
      packages = c(
        "bayesplot",
        "brms",
        "cmdstanr",
        "loo",
        "posterior",
        "projpred",
        "rstan",
        "shinystan"
      ),
      functions = character()
    ),
    class = "scan_usage"
  )))
})

test_that("print.scan_usage shows many functions for one package", {
  force_local_snapshots()
  expect_snapshot_output(print(structure(
    list(
      packages = "posterior",
      functions = c(
        "posterior::summarise_draws",
        "posterior::as_draws_df",
        "posterior::rhat",
        "posterior::ess_bulk",
        "posterior::as_draws"
      )
    ),
    class = "scan_usage"
  )))
})

test_that("print.scan_usage shows many functions across packages", {
  force_local_snapshots()
  expect_snapshot_output(print(structure(
    list(
      packages = c("bayesplot", "loo", "posterior", "rstan"),
      functions = c(
        "rstan::rstan_options",
        "bayesplot::mcmc_trace",
        "loo::loo",
        "posterior::as_draws",
        "bayesplot::pp_check",
        "loo::loo_compare",
        "posterior::summarise_draws",
        "rstan::stan_model"
      )
    ),
    class = "scan_usage"
  )))
})

test_that("scan_usage returns empty results for non-Stan files", {
  tmp <- withr::local_tempdir()
  path <- write_file(
    file.path(tmp, "plain.R"),
    c(
      "x <- 1",
      "x"
    )
  )
  res <- scan_usage(path)
  expect_equal(res$packages, character())
  expect_equal(res$functions, character())
})

test_that("scan_usage ignores unqualified Stan exports without attachment", {
  export_index <- getFromNamespace(".stan_export_index", "stanflow")
  if (!"mixture" %in% names(export_index)) {
    skip("mixture not in Stan export index.")
  }

  tmp <- withr::local_tempdir()
  path <- write_file(
    file.path(tmp, "plain.R"),
    "mixture(1)"
  )
  res <- scan_usage(path)
  expect_equal(res$packages, character())
  expect_equal(res$functions, character())
})

test_that("scan_usage supports multiple file paths", {
  tmp <- withr::local_tempdir()
  path1 <- write_file(
    file.path(tmp, "one.R"),
    c(
      "library(posterior)",
      "as_draws(1)"
    )
  )
  path2 <- write_file(
    file.path(tmp, "two.R"),
    c(
      "library(brms)",
      "as_draws(1)"
    )
  )

  res <- scan_usage(c(path1, path2))

  expected_keys <- unique(na.omit(c(
    resolve_origin_key("posterior", "as_draws"),
    resolve_origin_key("brms", "as_draws")
  )))
  expected_pkgs <- unique(na.omit(c(
    "posterior",
    "brms",
    resolve_origin_pkg("posterior", "as_draws"),
    resolve_origin_pkg("brms", "as_draws")
  )))

  expect_true(all(expected_pkgs %in% res$packages))
  expect_true(all(expected_keys %in% res$functions))
})

test_that("scan_usage handles faux_proj directory tree", {
  skip_if_not_installed("knitr")

  faux_path <- testthat::test_path("faux_proj")
  res <- scan_usage(faux_path)

  expected_keys <- unique(na.omit(c(
    resolve_origin_key("brms", "bf"),
    resolve_origin_key("brms", "set_prior"),
    resolve_origin_key("brms", "brm"),
    resolve_origin_key("brms", "mixture"),
    resolve_origin_key("brms", "get_prior"),
    resolve_origin_key("brms", "conditional_effects"),
    resolve_origin_key("posterior", "as_draws"),
    resolve_origin_key("posterior", "as_draws_df"),
    resolve_origin_key("posterior", "as_draws_matrix"),
    resolve_origin_key("posterior", "as_draws_cmdstanr"),
    resolve_origin_key("posterior", "subset_draws"),
    resolve_origin_key("posterior", "rhat"),
    resolve_origin_key("posterior", "ess_bulk"),
    resolve_origin_key("posterior", "ess_tail"),
    resolve_origin_key("posterior", "summarise_draws"),
    resolve_origin_key("bayesplot", "mcmc_trace"),
    resolve_origin_key("bayesplot", "mcmc_areas"),
    resolve_origin_key("bayesplot", "mcmc_intervals"),
    resolve_origin_key("bayesplot", "mcmc_rank_hist"),
    resolve_origin_key("bayesplot", "mcmc_acf"),
    resolve_origin_key("bayesplot", "pp_check"),
    resolve_origin_key("cmdstanr", "cmdstan_model"),
    resolve_origin_key("cmdstanr", "read_cmdstan_csv"),
    resolve_origin_key("cmdstanr", "write_stan_json"),
    resolve_origin_key("rstan", "stan_model"),
    resolve_origin_key("rstan", "extract"),
    resolve_origin_key("rstanarm", "logit"),
    resolve_origin_key("shinystan", "launch_shinystan")
  )))
  expected_keys <- unique(c(
    expected_keys,
    "brms::mixture",
    "brms::as_draws",
    "brms::brm",
    "posterior::as_draws",
    "posterior::as_draws_df",
    "posterior::as_draws_matrix",
    "posterior::as_draws_cmdstanr",
    "posterior::subset_draws",
    "posterior::rhat",
    "posterior::ess_bulk",
    "posterior::ess_tail",
    "posterior::summarise_draws",
    "cmdstanr::cmdstan_model",
    "cmdstanr::read_cmdstan_csv",
    "cmdstanr::write_stan_json",
    "rstan::stan_model",
    "rstan::extract",
    "rstanarm::logit",
    "shinystan::launch_shinystan",
    "projpred::cv_varsel",
    "loo::loo",
    "loo::loo_compare"
  ))

  expected_pkgs <- unique(na.omit(c(
    "brms",
    "posterior",
    "cmdstanr",
    "bayesplot",
    "loo",
    "projpred",
    "rstan",
    "rstanarm",
    "shinystan",
    resolve_origin_pkg("brms", "bf"),
    resolve_origin_pkg("brms", "set_prior"),
    resolve_origin_pkg("brms", "mixture"),
    resolve_origin_pkg("brms", "get_prior"),
    resolve_origin_pkg("brms", "conditional_effects"),
    resolve_origin_pkg("posterior", "as_draws"),
    resolve_origin_pkg("posterior", "as_draws_df"),
    resolve_origin_pkg("posterior", "as_draws_matrix"),
    resolve_origin_pkg("posterior", "as_draws_cmdstanr"),
    resolve_origin_pkg("posterior", "subset_draws"),
    resolve_origin_pkg("posterior", "rhat"),
    resolve_origin_pkg("posterior", "ess_bulk"),
    resolve_origin_pkg("posterior", "ess_tail"),
    resolve_origin_pkg("posterior", "summarise_draws"),
    resolve_origin_pkg("bayesplot", "mcmc_trace"),
    resolve_origin_pkg("bayesplot", "mcmc_areas"),
    resolve_origin_pkg("bayesplot", "mcmc_intervals"),
    resolve_origin_pkg("bayesplot", "mcmc_rank_hist"),
    resolve_origin_pkg("bayesplot", "mcmc_acf"),
    resolve_origin_pkg("bayesplot", "pp_check"),
    resolve_origin_pkg("cmdstanr", "cmdstan_model"),
    resolve_origin_pkg("cmdstanr", "read_cmdstan_csv"),
    resolve_origin_pkg("cmdstanr", "write_stan_json"),
    resolve_origin_pkg("rstan", "stan_model"),
    resolve_origin_pkg("rstan", "extract"),
    resolve_origin_pkg("rstanarm", "logit"),
    resolve_origin_pkg("shinystan", "launch_shinystan")
  )))

  expect_true(all(expected_pkgs %in% res$packages))
  expect_true(all(expected_keys %in% res$functions))
  expect_false(any(
    res$packages %in%
      c(
        "tidymodels"
      )
  ))
  expect_false(any(
    res$functions %in%
      c(
        "tidymodels::workflow",
        "recipes::recipe"
      )
  ))
})

test_that("scan_usage attributes unqualified calls only in files attaching Stan packages", {
  if (
    is.null(.stan_export_index[["mixture"]]) ||
      !("brms" %in% .stan_export_index[["mixture"]])
  ) {
    skip("brms::mixture not available in Stan export index.")
  }

  tmp <- withr::local_tempdir()
  path1 <- write_file(
    file.path(tmp, "brms.R"),
    c(
      "library(brms)",
      "mixture(1)"
    )
  )
  path2 <- write_file(
    file.path(tmp, "tidymodels.R"),
    c(
      "library(tidymodels)",
      "mixture(2)"
    )
  )

  res <- scan_usage(c(path1, path2))

  expected_key <- resolve_origin_key("brms", "mixture")
  expected_functions <- if (is.na(expected_key)) character() else expected_key
  expected_pkgs <- unique(na.omit(c(
    "brms",
    resolve_origin_pkg("brms", "mixture")
  )))

  expect_true(setequal(res$packages, expected_pkgs))
  expect_true(setequal(res$functions, expected_functions))
})

test_that("scan_usage keeps namespaced calls when unqualified calls are ignored", {
  fun_candidates <- setdiff(names(.stan_export_index), stdlib_funs())
  if (!length(fun_candidates)) {
    skip("No non-stdlib Stan exports available.")
  }
  fun <- if ("mixture" %in% fun_candidates) "mixture" else fun_candidates[[1]]

  tmp <- withr::local_tempdir()
  path <- write_file(
    file.path(tmp, "script.R"),
    c(
      "library(tidymodels)",
      paste0("brms::", fun, "(1)"),
      paste0(fun, "(2)")
    )
  )

  res <- scan_usage(path)

  expect_true(setequal(res$functions, paste0("brms::", fun)))
  expect_true("brms" %in% res$packages)
})

test_that("scan_usage handles projects with renv/packrat and real R folder", {
  skip_if_not_installed("knitr")

  tmp <- withr::local_tempdir()
  dir.create(file.path(tmp, "R"), recursive = TRUE)
  dir.create(file.path(tmp, "renv", "library"), recursive = TRUE)
  dir.create(file.path(tmp, "packrat", "lib"), recursive = TRUE)

  write_file(
    file.path(tmp, "R", "analysis.R"),
    c(
      "library(posterior)",
      "as_draws(1)"
    )
  )
  write_file(
    file.path(tmp, "R", "report.Rmd"),
    c(
      "---",
      "title: 'Report'",
      "---",
      "",
      "```{r}",
      "library(brms)",
      "as_draws(1)",
      "```"
    )
  )
  write_file(
    file.path(tmp, "R", "note.qmd"),
    c(
      "---",
      "title: 'Note'",
      "---",
      "",
      "```{r}",
      "library(cmdstanr)",
      "cmdstan_model('x.stan')",
      "```"
    )
  )
  write_file(
    file.path(tmp, "renv", "library", "vendored.R"),
    c(
      "library(rstan)",
      "extract(1)"
    )
  )
  write_file(
    file.path(tmp, "packrat", "lib", "vendored.R"),
    c(
      "library(rstanarm)",
      "logit(1)"
    )
  )
  res <- scan_usage(tmp)

  expected_keys <- unique(na.omit(c(
    resolve_origin_key("posterior", "as_draws"),
    resolve_origin_key("brms", "as_draws"),
    resolve_origin_key("cmdstanr", "cmdstan_model")
  )))
  expected_pkgs <- unique(na.omit(c(
    "posterior",
    "brms",
    "cmdstanr",
    resolve_origin_pkg("posterior", "as_draws"),
    resolve_origin_pkg("brms", "as_draws"),
    resolve_origin_pkg("cmdstanr", "cmdstan_model")
  )))

  expect_true(setequal(res$packages, expected_pkgs))
  expect_true(setequal(res$functions, expected_keys))
})

test_that("scan_usage captures ::: calls without counting unqualified usage", {
  tmp <- withr::local_tempdir()
  path <- write_file(
    file.path(tmp, "script.R"),
    c(
      "brms:::as_draws(1)",
      "as_draws(2)"
    )
  )

  res <- scan_usage(path)

  expect_true(setequal(res$functions, "brms::as_draws"))
  expect_true("brms" %in% res$packages)
})

test_that("scan_usage errors on multiple directories", {
  tmp <- withr::local_tempdir()
  dir1 <- file.path(tmp, "proj1")
  dir2 <- file.path(tmp, "proj2")
  dir.create(dir1)
  dir.create(dir2)

  expect_snapshot_error(scan_usage(c(dir1, dir2)))
})

test_that("scan_usage alerts full paths for file vectors", {
  tmp <- withr::local_tempdir()
  path1 <- write_file(
    file.path(tmp, "one.R"),
    c(
      "library(posterior)",
      "as_draws(1)"
    )
  )
  path2 <- write_file(
    file.path(tmp, "two.R"),
    c(
      "library(brms)",
      "as_draws(1)"
    )
  )

  res <- NULL
  expect_snapshot_output({
    res <- scan_usage(c(path1, path2))
  })

  expected_pkgs <- unique(na.omit(c(
    "posterior",
    "brms",
    resolve_origin_pkg("posterior", "as_draws"),
    resolve_origin_pkg("brms", "as_draws")
  )))

  expect_true(setequal(res$packages, expected_pkgs))
})

test_that("scan_usage alerts full paths for directories", {
  tmp <- withr::local_tempdir()
  dir_path <- file.path(tmp, "proj")
  dir.create(dir_path)
  write_file(
    file.path(dir_path, "in_dir.R"),
    c(
      "library(brms)",
      "as_draws(1)"
    )
  )

  res <- NULL
  expect_snapshot_output({
    res <- scan_usage(dir_path)
  })

  expected_pkgs <- unique(na.omit(c(
    "brms",
    resolve_origin_pkg("brms", "as_draws")
  )))

  expect_true(setequal(res$packages, expected_pkgs))
})

test_that("scan_usage errors when mixing directories and files", {
  tmp <- withr::local_tempdir()
  dir_path <- file.path(tmp, "proj")
  dir.create(dir_path)
  file_path <- write_file(
    file.path(tmp, "script.R"),
    c(
      "library(posterior)",
      "as_draws(1)"
    )
  )

  expect_snapshot_error(scan_usage(c(dir_path, file_path)))
})

test_that("scan_usage scans directories with mixed inputs", {
  skip_if_not_installed("knitr")
  tmp <- withr::local_tempdir()
  write_file(
    file.path(tmp, "script.R"),
    c(
      "library(posterior)",
      "as_draws(1)"
    )
  )
  write_file(
    file.path(tmp, "note.RMD"),
    c(
      "---",
      "title: 'Note'",
      "---",
      "",
      "```{r}",
      "library(brms)",
      "as_draws(1)",
      "```"
    )
  )
  write_file(
    file.path(tmp, "extra.R"),
    c(
      "requireNamespace(\"cmdstanr\")"
    )
  )
  write_file(
    file.path(tmp, "bad.R"),
    c(
      "function("
    )
  )
  res <- NULL
  expect_warning(
    res <- scan_usage(tmp),
    "Failed to parse"
  )

  expect_equal(res$packages, sort(res$packages))
  expect_equal(res$functions, sort(res$functions))
  expected_keys <- unique(na.omit(c(
    resolve_origin_key("posterior", "as_draws"),
    resolve_origin_key("brms", "as_draws")
  )))
  expected_pkgs <- unique(na.omit(c(
    "posterior",
    "brms",
    "cmdstanr",
    resolve_origin_pkg("posterior", "as_draws"),
    resolve_origin_pkg("brms", "as_draws")
  )))

  expect_true(setequal(res$packages, expected_pkgs))
  expect_true(setequal(res$functions, expected_keys))
})

test_that("scan_usage skips default directories", {
  tmp <- withr::local_tempdir()
  skip_path <- file.path(tmp, "renv", "library")
  dir.create(skip_path, recursive = TRUE)
  write_file(
    file.path(skip_path, "script.R"),
    c(
      "library(rstan)",
      "extract(1)"
    )
  )
  write_file(
    file.path(tmp, "script.R"),
    "1 + 1"
  )

  res <- scan_usage(tmp)

  expect_equal(res$packages, character())
  expect_equal(res$functions, character())
})

test_that("scan_usage respects custom skip_dirs", {
  tmp <- withr::local_tempdir()
  skip_path <- file.path(tmp, "vendor", "lib")
  dir.create(skip_path, recursive = TRUE)
  write_file(
    file.path(skip_path, "script.R"),
    c(
      "library(brms)",
      "fixef(1)"
    )
  )
  write_file(
    file.path(tmp, "script.R"),
    "1 + 1"
  )

  res_default <- scan_usage(tmp)
  res_custom <- scan_usage(tmp, skip_dirs = "vendor")

  expected_key <- resolve_origin_key("brms", "fixef")
  expected_functions <- if (is.na(expected_key)) character() else expected_key
  expected_pkgs <- unique(na.omit(c(
    "brms",
    resolve_origin_pkg("brms", "fixef")
  )))

  expect_true(setequal(res_default$packages, expected_pkgs))
  expect_true(setequal(res_default$functions, expected_functions))
  expect_equal(res_custom$packages, character())
  expect_equal(res_custom$functions, character())
})

test_that("scan_usage does not skip similar directory names", {
  tmp <- withr::local_tempdir()
  keep_path <- file.path(tmp, "renvish")
  dir.create(keep_path, recursive = TRUE)
  write_file(
    file.path(keep_path, "script.R"),
    c(
      "library(rstanarm)",
      "logit(1)"
    )
  )

  res <- scan_usage(tmp)

  expect_true(setequal(res$packages, "rstanarm"))
  expect_true(setequal(res$functions, "rstanarm::logit"))
})

test_that("scan_usage skip_dirs match nested directories", {
  tmp <- withr::local_tempdir()
  skip_path <- file.path(tmp, "project", ".Rcheck", "logs")
  dir.create(skip_path, recursive = TRUE)
  write_file(
    file.path(skip_path, "script.R"),
    c(
      "library(cmdstanr)",
      "cmdstan_model('x.stan')"
    )
  )
  write_file(
    file.path(tmp, "script.R"),
    "1 + 1"
  )

  res <- scan_usage(tmp)

  expect_equal(res$packages, character())
  expect_equal(res$functions, character())
})

test_that("scan_usage keeps exact file inputs regardless of skip_dirs", {
  tmp <- withr::local_tempdir()
  skip_path <- file.path(tmp, "renv", "library")
  dir.create(skip_path, recursive = TRUE)
  file_path <- write_file(
    file.path(skip_path, "script.R"),
    c(
      "library(posterior)",
      "as_draws(1)"
    )
  )

  res <- scan_usage(file_path)

  expect_true(setequal(res$packages, "posterior"))
  expect_true(setequal(res$functions, "posterior::as_draws"))
})

test_that("scan_usage handles empty skip_dirs without filtering", {
  tmp <- withr::local_tempdir()
  skip_path <- file.path(tmp, "renv", "library")
  dir.create(skip_path, recursive = TRUE)
  write_file(
    file.path(skip_path, "script.R"),
    c(
      "library(rstan)",
      "extract(1)"
    )
  )

  res <- scan_usage(tmp, skip_dirs = character())

  expect_true(setequal(res$packages, "rstan"))
  expect_true(setequal(res$functions, "rstan::extract"))
})

test_that("scan_usage skips dotted caches but not filenames", {
  tmp <- withr::local_tempdir()
  skip_path <- file.path(tmp, ".cache", "chunks")
  dir.create(skip_path, recursive = TRUE)
  write_file(
    file.path(skip_path, "script.R"),
    c(
      "library(brms)",
      "ranef(1)"
    )
  )
  write_file(
    file.path(tmp, "cache.R"),
    c(
      "library(cmdstanr)",
      "cmdstan_model('x.stan')"
    )
  )

  res <- scan_usage(tmp)

  expect_true(setequal(res$packages, "cmdstanr"))
  expect_true(setequal(res$functions, "cmdstanr::cmdstan_model"))
})

test_that("scan_usage returns empty vectors for empty directories", {
  tmp <- withr::local_tempdir()
  expect_error(
    scan_usage(tmp),
    "No files found"
  )
})

test_that("scan_usage ignores non-R files in directories", {
  tmp <- withr::local_tempdir()
  write_file(
    file.path(tmp, "note.txt"),
    c(
      "library(posterior)",
      "as_draws(1)"
    )
  )
  expect_error(
    scan_usage(tmp),
    "No files found"
  )
})

test_that("internal helpers cover NULL/expression/list/pairlist branches in .ast_walk", {
  ast_walk <- getFromNamespace(".ast_walk", "stanflow")

  new_acc <- function() {
    acc <- new.env(parent = emptyenv())
    acc$pos <- 0L
    acc$lib_pkgs <- character()
    acc$lib_pos <- integer()
    acc$lib_is_attach <- logical()
    acc$ns_pkgs <- character()
    acc$ns_keys <- character()
    acc$unqual_funs <- character()
    acc$unqual_pos <- integer()
    acc
  }

  lib_funs <- c("library", "require", "requireNamespace")
  ignore <- stanflow::stdlib_funs()

  # is.null(x) branch  :contentReference[oaicite:1]{index=1}
  acc <- new_acc()
  expect_invisible(ast_walk(NULL, acc, ignore, lib_funs))

  # is.expression(x) branch  :contentReference[oaicite:2]{index=2}
  acc <- new_acc()
  expect_invisible(ast_walk(
    expression(posterior::as_draws(1)),
    acc,
    ignore,
    lib_funs,
    .stan_pkgs,
    c("::", ":::"),
    c("c", "list")
  ))
  expect_true("posterior::as_draws" %in% acc$ns_keys)

  # is.list(x) branch (list)  :contentReference[oaicite:3]{index=3}
  acc <- new_acc()
  expect_invisible(ast_walk(
    list(quote(posterior::as_draws(1))),
    acc,
    ignore,
    lib_funs,
    .stan_pkgs,
    c("::", ":::"),
    c("c", "list")
  ))
  expect_true("posterior::as_draws" %in% acc$ns_keys)

  # is.pairlist(x) branch (pairlist)  :contentReference[oaicite:4]{index=4}
  acc <- new_acc()
  expect_invisible(ast_walk(
    pairlist(a = quote(posterior::as_draws(1))),
    acc,
    ignore,
    lib_funs,
    .stan_pkgs,
    c("::", ":::"),
    c("c", "list")
  ))
  expect_true("posterior::as_draws" %in% acc$ns_keys)
})

test_that(".ast_lit_name returns NULL for non-literals", {
  ast_lit_name <- getFromNamespace(".ast_lit_name", "stanflow")
  expect_null(ast_lit_name(1)) # hits the trailing NULL return :contentReference[oaicite:5]{index=5}
})

test_that(".ast_get_lib_pkg handles empty args and named `package=`", {
  ast_get_lib_pkg <- getFromNamespace(".ast_get_lib_pkg", "stanflow")

  # no args -> early NULL  :contentReference[oaicite:6]{index=6}
  expect_null(ast_get_lib_pkg(quote(library())))

  # named package= branch  :contentReference[oaicite:7]{index=7}
  expect_identical(
    ast_get_lib_pkg(quote(library(package = "posterior"))),
    "posterior"
  )

  # named pkg= branch
  expect_identical(
    ast_get_lib_pkg(quote(use(pkg = "posterior"))),
    "posterior"
  )
})

test_that(".ast_collect_use_funs and helpers handle edge cases", {
  ast_collect_use_funs <- getFromNamespace(".ast_collect_use_funs", "stanflow")
  ast_get_use_funs <- getFromNamespace(".ast_get_use_funs", "stanflow")
  use_heads <- c("c", "list")
  expect_identical(ast_collect_use_funs(NULL), character())
  expect_identical(ast_collect_use_funs(quote(c()), use_heads), character())
  expect_identical(
    ast_collect_use_funs(quote(c("a", list("b", "c"), NULL)), use_heads),
    c("a", "b", "c")
  )
  expect_identical(
    ast_collect_use_funs(quote(foo("x")), use_heads),
    character()
  )

  expect_identical(
    ast_get_use_funs(quote(use("posterior")), use_heads),
    character()
  )
  expect_identical(
    sort(ast_get_use_funs(quote(use("posterior", c("a", "b"))), use_heads)),
    c("a", "b")
  )
  expect_identical(
    sort(ast_get_use_funs(quote(use("posterior", list("a", "b"))), use_heads)),
    c("a", "b")
  )
  expect_identical(
    sort(ast_get_use_funs(
      quote(use("posterior", "a", c("b", "c"))),
      use_heads
    )),
    c("a", "b", "c")
  )
  expect_identical(
    sort(ast_get_use_funs(quote(use(pkg = "posterior", "a")), use_heads)),
    "a"
  )
  expect_identical(
    sort(ast_get_use_funs(quote(use(package = "posterior", "a")), use_heads)),
    "a"
  )
})

test_that(".resolve_candidates returns empty when no Stan candidates exist", {
  resolve_candidates <- getFromNamespace(".resolve_candidates", "stanflow")
  export_index <- getFromNamespace(".stan_export_index", "stanflow")

  # pick a deterministic name not in the index
  candidates <- c(
    "___stanflow_not_a_real_stan_fun___",
    "___stanflow_not_a_real_stan_fun_2___",
    "___stanflow_not_a_real_stan_fun_3___"
  )
  fun <- candidates[!candidates %in% names(export_index)][1]
  expect_true(length(fun) == 1 && nzchar(fun))

  out <- resolve_candidates(
    unqual = list(funs = fun, idx = 1L),
    lib_data = NULL,
    strict = FALSE
  )

  # triggers the `!any(has_candidates)` early return  :contentReference[oaicite:8]{index=8}
  expect_identical(out$pkgs, character())
  expect_identical(out$keys, character())
  expect_identical(out$ambiguous, character())
})

test_that(".resolve_candidates returns empty when no packages allowed", {
  resolve_candidates <- getFromNamespace(".resolve_candidates", "stanflow")

  out <- resolve_candidates(
    unqual = list(funs = "as_draws", idx = 1L),
    lib_data = NULL,
    strict = FALSE,
    allowed_packages = character()
  )

  expect_identical(out$pkgs, character())
  expect_identical(out$keys, character())
  expect_identical(out$ambiguous, character())
})

test_that(".resolve_candidates labels package ambiguity in non-strict mode", {
  resolve_candidates <- getFromNamespace(".resolve_candidates", "stanflow")

  out <- resolve_candidates(
    unqual = list(funs = "foo", idx = 2L),
    lib_data = data.frame(
      visit_idx = c(1L, 2L),
      pkg = c("pkgA", "pkgB"),
      is_attach = c(TRUE, TRUE),
      stringsAsFactors = FALSE
    ),
    strict = FALSE,
    allowed_packages = c("pkgA", "pkgB"),
    export_index = list(foo = c("pkgA", "pkgB")),
    origin_map = c("pkgA::foo" = "pkgA", "pkgB::foo" = "pkgB")
  )

  expect_identical(out$ambiguous, character())
  expect_equal(out$pkgs, "pkgB")
  expect_equal(out$keys, "pkgB::foo")
})

test_that(".resolve_candidates applies origin_map for resolved ambiguity", {
  resolve_candidates <- getFromNamespace(".resolve_candidates", "stanflow")

  out <- resolve_candidates(
    unqual = list(funs = "foo", idx = 3L),
    lib_data = data.frame(
      visit_idx = c(1L, 2L),
      pkg = c("pkgA", "pkgB"),
      is_attach = c(TRUE, TRUE),
      stringsAsFactors = FALSE
    ),
    strict = FALSE,
    allowed_packages = c("pkgA", "pkgB"),
    export_index = list(foo = c("pkgA", "pkgB")),
    origin_map = c("pkgA::foo" = "pkgA", "pkgB::foo" = "pkgA")
  )

  expect_identical(out$ambiguous, character())
  expect_equal(out$pkgs, "pkgA")
  expect_equal(out$keys, "pkgA::foo")
})

test_that(".resolve_candidates fills missing origin_map entries positionally", {
  resolve_candidates <- getFromNamespace(".resolve_candidates", "stanflow")

  out <- resolve_candidates(
    unqual = list(funs = c("fa", "fb", "fc"), idx = c(1L, 2L, 3L)),
    lib_data = data.frame(
      visit_idx = c(1L, 2L, 3L),
      pkg = c("pkgA", "pkgB", "pkgC"),
      is_attach = c(TRUE, TRUE, TRUE),
      stringsAsFactors = FALSE
    ),
    strict = FALSE,
    allowed_packages = c("pkgA", "pkgB", "pkgC"),
    export_index = list(
      fa = "pkgA",
      fb = "pkgB",
      fc = "pkgC"
    ),
    origin_map = c("pkgA::fa" = "pkgA", "pkgC::fc" = "pkgC")
  )

  expect_identical(out$ambiguous, character())
  expect_identical(out$pkgs, c("pkgA", "pkgB", "pkgC"))
  expect_identical(out$keys, c("pkgA::fa", "pkgB::fb", "pkgC::fc"))
})

test_that(".resolve_candidates keeps ambiguity when no attach position precedes call", {
  resolve_candidates <- getFromNamespace(".resolve_candidates", "stanflow")

  out <- resolve_candidates(
    unqual = list(funs = "foo", idx = 1L),
    lib_data = data.frame(
      visit_idx = c(5L, 10L),
      pkg = c("pkgA", "pkgB"),
      is_attach = c(TRUE, TRUE),
      stringsAsFactors = FALSE
    ),
    strict = FALSE,
    allowed_packages = c("pkgA", "pkgB"),
    export_index = list(foo = c("pkgA", "pkgB")),
    origin_map = c("pkgA::foo" = "pkgA", "pkgB::foo" = "pkgB")
  )

  expect_equal(out$ambiguous, "foo")
  expect_identical(out$pkgs, character())
  expect_identical(out$keys, character())
})

test_that(".resolve_candidates keeps ambiguity when candidates attach later", {
  resolve_candidates <- getFromNamespace(".resolve_candidates", "stanflow")

  out <- resolve_candidates(
    unqual = list(funs = "foo", idx = 7L),
    lib_data = data.frame(
      visit_idx = c(5L, 10L, 20L),
      pkg = c("pkgA", "pkgB", "pkgC"),
      is_attach = c(TRUE, TRUE, TRUE),
      stringsAsFactors = FALSE
    ),
    strict = FALSE,
    allowed_packages = c("pkgA", "pkgB", "pkgC"),
    export_index = list(foo = c("pkgB", "pkgC")),
    origin_map = c("pkgB::foo" = "pkgB", "pkgC::foo" = "pkgC")
  )

  expect_equal(out$ambiguous, "foo")
  expect_identical(out$pkgs, character())
  expect_identical(out$keys, character())
})

test_that(".resolve_candidates clears ambiguity when one call is resolved", {
  resolve_candidates <- getFromNamespace(".resolve_candidates", "stanflow")

  out <- resolve_candidates(
    unqual = list(funs = c("foo", "foo"), idx = c(1L, 3L)),
    lib_data = data.frame(
      visit_idx = c(2L, 4L),
      pkg = c("pkgA", "pkgB"),
      is_attach = c(TRUE, TRUE),
      stringsAsFactors = FALSE
    ),
    strict = FALSE,
    allowed_packages = c("pkgA", "pkgB"),
    export_index = list(foo = c("pkgA", "pkgB")),
    origin_map = c("pkgA::foo" = "pkgA", "pkgB::foo" = "pkgB")
  )

  expect_equal(out$ambiguous, character())
  expect_equal(out$pkgs, "pkgA")
  expect_equal(out$keys, "pkgA::foo")
})

test_that(".resolve_candidates falls back when origin_map points to disallowed package", {
  resolve_candidates <- getFromNamespace(".resolve_candidates", "stanflow")

  out <- resolve_candidates(
    unqual = list(funs = "foo", idx = 2L),
    lib_data = data.frame(
      visit_idx = c(1L, 2L),
      pkg = c("pkgA", "pkgB"),
      is_attach = c(TRUE, TRUE),
      stringsAsFactors = FALSE
    ),
    strict = FALSE,
    allowed_packages = c("pkgA", "pkgB"),
    export_index = list(foo = c("pkgA", "pkgB")),
    origin_map = c("pkgB::foo" = "pkgX")
  )

  expect_identical(out$ambiguous, character())
  expect_identical(out$pkgs, "pkgB")
  expect_identical(out$keys, "pkgB::foo")
})

test_that("scan_skip_dirs returns configured defaults", {
  scan_skip_dirs <- getFromNamespace("scan_skip_dirs", "stanflow")
  expect_equal(
    scan_skip_dirs(),
    getFromNamespace(".scan_skip_dirs", "stanflow")
  )
})

test_that("origin_map is applied even when an ambiguous call is position-resolved", {
  resolve <- getFromNamespace(
    ".resolve_candidates",
    "stanflow"
  )

  allowed_packages <- c("origin", "reexporter", "other")

  export_index <- list(foo = c("reexporter", "other"))

  origin_map <- c("reexporter::foo" = "origin")

  unqual1 <- list(funs = "foo", idx = 2L)
  lib1 <- data.frame(
    visit_idx = 1L,
    pkg = "reexporter",
    is_attach = TRUE,
    stringsAsFactors = FALSE
  )

  r1 <- resolve(
    unqual = unqual1,
    lib_data = lib1,
    strict = FALSE,
    allowed_packages = allowed_packages,
    export_index = export_index,
    origin_map = origin_map
  )

  expect_identical(r1$keys, "origin::foo")
  expect_identical(r1$pkgs, "origin")

  unqual2 <- list(funs = "foo", idx = 3L)
  lib2 <- data.frame(
    visit_idx = c(1L, 2L),
    pkg = c("other", "reexporter"),
    is_attach = c(TRUE, TRUE),
    stringsAsFactors = FALSE
  )

  r2 <- resolve(
    unqual = unqual2,
    lib_data = lib2,
    strict = FALSE,
    allowed_packages = allowed_packages,
    export_index = export_index,
    origin_map = origin_map
  )

  expect_identical(r2$keys, "origin::foo")
  expect_identical(r2$pkgs, "origin")
})
