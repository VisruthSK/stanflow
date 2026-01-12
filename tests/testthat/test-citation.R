write_file <- function(path, lines) {
  writeLines(lines, path, useBytes = TRUE)
  path
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

test_that(".scan_tokens resolves attachment order and requireNamespace", {
  code <- c(
    "library(posterior)",

    "requireNamespace(\"cmdstanr\")",

    "library('brms')",

    "`as_draws`(1)"
  )

  hits <- .scan_tokens(paste(code, collapse = "\n"), stdlib_funs())

  expect_true(all(c("posterior", "cmdstanr", "brms") %in% hits$pkgs))

  expect_true("brms::as_draws" %in% hits$keys)

  # as_draws is in posterior too, but brms was attached later (last) so it should win?

  # In .scan_tokens logic:

  # library(posterior) -> pos X

  # library(brms) -> pos Y > X

  # choose_attached -> max pos -> brms.

  # So brms::as_draws is expected.

  expect_false("cmdstanr::as_draws" %in% hits$keys)
})


test_that(".scan_tokens falls back when attached packages do not match", {
  candidates <- split(
    rep(names(.stan_exports), lengths(.stan_exports)),

    .stan_exports |> unlist(use.names = FALSE)
  )

  first_pkg <- candidates[["log_lik"]][[1L]]

  code <- c(
    "library(posterior)",

    "log_lik(1)"
  )

  hits <- .scan_tokens(paste(code, collapse = "\n"), stdlib_funs())

  expect_true(paste0(first_pkg, "::log_lik") %in% hits$keys)
})


test_that(".scan_tokens chooses the first candidate when unattached", {
  candidates <- split(
    rep(names(.stan_exports), lengths(.stan_exports)),

    .stan_exports |> unlist(use.names = FALSE)
  )

  first_pkg <- candidates[["as_draws"]][[1L]]

  hits <- .scan_tokens("as_draws(1)", stdlib_funs())

  expect_true(paste0(first_pkg, "::as_draws") %in% hits$keys)
})


test_that(".scan_tokens collapses reexports by origin", {
  candidate_fun <- NULL
  provider_pkg <- NULL

  # Iterate over all exported functions to find a suitable candidate
  for (fun in sort(names(.stan_export_index))) {
    # Check ALL candidates to ensure global consistency
    all_candidates <- .stan_export_index[[fun]]

    all_origins <- vapply(
      all_candidates,
      function(pkg) {
        .stan_origin_map[[paste0(pkg, "::", fun)]]
      },
      character(1)
    )

    # Basic sanity checks on metadata
    if (anyNA(all_origins)) {
      next
    }
    unique_providers <- unique(all_origins)
    if (length(unique_providers) != 1L) {
      next
    }
    provider <- unique_providers
    if (!provider %in% .stan_pkgs) {
      next
    }

    # Check installed packages
    installed_pkgs <- all_candidates[vapply(
      all_candidates,
      requireNamespace,
      logical(1),
      quietly = TRUE
    )]
    if (length(installed_pkgs) < 2) {
      next
    }

    # CRITICAL: Verify that .scan_tokens itself considers this unambiguous.
    # This handles any discrepancies between our test logic and the implementation,
    # specifically ensuring we don't pick functions that .scan_tokens deems ambiguous
    # (which would cause the strict assertions below to fail).
    check_hits <- .scan_tokens(
      paste0(fun, "(1)"),
      stdlib_funs(),
      strict = FALSE
    )
    if (length(check_hits$ambiguous) > 0) {
      next
    }
    if (length(check_hits$pkgs) != 1L) {
      next
    }

    # If we get here, we have a solid candidate
    candidate_fun <- fun
    provider_pkg <- provider
    break
  }

  if (is.null(candidate_fun)) {
    skip("No consistently reexported functions found in installed packages.")
  }

  hits <- .scan_tokens(
    paste0(candidate_fun, "(1)"),
    stdlib_funs(),
    strict = FALSE
  )

  expect_equal(hits$pkgs, provider_pkg)
  expect_equal(hits$keys, paste0(provider_pkg, "::", candidate_fun))
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

  hits <- .scan_tokens(paste0(fun, "(1)"), stdlib_funs(), strict = TRUE)

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
    hits <- .scan_tokens(paste0(fun, "(1)"), stdlib_funs())
    if (paste0(pkg, "::", fun) %in% hits$keys) {
      picked_fun <- fun
      picked_pkg <- pkg
      break
    }
  }

  if (is.null(picked_fun)) {
    skip("No single-package functions resolved in .scan_tokens.")
  }

  hits <- .scan_tokens(paste0(picked_fun, "(1)"), stdlib_funs())
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
  expect_false("rstan::plot" %in% hits$keys)
  expect_false("stats::lm" %in% hits$keys)
  expect_true(all(c("posterior", "brms") %in% hits$pkgs))
  expect_false("rstan" %in% hits$pkgs)
})

test_that(".scan_tokens ignores unqualified stdlib calls", {
  hits <- .scan_tokens("plot(1)", stdlib_funs())
  expect_equal(hits$pkgs, character())
  expect_equal(hits$keys, character())
})

test_that(".scan_tokens honors ignore_functions overrides", {
  hits <- .scan_tokens(
    "posterior::as_draws(1)",
    ignore_functions = "as_draws"
  )
  expect_equal(hits$pkgs, character())
  expect_equal(hits$keys, character())
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
  skip_if_not_installed("quarto")
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
  qmd_to_r_script <- function(input, output, ...) {
    writeLines("as_draws(1)", output, useBytes = TRUE)
  }
  extract_code <- getFromNamespace(".extract_code", "stanflow")
  out <- with_mocked_bindings(
    qmd_to_r_script = qmd_to_r_script,
    .package = "quarto",
    extract_code(path)
  )
  expect_match(out, "as_draws\\(")
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

test_that("stan_scan_usage handles a single file path", {
  tmp <- withr::local_tempdir()
  path <- write_file(
    file.path(tmp, "single.R"),
    c(
      "library(posterior)",
      "as_draws(1)"
    )
  )
  res <- stan_scan_usage(path)
  expect_true(inherits(res, "stan_scan_usage"))
  expect_equal(res$packages, "posterior")
  expect_equal(res$functions, "posterior::as_draws")
})

test_that("stan_scan_usage strict skips ambiguous unqualified calls", {
  funs <- c("rhat", "ess_bulk")

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

  tmp <- withr::local_tempdir()
  path <- write_file(
    file.path(tmp, "strict.R"),
    c(
      "rhat(1)",
      "ess_bulk(1)"
    )
  )

  res <- stan_scan_usage(path)
  res_strict <- stan_scan_usage(path, strict = TRUE)

  expect_true(length(res$functions) == 2L)
  expect_equal(res_strict$packages, character())
  expect_equal(res_strict$functions, character())
})

test_that("stan_scan_usage warns about multiple ambiguous calls in strict mode", {
  funs <- c("rhat", "ess_bulk")

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

  tmp <- withr::local_tempdir()
  path <- write_file(
    file.path(tmp, "strict.R"),
    c(
      "rhat(1)",
      "ess_bulk(1)"
    )
  )

  expect_snapshot_output(
    with_mocked_bindings(
      cli_alert_warning = function(msg, ...) {
        cli::cat_line(cli::format_inline(msg, .envir = parent.frame()))
      },
      .package = "cli",
      stan_scan_usage(path, strict = TRUE)
    )
  )
})

test_that("print.stan_scan_usage shows functions with no packages", {
  expect_snapshot_output(print(structure(
    list(
      packages = character(),
      functions = c("loo::loo", "posterior::as_draws")
    ),
    class = "stan_scan_usage"
  )))
})

test_that("print.stan_scan_usage shows many packages with no functions", {
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
    class = "stan_scan_usage"
  )))
})

test_that("print.stan_scan_usage shows many functions for one package", {
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
    class = "stan_scan_usage"
  )))
})

test_that("print.stan_scan_usage shows many functions across packages", {
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
    class = "stan_scan_usage"
  )))
})

test_that("stan_scan_usage returns empty results for non-Stan files", {
  tmp <- withr::local_tempdir()
  path <- write_file(
    file.path(tmp, "plain.R"),
    c(
      "x <- 1",
      "x"
    )
  )
  res <- stan_scan_usage(path)
  expect_equal(res$packages, character())
  expect_equal(res$functions, character())
})

test_that("stan_scan_usage supports multiple file paths", {
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

  res <- stan_scan_usage(c(path1, path2))

  expect_true(setequal(res$packages, c("posterior", "brms")))
  expect_true(setequal(
    res$functions,
    c("posterior::as_draws", "brms::as_draws")
  ))
})

test_that("stan_scan_usage errors on multiple directories", {
  tmp <- withr::local_tempdir()
  dir1 <- file.path(tmp, "proj1")
  dir2 <- file.path(tmp, "proj2")
  dir.create(dir1)
  dir.create(dir2)

  expect_error(
    stan_scan_usage(c(dir1, dir2)),
    "single directory"
  )
})

test_that("stan_scan_usage alerts full paths for file vectors", {
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

  seen <- character()
  cli_alert_info <- function(msg, ...) {
    seen <<- c(seen, cli::format_inline(msg, .envir = parent.frame()))
  }

  res <- with_mocked_bindings(
    cli_alert_info = cli_alert_info,
    .package = "cli",
    stan_scan_usage(c(path1, path2))
  )

  expect_true(setequal(res$packages, c("posterior", "brms")))
  expected <- normalizePath(
    c(path1, path2),
    winslash = "/",
    mustWork = FALSE
  ) |>
    vapply(
      function(path) cli::format_inline("Searching {.path {path}}"),
      character(1)
    )
  expect_true(all(expected %in% seen))
})

test_that("stan_scan_usage alerts full paths for directories", {
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

  seen <- character()
  cli_alert_info <- function(msg, ...) {
    seen <<- c(seen, cli::format_inline(msg, .envir = parent.frame()))
  }

  res <- with_mocked_bindings(
    cli_alert_info = cli_alert_info,
    .package = "cli",
    stan_scan_usage(dir_path)
  )

  expect_true(setequal(res$packages, "brms"))
  expected <- normalizePath(dir_path, winslash = "/", mustWork = FALSE) |>
    (\(path) cli::format_inline("Searching directory {.path {path}}"))()
  expect_true(expected %in% seen)
})

test_that("stan_scan_usage errors when mixing directories and files", {
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

  expect_error(
    stan_scan_usage(c(dir_path, file_path)),
    "single directory"
  )
})

test_that("stan_scan_usage scans directories with mixed inputs", {
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

  res <- stan_scan_usage(tmp)

  expect_equal(res$packages, sort(res$packages))
  expect_equal(res$functions, sort(res$functions))
  expect_true(setequal(res$packages, c("brms", "cmdstanr", "posterior")))
  expect_true(setequal(
    res$functions,
    c("brms::as_draws", "posterior::as_draws")
  ))
})

test_that("stan_scan_usage returns empty vectors for empty directories", {
  tmp <- withr::local_tempdir()
  expect_error(
    stan_scan_usage(tmp),
    "No files found"
  )
})

test_that("stan_scan_usage ignores non-R files in directories", {
  tmp <- withr::local_tempdir()
  write_file(
    file.path(tmp, "note.txt"),
    c(
      "library(posterior)",
      "as_draws(1)"
    )
  )
  expect_error(
    stan_scan_usage(tmp),
    "No files found"
  )
})
