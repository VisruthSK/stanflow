write_file <- function(path, lines) {
  writeLines(lines, path, useBytes = TRUE)
  path
}

test_that(".scan_tokens returns empty results for parse errors", {
  hits <- .scan_tokens("function(", stdlib_funs())
  expect_equal(hits$pkgs, character())
  expect_equal(hits$keys, character())
})

test_that(".scan_tokens handles empty library calls", {
  hits <- .scan_tokens("library()", stdlib_funs())
  expect_equal(hits$pkgs, character())
  expect_equal(hits$keys, character())
})

test_that(".scan_tokens resolves attachment order and requireNamespace", {
  code <- c(
    "library(posterior)",
    "requireNamespace(cmdstanr)",
    "library('brms')",
    "`as_draws`(1)"
  )
  hits <- .scan_tokens(paste(code, collapse = "\n"), stdlib_funs())

  expect_true(all(c("posterior", "cmdstanr", "brms") %in% hits$pkgs))
  expect_true("brms::as_draws" %in% hits$keys)
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
  tmp <- withr::local_tempdir()
  path <- write_file(
    file.path(tmp, "strict.R"),
    c(
      "as_draws_df(1)",
      "loo(1)"
    )
  )

  res <- stan_scan_usage(path)
  res_strict <- stan_scan_usage(path, strict = TRUE)

  expect_true(length(res$functions) == 2L)
  expect_equal(res_strict$packages, character())
  expect_equal(res_strict$functions, character())
})

test_that("stan_scan_usage warns about multiple ambiguous calls in strict mode", {
  tmp <- withr::local_tempdir()
  path <- write_file(
    file.path(tmp, "strict.R"),
    c(
      "as_draws_df(1)",
      "loo(1)"
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

test_that("print.stan_scan_usage reports empty usage", {
  expect_snapshot_output(print(structure(
    list(packages = character(), functions = character()),
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

test_that("stan_scan_usage applies ignore patterns for directory searches", {
  tmp <- withr::local_tempdir()
  dir_path <- file.path(tmp, "proj")
  dir.create(dir_path)
  dir.create(file.path(dir_path, "data"))
  dir.create(file.path(dir_path, "sub"))

  write_file(
    file.path(dir_path, "keep.R"),
    c(
      "library(posterior)",
      "as_draws(1)"
    )
  )
  write_file(
    file.path(dir_path, "skip.R"),
    c(
      "library(brms)",
      "as_draws(1)"
    )
  )
  write_file(
    file.path(dir_path, "data", "skip.R"),
    c(
      "library(cmdstanr)"
    )
  )
  write_file(
    file.path(dir_path, "data", "keep.R"),
    c(
      "library(brms)",
      "as_draws(1)"
    )
  )
  write_file(
    file.path(dir_path, "sub", "skip.R"),
    c(
      "library(rstanarm)"
    )
  )

  ignore_path <- write_file(
    file.path(tmp, "ignore.txt"),
    c(
      "# comment",
      "",
      "!",
      "/",
      "/data/",
      "skip.R",
      "data/",
      "data/cache/",
      "!data/keep.R",
      "sub/*.R"
    )
  )

  res <- stan_scan_usage(dir_path, ignore_files = ignore_path)

  expect_true(setequal(res$packages, c("posterior", "brms")))
  expect_true(setequal(
    res$functions,
    c("posterior::as_draws", "brms::as_draws")
  ))
})

test_that("stan_scan_usage respects anchored ignore patterns", {
  tmp <- withr::local_tempdir()
  dir_path <- file.path(tmp, "proj")
  dir.create(dir_path)
  dir.create(file.path(dir_path, "sub"))

  write_file(
    file.path(dir_path, "skip_root.R"),
    c(
      "library(posterior)"
    )
  )
  write_file(
    file.path(dir_path, "sub", "skip_root.R"),
    c(
      "library(brms)"
    )
  )

  ignore_path <- write_file(
    file.path(tmp, "ignore-root.txt"),
    c(
      "/skip_root.R"
    )
  )

  res <- stan_scan_usage(dir_path, ignore_files = ignore_path)
  expect_true(setequal(res$packages, "brms"))
})

test_that("stan_scan_usage errors when ignore is used with file vectors", {
  tmp <- withr::local_tempdir()
  path <- write_file(
    file.path(tmp, "script.R"),
    c(
      "library(posterior)",
      "as_draws(1)"
    )
  )
  ignore_path <- write_file(
    file.path(tmp, "ignore.txt"),
    c(
      "*.R"
    )
  )

  expect_error(
    stan_scan_usage(path, ignore_files = ignore_path),
    "ignore"
  )
})

test_that("stan_scan_usage keeps files when ignore file is empty", {
  tmp <- withr::local_tempdir()
  dir_path <- file.path(tmp, "proj")
  dir.create(dir_path)
  write_file(
    file.path(dir_path, "script.R"),
    c(
      "library(posterior)",
      "as_draws(1)"
    )
  )
  ignore_path <- write_file(
    file.path(tmp, "ignore-empty.txt"),
    c(
      "# comment only",
      "",
      "   "
    )
  )

  res <- stan_scan_usage(dir_path, ignore_files = ignore_path)
  expect_true(setequal(res$packages, "posterior"))
  expect_true(setequal(res$functions, "posterior::as_draws"))
})

test_that("stan_scan_usage handles empty directories with ignore files", {
  tmp <- withr::local_tempdir()
  dir_path <- file.path(tmp, "proj")
  dir.create(dir_path)
  ignore_path <- write_file(
    file.path(tmp, "ignore-empty.txt"),
    c(
      "*.R"
    )
  )

  expect_error(
    stan_scan_usage(dir_path, ignore_files = ignore_path),
    "No files found"
  )
})

test_that("stan_scan_usage errors when ignore file is missing", {
  tmp <- withr::local_tempdir()
  dir_path <- file.path(tmp, "proj")
  dir.create(dir_path)

  expect_error(
    stan_scan_usage(dir_path, ignore_files = file.path(tmp, "missing")),
    "ignore"
  )
})
