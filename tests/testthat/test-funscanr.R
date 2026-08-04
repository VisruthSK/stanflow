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
.scan_tokens <- bind_internal(".scan_tokens")
.extract_code <- bind_internal(".extract_code")
.extract_markdown_code <- bind_internal(".extract_markdown_code")
.ast_member_fun <- bind_internal(".ast_member_fun")
.stan_exports <- bind_internal(".stan_exports")
.stan_export_index <- bind_internal(".stan_export_index")
.stan_origin_map <- bind_internal(".stan_origin_map")
.stan_pkgs <- bind_internal(".stan_pkgs")
.stan_core <- bind_internal("core")

scan_usage_pkg <- getExportedValue("stanflow", "scan_usage")
scan_usage <- function(
  ...,
  allowed_packages = .stan_pkgs,
  export_index = .stan_export_index,
  origin_map = .stan_origin_map,
  metapackages = list(stanflow = .stan_core)
) {
  scan_usage_pkg(
    ...,
    allowed_packages = allowed_packages,
    export_index = export_index,
    origin_map = origin_map,
    metapackages = metapackages
  )
}

expect_scan_usage_knitr_parity <- function(path, ..., info = NULL) {
  skip_if_not_installed("knitr")

  fast <- scan_usage(path, ..., quiet = TRUE, use_knitr = FALSE)
  knitr <- scan_usage(path, ..., quiet = TRUE, use_knitr = TRUE)

  expect_identical(unclass(fast), unclass(knitr), info = info)
}

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

test_that(".stan_origin_map has complete keys and valid origins", {
  all_funs <- unlist(.stan_exports, use.names = FALSE)
  providers <- rep(names(.stan_exports), lengths(.stan_exports))
  keys <- paste0(providers, "::", all_funs)

  expect_true(length(keys) > 0)
  expect_true(all(keys %in% names(.stan_origin_map)))

  mapped <- unname(.stan_origin_map[keys])
  expect_false(anyNA(mapped))
  expect_true(all(nzchar(mapped)))
})

test_that("default index resolves an indexed cmdstanr member call", {
  fun <- "sample"
  expect_true(
    !is.null(.stan_export_index[[fun]]) &&
      "cmdstanr" %in% .stan_export_index[[fun]]
  )

  code <- c(
    "library(cmdstanr)",
    paste0("fit$", fun, "()")
  )

  hits <- .scan_tokens(
    paste(code, collapse = "\n"),
    stdlib_funs(),
    allowed_packages = .stan_pkgs,
    export_index = .stan_export_index,
    origin_map = .stan_origin_map
  )

  expected_key <- resolve_origin_key("cmdstanr", fun)
  if (is.na(expected_key)) {
    expected_key <- paste0("cmdstanr::", fun)
  }

  expect_true(expected_key %in% hits$keys)
  expect_true("cmdstanr" %in% hits$pkgs)
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
  expect_equal(
    .scan_tokens(
      "",
      stdlib_funs(),
      allowed_packages = "posterior",
      export_index = unname(list("posterior")),
      origin_map = character()
    ),
    list(pkgs = character(), keys = character(), ambiguous = character())
  )
})

test_that(".ast_member_fun returns NULL when call operator is not a symbol", {
  malformed <- as.call(list(1, quote(fit), quote(sample)))
  expect_null(.ast_member_fun(malformed))
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

test_that(".scan_tokens handles empty resolver exports", {
  code <- c(
    "library(posterior)",
    "as_draws(1)"
  )
  hits <- .scan_tokens(
    paste(code, collapse = "\n"),
    ignore_unqualified_functions = character(),
    allowed_packages = "posterior",
    export_index = unname(list()),
    origin_map = character()
  )

  expect_identical(hits$pkgs, "posterior")
  expect_identical(hits$keys, character())
  expect_identical(hits$ambiguous, character())
})

test_that(".scan_resolver_index keeps empty provider entries null", {
  scan_resolver_index <- getFromNamespace(".scan_resolver_index", "stanflow")

  out <- scan_resolver_index(
    export_index = list(foo = character(), bar = "pkgA"),
    origin_map = c("pkgA::bar" = "pkgA")
  )

  expect_null(out$foo)
  expect_identical(out$bar$provider, "pkgA")
  expect_identical(out$bar$origin, "pkgA")
})

test_that(".scan_tokens expands metapackages for unqualified resolution", {
  hits <- .scan_tokens(
    "library(meta)\nfoo(1)",
    ignore_unqualified_functions = character(),
    allowed_packages = "pkgA",
    export_index = list(foo = "pkgA"),
    origin_map = c("pkgA::foo" = "pkgA"),
    metapackages = list(meta = "pkgA")
  )

  expect_true(all(hits$pkgs == "pkgA"))
  expect_identical(hits$keys, "pkgA::foo")
  expect_identical(hits$ambiguous, character())
})

test_that(".normalize_metapackages returns null for null input", {
  normalize_metapackages <- getFromNamespace(
    ".normalize_metapackages",
    "stanflow"
  )

  expect_null(normalize_metapackages(NULL, "pkgA"))
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

test_that(".scan_tokens detects cmdstanr R6 methods from the vignette", {
  export_index <- list(
    cmdstan_model = "cmdstanr",
    sample = "cmdstanr",
    draws = "cmdstanr",
    sampler_diagnostics = "cmdstanr",
    diagnostic_summary = "cmdstanr",
    optimize = "cmdstanr",
    laplace = "cmdstanr",
    variational = "cmdstanr",
    pathfinder = "cmdstanr",
    save_object = "cmdstanr"
  )
  origin_map <- c(
    "cmdstanr::cmdstan_model" = "cmdstanr",
    "cmdstanr::sample" = "cmdstanr",
    "cmdstanr::draws" = "cmdstanr",
    "cmdstanr::sampler_diagnostics" = "cmdstanr",
    "cmdstanr::diagnostic_summary" = "cmdstanr",
    "cmdstanr::optimize" = "cmdstanr",
    "cmdstanr::laplace" = "cmdstanr",
    "cmdstanr::variational" = "cmdstanr",
    "cmdstanr::pathfinder" = "cmdstanr",
    "cmdstanr::save_object" = "cmdstanr"
  )

  code <- c(
    "library(cmdstanr)",
    "mod <- cmdstan_model('model.stan')",
    "fit <- mod$sample(data = list(N = 10, y = rnorm(10)))",
    "fit$draws()",
    "fit$sampler_diagnostics(format = 'df')",
    "fit$diagnostic_summary()",
    "fit$save_object(file = 'fit.RDS')",
    "mod$optimize(data = list(N = 10, y = rnorm(10)))",
    "mod$laplace(mode = fit, draws = 100)",
    "mod$variational(data = list(N = 10, y = rnorm(10)), draws = 100)",
    "mod$pathfinder(data = list(N = 10, y = rnorm(10)), draws = 100)",
    "fit$output_files",
    "fit@metadata"
  )

  hits <- .scan_tokens(
    paste(code, collapse = "\n"),
    stdlib_funs(),
    allowed_packages = "cmdstanr",
    export_index = export_index,
    origin_map = origin_map
  )

  expect_true("cmdstanr" %in% hits$pkgs)
  expect_equal(
    hits$keys,
    c(
      "cmdstanr::cmdstan_model",
      "cmdstanr::sample",
      "cmdstanr::draws",
      "cmdstanr::sampler_diagnostics",
      "cmdstanr::diagnostic_summary",
      "cmdstanr::save_object",
      "cmdstanr::optimize",
      "cmdstanr::laplace",
      "cmdstanr::variational",
      "cmdstanr::pathfinder"
    )
  )
  expect_identical(hits$ambiguous, character())
})

test_that(".scan_tokens thoroughly detects invoked R6 member methods", {
  export_index <- list(
    sample_fit = "pkgA",
    draws_df = "pkgA",
    is_alive = "pkgB",
    terminate = "pkgB",
    diagnose_fit = "pkgA",
    collect_metrics = "pkgC",
    emit_report = "pkgC"
  )
  origin_map <- c(
    "pkgA::sample_fit" = "pkgA",
    "pkgA::draws_df" = "pkgA",
    "pkgB::is_alive" = "pkgB",
    "pkgB::terminate" = "pkgB",
    "pkgA::diagnose_fit" = "pkgA",
    "pkgC::collect_metrics" = "pkgC",
    "pkgC::emit_report" = "pkgC"
  )

  code <- c(
    "library(pkgA)",
    "library(pkgB)",
    "library(pkgC)",
    "model$sample_fit(data = list(N = 10))",
    "fit$draws_df()",
    "proc$is_alive()",
    "proc$terminate()",
    "fit$diagnose_fit()",
    "monitor$collect_metrics()",
    "report$emit_report(format = 'html')",
    "fit$output_files",
    "proc@private"
  )

  hits <- .scan_tokens(
    paste(code, collapse = "\n"),
    stdlib_funs(),
    allowed_packages = c("pkgA", "pkgB", "pkgC"),
    export_index = export_index,
    origin_map = origin_map
  )

  expect_true(all(c("pkgA", "pkgB", "pkgC") %in% hits$pkgs))
  expect_equal(
    hits$keys,
    c(
      "pkgA::sample_fit",
      "pkgA::draws_df",
      "pkgB::is_alive",
      "pkgB::terminate",
      "pkgA::diagnose_fit",
      "pkgC::collect_metrics",
      "pkgC::emit_report"
    )
  )
  expect_identical(hits$ambiguous, character())
})

test_that(".scan_tokens resolves ambiguous invoked member methods by attachment order", {
  code <- c(
    "library(pkgA)",
    "library(pkgB)",
    "library(pkgC)",
    "obj$train_model(1)"
  )

  hits <- .scan_tokens(
    paste(code, collapse = "\n"),
    stdlib_funs(),
    allowed_packages = c("pkgA", "pkgB", "pkgC"),
    export_index = list(train_model = c("pkgA", "pkgB", "pkgC")),
    origin_map = c(
      "pkgA::train_model" = "pkgA",
      "pkgB::train_model" = "pkgB",
      "pkgC::train_model" = "pkgC"
    )
  )

  expect_true(all(c("pkgA", "pkgB", "pkgC") %in% hits$pkgs))
  expect_equal(hits$keys, "pkgC::train_model")
  expect_identical(hits$ambiguous, character())
})

test_that(".scan_tokens resolves attachment-ordered member methods in strict mode", {
  code <- c(
    "library(pkgA)",
    "library(pkgB)",
    "library(pkgC)",
    "obj$train_model(1)"
  )

  hits <- .scan_tokens(
    paste(code, collapse = "\n"),
    stdlib_funs(),
    allowed_packages = c("pkgA", "pkgB", "pkgC"),
    export_index = list(train_model = c("pkgA", "pkgB", "pkgC")),
    origin_map = c(
      "pkgA::train_model" = "pkgA",
      "pkgB::train_model" = "pkgB",
      "pkgC::train_model" = "pkgC"
    )
  )

  expect_true(all(c("pkgA", "pkgB", "pkgC") %in% hits$pkgs))
  expect_equal(hits$keys, "pkgC::train_model")
  expect_identical(hits$ambiguous, character())
})

test_that(".scan_tokens keeps invoked member methods ambiguous in strict mode when no attachment resolves them", {
  code <- c(
    "obj$train_model(1)",
    "library(pkgA)",
    "library(pkgB)",
    "library(pkgC)"
  )

  hits <- .scan_tokens(
    paste(code, collapse = "\n"),
    stdlib_funs(),
    allowed_packages = c("pkgA", "pkgB", "pkgC"),
    export_index = list(train_model = c("pkgA", "pkgB", "pkgC")),
    origin_map = c(
      "pkgA::train_model" = "pkgA",
      "pkgB::train_model" = "pkgB",
      "pkgC::train_model" = "pkgC"
    )
  )

  expect_true(all(c("pkgA", "pkgB", "pkgC") %in% hits$pkgs))
  expect_equal(hits$keys, character())
  expect_equal(hits$ambiguous, "train_model")
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

  code <- c(paste0(fun, "(1)"), paste0("library(", pkgs, ")"))
  hits <- .scan_tokens(
    paste(code, collapse = "\n"),
    stdlib_funs(),
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
  expect_snapshot(out)
})

test_that(".extract_code extracts Qmd chunks", {
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
  expect_snapshot(out)
})

test_that(".extract_code handles chunk options and tilde fences", {
  tmp <- withr::local_tempdir()
  path <- write_file(
    file.path(tmp, "doc.Rmd"),
    c(
      "---",
      "title: 'Doc'",
      "---",
      "",
      "~~~{r setup, include=FALSE}",
      "as_draws(1)",
      "~~~",
      "",
      "````{r fig.width=8}",
      "rhat(1)",
      "````"
    )
  )

  out <- .extract_code(path)
  expect_snapshot(out)
})

test_that(".extract_code returns empty string when file has no allowed packages", {
  tmp <- withr::local_tempdir()
  path <- write_file(
    file.path(tmp, "doc.Rmd"),
    c(
      "---",
      "title: 'Doc'",
      "---",
      "",
      "```{r}",
      "mean(1:3)",
      "```"
    )
  )

  expect_identical(
    .extract_code(path, allowed_packages = c("posterior", "loo")),
    ""
  )
})

test_that(".extract_code early skip respects word boundaries and regex escaping", {
  tmp <- withr::local_tempdir()

  no_boundary <- write_file(
    file.path(tmp, "no-boundary.R"),
    "foobar(1)"
  )
  expect_identical(.extract_code(no_boundary, allowed_packages = "foo"), "")

  dotted <- write_file(
    file.path(tmp, "dotted.R"),
    "foo.bar(1)"
  )
  expect_identical(
    .extract_code(dotted, allowed_packages = "foo.bar"),
    "foo.bar(1)"
  )
})

test_that(".extract_code returns empty for empty allowed_packages", {
  tmp <- withr::local_tempdir()
  path <- write_file(file.path(tmp, "plain.R"), "posterior::as_draws_df(x)")

  expect_identical(.extract_code(path, allowed_packages = character()), "")
})

test_that(".extract_code keeps fast-extracted non-R display chunks in default mode", {
  tmp <- withr::local_tempdir()
  path <- write_file(
    file.path(tmp, "doc.Rmd"),
    c(
      "```{r, eval=FALSE}",
      "/**",
      " * not R code",
      " */",
      "```"
    )
  )

  out <- .extract_code(path)
  expect_snapshot(out)
})

test_that(".extract_code uses knitr when requested", {
  skip_if_not_installed("knitr")
  tmp <- withr::local_tempdir()
  path <- write_file(
    file.path(tmp, "doc.Rmd"),
    c(
      "```{r}",
      "as_draws(1)",
      "```"
    )
  )

  out <- .extract_code(path, use_knitr = TRUE)
  expect_snapshot(out)
})

test_that(".extract_code matches knitr::purl on ordinary Rmd documents", {
  skip_if_not_installed("knitr")
  tmp <- withr::local_tempdir()
  path <- write_file(
    file.path(tmp, "doc.Rmd"),
    c(
      "---",
      "title: 'Doc'",
      "---",
      "",
      "```{r setup, include=FALSE}",
      "library(posterior)",
      "```",
      "",
      "```{r model}",
      "draws <- as_draws(list(mu = rnorm(10)))",
      "rhat(draws)",
      "```"
    )
  )

  fast <- .extract_code(path)
  knitr <- .extract_code(path, use_knitr = TRUE)

  expect_identical(
    parse(text = fast, keep.source = FALSE),
    parse(text = knitr, keep.source = FALSE)
  )
})

test_that(".extract_code matches knitr::purl on ordinary Qmd documents", {
  skip_if_not_installed("knitr")
  tmp <- withr::local_tempdir()
  path <- write_file(
    file.path(tmp, "doc.qmd"),
    c(
      "---",
      "title: 'Doc'",
      "---",
      "",
      "```{r setup, include=FALSE}",
      "library(posterior)",
      "```",
      "",
      "```{r}",
      "draws <- as_draws(list(mu = rnorm(10)))",
      "rhat(draws)",
      "```"
    )
  )

  fast <- .extract_code(path)
  knitr <- .extract_code(path, use_knitr = TRUE)

  expect_identical(
    parse(text = fast, keep.source = FALSE),
    parse(text = knitr, keep.source = FALSE)
  )
})

test_that("scan_usage matches knitr on ordinary Rmd and Qmd fixtures", {
  skip_if_not_installed("knitr")
  tmp <- withr::local_tempdir()

  fixtures <- list(
    basic = c(
      "---",
      "title: 'Doc'",
      "---",
      "",
      "```{r setup, include=FALSE}",
      "library(posterior)",
      "requireNamespace('loo')",
      "```",
      "",
      "```{r}",
      "draws <- as_draws(list(mu = rnorm(10)))",
      "rhat(draws)",
      "loo::loo(matrix(1))",
      "```"
    ),
    attach_order = c(
      "---",
      "title: 'Doc'",
      "---",
      "",
      "```{r}",
      "library(posterior)",
      "library(brms)",
      "```",
      "",
      "```{r}",
      "as_draws(1)",
      "mixture(0.4)",
      "posterior::rhat(as_draws(list(mu = rnorm(10))))",
      "```"
    ),
    explicit_and_member = c(
      "---",
      "title: 'Doc'",
      "---",
      "",
      "```{r setup, include=FALSE}",
      "library(cmdstanr)",
      "use('posterior', c('as_draws', 'rhat'))",
      "```",
      "",
      "```{r}",
      "fit$summary()",
      "fit$loo(moment_match = TRUE)",
      "posterior::as_draws(list(mu = rnorm(10)))",
      "```"
    ),
    stanflow_meta = c(
      "---",
      "title: 'Doc'",
      "---",
      "",
      "```{r}",
      "library(stanflow)",
      "as_draws_df(matrix(1))",
      "mcmc_hist(as_draws_df(matrix(1)), pars = 'theta')",
      "loo(matrix(1))",
      "```"
    ),
    irrelevant = c(
      "---",
      "title: 'Doc'",
      "---",
      "",
      "```{r echo=FALSE}",
      "mean(1:3)",
      "plot(1:5)",
      "```"
    )
  )

  for (ext in c("Rmd", "qmd")) {
    for (name in names(fixtures)) {
      path <- write_file(
        file.path(tmp, paste0(name, ".", ext)),
        fixtures[[name]]
      )
      expect_scan_usage_knitr_parity(path, info = paste(ext, name))
    }
  }
})

test_that(".extract_code errors when use_knitr is true and knitr is unavailable", {
  tmp <- withr::local_tempdir()
  path <- write_file(
    file.path(tmp, "doc.Rmd"),
    c(
      "```{r}",
      "as_draws(1)",
      "```"
    )
  )

  local_mocked_bindings(
    requireNamespace = function(...) FALSE,
    .package = "base"
  )

  expect_error(.extract_code(path, use_knitr = TRUE), "knitr")
})

test_that(".extract_code default mode does not depend on knitr for invalid extracted code", {
  tmp <- withr::local_tempdir()
  path <- write_file(
    file.path(tmp, "doc.Rmd"),
    c(
      "```{r, eval=FALSE}",
      "/**",
      " * not R code",
      " */",
      "```"
    )
  )

  out <- .extract_code(path)
  expect_snapshot(out)
})

test_that(".extract_code errors on unsupported extensions", {
  tmp <- withr::local_tempdir()
  path <- write_file(file.path(tmp, "note.txt"), "x <- 1")
  expect_snapshot_error(.extract_code(path))
})

test_that(".extract_markdown_code handles empty and non-R fences", {
  expect_identical(.extract_markdown_code(character()), "")
  expect_identical(.extract_markdown_code("plain text"), "")
  expect_identical(
    .extract_markdown_code(c("plain text", "~~~{python}", "x = 1", "~~~")),
    ""
  )
})

test_that(".extract_markdown_code skips non-closing fence candidates inside chunks", {
  out <- .extract_markdown_code(c(
    "```{r}",
    "x <- 1",
    "```{python}",
    "print('not a close fence')",
    "```"
  ))

  expect_snapshot(out)
})

test_that(".scan_tokens warns on parse errors with unknown file path", {
  expect_warning(
    res <- .scan_tokens("function(", stdlib_funs()),
    "Failed to parse"
  )
  expect_identical(res$pkgs, character())
  expect_identical(res$keys, character())
  expect_identical(res$ambiguous, character())
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
  res <- scan_usage(path, quiet = TRUE)
  expect_true(inherits(res, "scan_usage"))
  expect_equal(res$packages, "posterior")
  expect_equal(res$functions, "posterior::as_draws")
})

test_that("scan_usage handles modern syntax and Windows line endings", {
  expected_key <- resolve_origin_key("posterior", "as_draws")

  tmp <- withr::local_tempdir()
  path <- file.path(tmp, "syntax.R")
  writeLines(
    c(
      "library(posterior)",
      "note <- 'as_draws(1)'",
      "# as_draws(2)",
      "`as_draws`(3)",
      "1 |> as_draws()",
      "(\\(x) as_draws(x))(1)",
      "text <- \"caf\\u00e9\""
    ),
    path,
    sep = "\r\n",
    useBytes = TRUE
  )

  res <- scan_usage(path, quiet = TRUE)
  expect_true("posterior" %in% res$packages)
  expect_equal(res$functions, expected_key)
})

test_that("scan_usage skips irrelevant parse errors in strict mode", {
  tmp <- withr::local_tempdir()
  path <- write_file(
    file.path(tmp, "bad.R"),
    c(
      "function("
    )
  )

  res <- scan_usage(path, strict = TRUE, quiet = TRUE)
  expect_identical(res$packages, character())
  expect_identical(res$functions, character())
  expect_identical(res$ambiguous, character())
})

test_that("scan_usage warns on parse errors in relevant files", {
  tmp <- withr::local_tempdir()
  path <- write_file(
    file.path(tmp, "bad.R"),
    c(
      "library(posterior",
      "as_draws(1)"
    )
  )

  warn <- NULL
  res <- withCallingHandlers(
    scan_usage(path, strict = TRUE, quiet = TRUE),
    warning = function(w) {
      warn <<- conditionMessage(w)
      invokeRestart("muffleWarning")
    }
  )

  expect_match(warn, "Failed to parse")
  expect_identical(res$packages, character())
  expect_identical(res$functions, character())
  expect_identical(res$ambiguous, character())
})

test_that("scan_usage strict aborts on unresolved unqualified calls", {
  tmp <- withr::local_tempdir()
  path <- write_file(
    file.path(tmp, "strict.R"),
    c(
      "foo(1)",
      "library(pkgA)",
      "library(pkgB)"
    )
  )

  expect_error(
    scan_usage(
      path,
      strict = TRUE,
      quiet = TRUE,
      allowed_packages = c("pkgA", "pkgB"),
      export_index = list(foo = c("pkgA", "pkgB")),
      origin_map = c("pkgA::foo" = "pkgA", "pkgB::foo" = "pkgB")
    ),
    "Ambiguous functions"
  )
})

test_that("scan_usage strict respects attachment order when the winner is known", {
  path <- write_file(
    file.path(withr::local_tempdir(), "strict-order.R"),
    c(
      "library(pkgA)",
      "library(pkgB)",
      "foo(1)"
    )
  )

  warns <- character()
  res <- withCallingHandlers(
    scan_usage(
      path,
      strict = TRUE,
      quiet = TRUE,
      allowed_packages = c("pkgA", "pkgB"),
      export_index = list(foo = c("pkgA", "pkgB")),
      origin_map = c("pkgA::foo" = "pkgA", "pkgB::foo" = "pkgB")
    ),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  expect_identical(warns, character())
  expect_true(all(c("pkgA", "pkgB") %in% res$packages))
  expect_equal(res$functions, "pkgB::foo")
  expect_identical(res$ambiguous, character())
})

test_that("scan_usage strict remaps a later-attached reexporter to the origin package", {
  path <- write_file(
    file.path(withr::local_tempdir(), "strict-reexport.R"),
    c(
      "library(pkgA)",
      "library(pkgB)",
      "foo(1)"
    )
  )

  warns <- character()
  res <- withCallingHandlers(
    scan_usage(
      path,
      strict = TRUE,
      quiet = TRUE,
      allowed_packages = c("pkgA", "pkgB"),
      export_index = list(foo = c("pkgA", "pkgB")),
      origin_map = c("pkgA::foo" = "pkgA", "pkgB::foo" = "pkgA")
    ),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  expect_identical(warns, character())
  expect_true(all(c("pkgA", "pkgB") %in% res$packages))
  expect_equal(res$functions, "pkgA::foo")
  expect_identical(res$ambiguous, character())
})

test_that("scan_usage strict aborts on every unresolved unqualified call", {
  tmp <- withr::local_tempdir()
  path <- write_file(
    file.path(tmp, "strict.R"),
    c(
      "foo(1)",
      "bar(1)",
      "library(pkgA)",
      "library(pkgB)",
      "library(pkgC)"
    )
  )

  err <- tryCatch(
    scan_usage(
      path,
      strict = TRUE,
      quiet = TRUE,
      allowed_packages = c("pkgA", "pkgB", "pkgC"),
      export_index = list(
        foo = c("pkgA", "pkgB"),
        bar = c("pkgB", "pkgC")
      ),
      origin_map = c(
        "pkgA::foo" = "pkgA",
        "pkgB::foo" = "pkgB",
        "pkgB::bar" = "pkgB",
        "pkgC::bar" = "pkgC"
      )
    ),
    error = identity
  )

  expect_s3_class(err, "error")
  expect_match(conditionMessage(err), "foo")
  expect_match(conditionMessage(err), "bar")
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

  warn <- NULL
  res <- withCallingHandlers(
    scan_usage(
      path,
      strict = FALSE,
      quiet = TRUE,
      allowed_packages = c("pkgA", "pkgB"),
      export_index = list(foo = c("pkgA", "pkgB")),
      origin_map = c("pkgA::foo" = "pkgA", "pkgB::foo" = "pkgB")
    ),
    warning = function(w) {
      warn <<- conditionMessage(w)
      invokeRestart("muffleWarning")
    }
  )
  expect_match(warn, "Ambiguous functions")
  expect_match(warn, "foo")
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
  res <- scan_usage(path, quiet = TRUE)
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
  res <- scan_usage(path, quiet = TRUE)
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

  res <- scan_usage(c(path1, path2), quiet = TRUE)

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
  faux_path <- test_path("faux_proj")
  res <- scan_usage(faux_path, quiet = TRUE)

  expected_cmdstanr_funs <- sort(c(
    "cmdstanr::cmdstan_model",
    "cmdstanr::sample",
    "cmdstanr::print",
    "cmdstanr::exe_file",
    "cmdstanr::draws",
    "cmdstanr::summary",
    "cmdstanr::diagnostic_summary",
    "cmdstanr::pathfinder",
    "cmdstanr::read_cmdstan_csv",
    "cmdstanr::write_stan_json"
  ))

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
    resolve_origin_key("cmdstanr", "print"),
    resolve_origin_key("cmdstanr", "exe_file"),
    resolve_origin_key("cmdstanr", "draws"),
    resolve_origin_key("cmdstanr", "summary"),
    resolve_origin_key("cmdstanr", "diagnostic_summary"),
    resolve_origin_key("cmdstanr", "pathfinder"),
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
    "cmdstanr::print",
    "cmdstanr::exe_file",
    "cmdstanr::draws",
    "cmdstanr::summary",
    "cmdstanr::diagnostic_summary",
    "cmdstanr::pathfinder",
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
    resolve_origin_pkg("cmdstanr", "print"),
    resolve_origin_pkg("cmdstanr", "exe_file"),
    resolve_origin_pkg("cmdstanr", "draws"),
    resolve_origin_pkg("cmdstanr", "summary"),
    resolve_origin_pkg("cmdstanr", "diagnostic_summary"),
    resolve_origin_pkg("cmdstanr", "pathfinder"),
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

  detected_cmdstanr_funs <- sort(res$functions[grepl(
    "^cmdstanr::",
    res$functions
  )])
  expect_equal(detected_cmdstanr_funs, expected_cmdstanr_funs)

  force_local_snapshots()
  expect_snapshot_value(
    list(
      packages = res$packages,
      functions = res$functions
    ),
    style = "json2"
  )
})

test_that("scan_usage matches knitr on faux_proj directory tree", {
  expect_scan_usage_knitr_parity(
    test_path("faux_proj"),
    info = "faux_proj"
  )
})

test_that("scan_usage attributes unqualified calls only in files attaching Stan packages", {
  if (
    is.null(.stan_export_index[["mixture"]]) ||
      !"brms" %in% .stan_export_index[["mixture"]]
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

  res <- scan_usage(c(path1, path2), quiet = TRUE)

  expected_key <- resolve_origin_key("brms", "mixture")
  expected_functions <- if (is.na(expected_key)) character() else expected_key
  expected_pkgs <- unique(na.omit(c(
    "brms",
    resolve_origin_pkg("brms", "mixture")
  )))

  expect_true(setequal(res$packages, expected_pkgs))
  expect_true(setequal(res$functions, expected_functions))
})

test_that("scan_usage treats stanflow attachment as core packages", {
  tmp <- withr::local_tempdir()
  path <- write_file(
    file.path(tmp, "stanflow.R"),
    c(
      "library(stanflow)",
      "as_draws_df(matrix(1))",
      "mcmc_hist(as_draws_df(matrix(1)), pars = 'theta')",
      "loo(matrix(1))"
    )
  )

  res <- scan_usage(path, quiet = TRUE)

  expected_keys <- unique(na.omit(c(
    resolve_origin_key("posterior", "as_draws_df"),
    resolve_origin_key("bayesplot", "mcmc_hist"),
    resolve_origin_key("loo", "loo")
  )))
  expected_pkgs <- unique(na.omit(c(
    "posterior",
    "bayesplot",
    "loo",
    resolve_origin_pkg("posterior", "as_draws_df"),
    resolve_origin_pkg("bayesplot", "mcmc_hist"),
    resolve_origin_pkg("loo", "loo")
  )))

  expect_true(all(expected_pkgs %in% res$packages))
  expect_true(all(expected_keys %in% res$functions))
})

test_that("scan_usage treats require(stanflow) as core attachment", {
  tmp <- withr::local_tempdir()
  path <- write_file(
    file.path(tmp, "stanflow-require.R"),
    c(
      "require(stanflow)",
      "as_draws_df(matrix(1))",
      "loo(matrix(1))"
    )
  )

  res <- scan_usage(path, quiet = TRUE)

  expected_keys <- unique(na.omit(c(
    resolve_origin_key("posterior", "as_draws_df"),
    resolve_origin_key("loo", "loo")
  )))
  expected_pkgs <- unique(na.omit(c(
    "posterior",
    "loo",
    resolve_origin_pkg("posterior", "as_draws_df"),
    resolve_origin_pkg("loo", "loo")
  )))

  expect_true(all(expected_pkgs %in% res$packages))
  expect_true(all(expected_keys %in% res$functions))
})

test_that("scan_usage does not treat requireNamespace(stanflow) as core attachment", {
  tmp <- withr::local_tempdir()
  path <- write_file(
    file.path(tmp, "stanflow-namespace.R"),
    c(
      "requireNamespace('stanflow')",
      "loo(matrix(1))"
    )
  )

  res <- scan_usage(path, quiet = TRUE)

  expect_true("stanflow" %in% res$packages)
  expect_false(any(
    res$packages %in%
      c("bayesplot", "loo", "posterior", "projpred", "shinystan")
  ))
  expect_identical(res$functions, character())
})

test_that("scan_usage handles stanflow attachment in qmd", {
  tmp <- withr::local_tempdir()
  path <- write_file(
    file.path(tmp, "note.qmd"),
    c(
      "---",
      "title: 'Note'",
      "---",
      "",
      "```{r}",
      "library(stanflow)",
      "loo(matrix(1))",
      "```"
    )
  )

  res <- scan_usage(path, quiet = TRUE)

  expected_key <- resolve_origin_key("loo", "loo")
  expected_keys <- if (is.na(expected_key)) character() else expected_key
  expected_pkgs <- unique(na.omit(c(
    "loo",
    resolve_origin_pkg("loo", "loo")
  )))

  expect_true(all(expected_pkgs %in% res$packages))
  expect_true(all(expected_keys %in% res$functions))
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

  res <- scan_usage(path, quiet = TRUE)

  expect_true(setequal(res$functions, paste0("brms::", fun)))
  expect_true("brms" %in% res$packages)
})

test_that("scan_usage handles projects with renv/packrat and real R folder", {
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
  res <- scan_usage(tmp, quiet = TRUE)

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

  res <- scan_usage(path, quiet = TRUE)

  expect_true(setequal(res$functions, "brms::as_draws"))
  expect_true("brms" %in% res$packages)
})

test_that("scan_usage errors on multiple directories", {
  tmp <- withr::local_tempdir()
  dir1 <- file.path(tmp, "proj1")
  dir2 <- file.path(tmp, "proj2")
  dir.create(dir1)
  dir.create(dir2)

  expect_snapshot_error(scan_usage(c(dir1, dir2), quiet = TRUE))
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

  withr::local_output_sink(withr::local_tempfile())
  res <- scan_usage(c(path1, path2), quiet = TRUE)

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

  withr::local_output_sink(withr::local_tempfile())
  res <- scan_usage(dir_path, quiet = TRUE)

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

  expect_snapshot_error(scan_usage(c(dir_path, file_path), quiet = TRUE))
})

test_that("scan_usage scans directories with mixed inputs", {
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
  res <- scan_usage(tmp, quiet = TRUE)

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

  res <- scan_usage(tmp, quiet = TRUE)

  expect_equal(res$packages, character())
  expect_equal(res$functions, character())
})

test_that(".scan_dir_files prunes skipped directories during traversal", {
  tmp <- withr::local_tempdir()
  keep_path <- file.path(tmp, "R")
  skip_path <- file.path(tmp, "renv", "library")
  dir.create(keep_path, recursive = TRUE)
  dir.create(skip_path, recursive = TRUE)

  keep_file <- write_file(file.path(keep_path, "analysis.R"), "1 + 1")
  write_file(file.path(skip_path, "vendored.R"), "2 + 2")

  visited <- character()
  base_list_files <- base::list.files

  local_mocked_bindings(
    list.files = function(path, ...) {
      dots <- list(...)
      if (isTRUE(dots$recursive)) {
        stop("recursive traversal reached base::list.files()")
      }
      visited <<- c(
        visited,
        normalizePath(path, winslash = "/", mustWork = TRUE)
      )
      base_list_files(path, ...)
    },
    .package = "base"
  )

  out <- .scan_dir_files(tmp, "renv")

  expect_equal(out, normalizePath(keep_file, winslash = "/", mustWork = FALSE))
  expect_true(normalizePath(tmp, winslash = "/", mustWork = TRUE) %in% visited)
  expect_true(
    normalizePath(keep_path, winslash = "/", mustWork = TRUE) %in% visited
  )
  expect_false(
    normalizePath(file.path(tmp, "renv"), winslash = "/", mustWork = TRUE) %in%
      visited
  )
  expect_false(skip_path %in% visited)
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

  res_default <- scan_usage(tmp, quiet = TRUE)
  res_custom <- scan_usage(tmp, skip_dirs = "vendor", quiet = TRUE)

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

  res <- scan_usage(tmp, quiet = TRUE)

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

  res <- scan_usage(tmp, quiet = TRUE)

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

  res <- scan_usage(file_path, quiet = TRUE)

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

  res <- scan_usage(tmp, skip_dirs = character(), quiet = TRUE)

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

  res <- scan_usage(tmp, quiet = TRUE)

  expect_true(setequal(res$packages, "cmdstanr"))
  expect_true(setequal(res$functions, "cmdstanr::cmdstan_model"))
})

test_that("scan_usage returns empty vectors for empty directories", {
  tmp <- withr::local_tempdir()
  expect_error(
    scan_usage(tmp, quiet = TRUE),
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
    scan_usage(tmp, quiet = TRUE),
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

  # is.null(x) branch
  acc <- new_acc()
  expect_invisible(ast_walk(NULL, acc, ignore, lib_funs))

  # is.expression(x) branch
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

  # is.list(x) branch (list)
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

  # is.pairlist(x) branch (pairlist)
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
  expect_null(ast_lit_name(1)) # hits the trailing NULL return
})

test_that(".ast_get_lib_pkg handles empty args and named `package=`", {
  ast_get_lib_pkg <- getFromNamespace(".ast_get_lib_pkg", "stanflow")

  # no args -> early NULL
  expect_null(ast_get_lib_pkg(quote(library())))

  # named package= branch
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
    lib_data = data.frame(
      visit_idx = 1L,
      pkg = "posterior",
      is_attach = TRUE,
      stringsAsFactors = FALSE
    )
  )

  # missing index entry -> no metadata built for the function
  expect_identical(out$pkgs, character())
  expect_identical(out$keys, character())
  expect_identical(out$ambiguous, character())

  out_disallowed <- resolve_candidates(
    unqual = list(funs = "foo", idx = 1L),
    lib_data = data.frame(
      visit_idx = 1L,
      pkg = "pkgA",
      is_attach = TRUE,
      stringsAsFactors = FALSE
    ),
    allowed_packages = "pkgA",
    export_index = list(foo = "pkgB"),
    origin_map = character()
  )

  # indexed provider exists but is filtered out by allowed_packages
  expect_identical(out_disallowed$pkgs, character())
  expect_identical(out_disallowed$keys, character())
  expect_identical(out_disallowed$ambiguous, character())
})

test_that(".resolve_candidates returns empty when no packages allowed", {
  resolve_candidates <- getFromNamespace(".resolve_candidates", "stanflow")

  out <- resolve_candidates(
    unqual = list(funs = "as_draws", idx = 1L),
    lib_data = NULL,
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
    allowed_packages = c("pkgA", "pkgB", "pkgC"),
    export_index = list(foo = c("pkgB", "pkgC")),
    origin_map = c("pkgB::foo" = "pkgB", "pkgC::foo" = "pkgC")
  )

  expect_equal(out$ambiguous, "foo")
  expect_identical(out$pkgs, character())
  expect_identical(out$keys, character())
})

test_that(".resolve_candidates keeps ambiguity when one call remains unresolved", {
  resolve_candidates <- getFromNamespace(".resolve_candidates", "stanflow")

  out <- resolve_candidates(
    unqual = list(funs = c("foo", "foo"), idx = c(1L, 3L)),
    lib_data = data.frame(
      visit_idx = c(2L, 4L),
      pkg = c("pkgA", "pkgB"),
      is_attach = c(TRUE, TRUE),
      stringsAsFactors = FALSE
    ),
    allowed_packages = c("pkgA", "pkgB"),
    export_index = list(foo = c("pkgA", "pkgB")),
    origin_map = c("pkgA::foo" = "pkgA", "pkgB::foo" = "pkgB")
  )

  expect_equal(out$ambiguous, "foo")
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
    allowed_packages = c("pkgA", "pkgB"),
    export_index = list(foo = c("pkgA", "pkgB")),
    origin_map = c("pkgB::foo" = "pkgX")
  )

  expect_identical(out$ambiguous, character())
  expect_identical(out$pkgs, "pkgA")
  expect_identical(out$keys, "pkgA::foo")
})

test_that(".resolve_candidates resolves attachment-ordered calls", {
  resolve_candidates <- getFromNamespace(".resolve_candidates", "stanflow")

  out <- resolve_candidates(
    unqual = list(funs = "foo", idx = 3L),
    lib_data = data.frame(
      visit_idx = c(1L, 2L),
      pkg = c("pkgA", "pkgB"),
      is_attach = c(TRUE, TRUE),
      stringsAsFactors = FALSE
    ),
    allowed_packages = c("pkgA", "pkgB"),
    export_index = list(foo = c("pkgA", "pkgB")),
    origin_map = c("pkgA::foo" = "pkgA", "pkgB::foo" = "pkgB")
  )

  expect_identical(out$ambiguous, character())
  expect_identical(out$pkgs, "pkgB")
  expect_identical(out$keys, "pkgB::foo")
})

test_that(".resolve_candidates uses the most recent matching reattach", {
  resolve_candidates <- getFromNamespace(".resolve_candidates", "stanflow")

  out <- resolve_candidates(
    unqual = list(funs = "foo", idx = 4L),
    lib_data = data.frame(
      visit_idx = c(1L, 2L, 3L),
      pkg = c("pkgA", "pkgB", "pkgA"),
      is_attach = c(TRUE, TRUE, TRUE),
      stringsAsFactors = FALSE
    ),
    allowed_packages = c("pkgA", "pkgB"),
    export_index = list(foo = c("pkgA", "pkgB")),
    origin_map = c("pkgA::foo" = "pkgA", "pkgB::foo" = "pkgB")
  )

  expect_identical(out$ambiguous, character())
  expect_identical(out$pkgs, "pkgA")
  expect_identical(out$keys, "pkgA::foo")
})

test_that(".resolve_candidates returns empty when there are no attaches", {
  resolve_candidates <- getFromNamespace(".resolve_candidates", "stanflow")

  out <- resolve_candidates(
    unqual = list(funs = "foo", idx = 2L),
    lib_data = data.frame(
      visit_idx = 1L,
      pkg = "pkgA",
      is_attach = FALSE,
      stringsAsFactors = FALSE
    ),
    allowed_packages = "pkgA",
    export_index = list(foo = "pkgA"),
    origin_map = c("pkgA::foo" = "pkgA")
  )

  expect_identical(out$ambiguous, character())
  expect_identical(out$pkgs, character())
  expect_identical(out$keys, character())
})

test_that(".resolve_candidates treats same-origin providers as unambiguous", {
  resolve_candidates <- getFromNamespace(".resolve_candidates", "stanflow")

  out <- resolve_candidates(
    unqual = list(funs = "foo", idx = 2L),
    lib_data = data.frame(
      visit_idx = c(1L, 2L),
      pkg = c("pkgA", "pkgB"),
      is_attach = c(TRUE, TRUE),
      stringsAsFactors = FALSE
    ),
    allowed_packages = c("origin", "pkgA", "pkgB"),
    export_index = list(foo = c("pkgA", "pkgB")),
    origin_map = c("pkgA::foo" = "origin", "pkgB::foo" = "origin")
  )

  expect_identical(out$ambiguous, character())
  expect_identical(out$pkgs, "origin")
  expect_identical(out$keys, "origin::foo")
})

test_that(".resolve_candidates falls back to the resolved provider when its mapped origin is disallowed", {
  resolve_candidates <- getFromNamespace(".resolve_candidates", "stanflow")

  out <- resolve_candidates(
    unqual = list(funs = "foo", idx = 4L),
    lib_data = data.frame(
      visit_idx = c(1L, 2L, 3L),
      pkg = c("pkgA", "pkgC", "pkgB"),
      is_attach = c(TRUE, TRUE, TRUE),
      stringsAsFactors = FALSE
    ),
    allowed_packages = c("pkgA", "pkgB", "pkgC"),
    export_index = list(foo = c("pkgA", "pkgB", "pkgC")),
    origin_map = c(
      "pkgA::foo" = "pkgA",
      "pkgB::foo" = "pkgX",
      "pkgC::foo" = "pkgC"
    )
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

test_that(".scan_skip_regex escapes metacharacters and matches directory boundaries", {
  scan_skip_regex <- getFromNamespace(".scan_skip_regex", "stanflow")
  pattern <- scan_skip_regex(c(".quarto_cache", "renv", "a+b"))

  expect_true(grepl(pattern, "proj/renv/lib/file.R"))
  expect_true(grepl(pattern, "proj/.quarto_cache/chunk.R"))
  expect_true(grepl(pattern, "proj/a+b/src/file.R"))
  expect_false(grepl(pattern, "proj/renvish/lib/file.R"))
  expect_false(grepl(pattern, "proj/aXb/src/file.R"))
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
    allowed_packages = allowed_packages,
    export_index = export_index,
    origin_map = origin_map
  )

  expect_identical(r2$keys, "origin::foo")
  expect_identical(r2$pkgs, "origin")
})

test_that("scan_usage quiet suppresses cli messages", {
  tmp <- withr::local_tempdir()
  path <- write_file(file.path(tmp, "plain.R"), "1 + 1")

  noisy <- capture_messages(scan_usage(path, quiet = FALSE))
  expect_true(length(noisy) > 0)

  silent <- capture_messages(scan_usage(path, quiet = TRUE))
  expect_equal(silent, character())
})

test_that("scan_usage defaults to stanflow.quiet option", {
  tmp <- withr::local_tempdir()
  path <- write_file(file.path(tmp, "plain.R"), "1 + 1")

  withr::local_options(list(stanflow.quiet = TRUE))
  silent <- capture_messages(scan_usage(path))
  expect_equal(silent, character())

  withr::local_options(list(stanflow.quiet = FALSE))
  noisy <- capture_messages(scan_usage(path))
  expect_true(length(noisy) > 0)
})
