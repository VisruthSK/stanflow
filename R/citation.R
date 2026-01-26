#' Collect citations
#'
#' Unqualified function calls are only attributed when a Stan package is
#' attached via `library()` or `require()` in the same file. Known reexports
#' are remapped to their origin packages; missing mappings fall back to the
#' resolved package.
#'
#' @param path A single project directory (searched recursively) or a vector of
#'   files (.R/.Rmd/.qmd).
#' @param ignore_unqualified_functions Character vector of function names to ignore when
#'   attributing (unqualified) calls to Stan packages. Defaults to exports from
#'   base R packages listed in `stdlib_funs()`. Calls like `rstan::plot()` will NOT
#'   be ignored even if `plot` is in `ignore_unqualified_functions`, since they are
#'   namespaced.
#' @param strict If `TRUE`, only count unqualified function calls that resolve
#'   to a single Stan package.
#' @param skip_dirs Character vector of directory names to skip when scanning a
#'   directory.
#' @param format One of "bibtex" or "bibentry".
#' @return A BibTeX character vector or a bibentry object.
#' @export
stan_cite <- function(
  path = ".",
  ignore_unqualified_functions = .stdlib_funs,
  strict = TRUE,
  skip_dirs = .scan_skip_dirs,
  format = c("bibtex", "bibentry")
) {
  scan_usage(
    path = path,
    ignore_unqualified_functions = ignore_unqualified_functions,
    strict = strict,
    skip_dirs = skip_dirs,
    allowed_packages = .stan_pkgs,
    export_index = .stan_export_index,
    origin_map = .stan_origin_map
  ) |>
    with(expr = c(packages, functions, "stanflow", "R")) |>
    unique() |>
    (\(keys) {
      mget(
        keys,
        envir = .stan_citation_funs,
        inherits = TRUE,
        ifnotfound = list(NULL)
      ) |>
        Map(
          \(key, entry) if (is.null(entry)) .build_pkg_citation(key) else entry,
          key = keys,
          entry = _
        ) |>
        Filter(Negate(is.null), x = _)
    })() |>
    (\(entries) {
      if (!length(entries)) {
        cli::cli_alert_info("No citations found.")
        character()
      } else {
        entries <- do.call(c, entries)
        if (match.arg(format, c("bibtex", "bibentry")) == "bibentry") {
          entries
        } else {
          toBibtex(entries)
        }
      }
    })()
}

.pkg_cite <- function(pkg) {
  meta <- packageDescription(pkg)
  utils::bibentry(
    bibtype = "Manual",
    key = pkg,
    title = meta[["Title"]],
    author = citation(meta[["Package"]])[[1]]$author,
    year = sub("-.*", "", meta[["Date"]]),
    note = sprintf(
      "R package version %s, https://discourse.mc-stan.org",
      meta$Version
    ),
    # rstan url will point to rstan package site instead of main Stan site here.
    url = sprintf("https://mc-stan.org/%s/", pkg)
  )
}

# TODO: the two papers are used in papers.R already, reference those instead of rebuilding?
.build_pkg_citation <- function(pkg) {
  c(
    .pkg_cite(pkg),
    switch(
      pkg,
      bayesplot = utils::bibentry(
        bibtype = "Article",
        key = "bayesplot-2019",
        title = "Visualization in Bayesian workflow",
        author = c(
          person("Jonah", "Gabry"),
          person("Daniel", "Simpson"),
          person("Aki", "Vehtari"),
          person("Michael", "Betancourt"),
          person("Andrew", "Gelman")
        ),
        year = "2019",
        journal = "J. R. Stat. Soc. A",
        volume = 182,
        issue = 2,
        pages = "389-402",
        doi = "10.1111/rssa.12378"
      ),
      posterior = utils::bibentry(
        bibtype = "Article",
        key = "rhat-2021",
        title = "Rank-normalization, folding, and localization: An improved Rhat for assessing convergence of MCMC (with discussion)",
        author = c(
          person("Aki", "Vehtari"),
          person("Andrew", "Gelman"),
          person("Daniel", "Simpson"),
          person("Bob", "Carpenter"),
          person("Paul-Christian", "B\\\"urkner")
        ),
        journal = "Bayesian Analysis",
        year = "2021",
        volume = "16",
        number = "2",
        pages = "667-718"
      ),
      NULL
    )
  )
}
