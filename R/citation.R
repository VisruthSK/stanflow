#' Cite Stan packages in a project/files
#'
#' `stan_cite()` generates the correct citations for Stan packages
#' in a directory or set of files. The `{knitr}` package is required
#' to parse Quarto (.qmd) or RMarkdown (.Rmd) documents. `stan_cite()`
#' uses some simple heuristics to guess which packages export functions,
#' and also attempts to map re-exports to their origin package.
#'
#' The parsing is handled by `funscanr()`; `stan_cite()` owns
#' the citation lookups.
#'
#' @param path A single project directory (searched recursively) or a vector of
#'   files (.R/.Rmd/.qmd).
#' @param ignore_unqualified_functions Defaults to exports from base R packages
#'   listed in `stdlib_funs()`. Character vector of function names to ignore when
#'   attributing (unqualified) calls to Stan packages. Calls like `rstan::plot()`
#'   will NOT be ignored even if `plot` is in `ignore_unqualified_functions`, since
#'   they are namespaced.
#' @param strict If `TRUE` (default), only count unqualified function calls that resolve
#'   to a single Stan package.
#' @param skip_dirs Defaults to directories listed in `scan_skip_dirs`. Character
#'   vector of directory names to skip when scanning a directory.
#' @param format One of "bibtex" or "bibentry", specifying the return format.
#' @return A BibTeX character vector or a bibentry object.
#' @export
stan_cite <- function(
  path = ".",
  strict = TRUE,
  format = c("bibtex", "bibentry"),
  skip_dirs = .scan_skip_dirs,
  ignore_unqualified_functions = .stdlib_funs
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
    (\(x) {
      list(
        pkgs = unique(c(x$packages, "stanflow")),
        funs = unique(x$functions)
      )
    })() |>
    (\(k) {
      c(
        # Build package citations by first looking them up in the `.stan_citation_pkgs`
        # environment generated in `data-raw/sysdata.R`, then appealing to `.build_pkg_citation()`.
        packages = mget(
          k$pkgs,
          envir = .stan_citation_pkgs,
          inherits = TRUE,
          ifnotfound = list(NULL)
        ) |>
          Map(
            \(pkg, entry) {
              if (is.null(entry)) .build_pkg_citation(pkg) else entry
            },
            pkg = k$pkgs,
            entry = _
          ),
        # Build function citations by pure lookup against `.stan_citation_funs`
        # environment generated in `data-raw/sysdata.R`
        functions = mget(
          k$funs,
          envir = .stan_citation_funs,
          inherits = TRUE,
          ifnotfound = list(NULL)
        )
      ) |>
        Filter(Negate(is.null), x = _) # ignore functions without citations
    })() |>
    (\(entries) {
      if (!length(entries)) {
        cli::cli_alert_info("No citations found.")
        character()
      } else {
        # Add base R citation, and format as requested
        entries <- do.call(c, entries) |> c(utils::citation("base"))
        if (match.arg(format, c("bibtex", "bibentry")) == "bibentry") {
          entries
        } else {
          toBibtex(entries)
        }
      }
    })()
}

#' Build Stan package bibentry citations
#'
#' Helper function to build standardized package citations.
#' This mostly matches how each Stan R package wants to be
#' cited.
#'
#' @param pkg Stan package name as a character scalar.
#' @return A bibentry for citing that package.
#' @keywords internal
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

#' Cite Stan Packages
#'
#' Build the appropriate citation for R packages, including papers
#' needed to cite the package. Equivalent to `.pkg_cite()` for most packages.
#'
#' Bayesplot and Posterior have papers in addition to their "typical" software
#' citation that should be cited when using the package, which is why this exists.
#'
#' @param pkg Stan package name as a character scalar.
#' @return Vector of bibentries to properly cite the provided Stan package
#' @keywords internal
.build_pkg_citation <- function(pkg) {
  # TODO: the two papers are used in papers.R already, reference those instead of rebuilding?
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
