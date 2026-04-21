#' Cite Stan packages in a project/files
#'
#' `stan_cite()` generates the correct citations for Stan packages
#' in a directory or set of files. The `{knitr}` package is required
#' to parse Quarto (.qmd) or RMarkdown (.Rmd) documents. `stan_cite()`
#' uses some simple heuristics to guess which packages export functions,
#' and also attempts to map re-exports to their origin package. Calls
#' to `library()`, `require()`, `requireNamespace()`, or `use()` are
#' all recognized as attaching a package.
#'
#' The parsing is handled by `scan_usage()`; `stan_cite()` owns
#' the citation lookups.
#'
#' @param path A single project directory (searched recursively) or a vector of
#'   files (.R/.Rmd/.qmd).
#' @param ignore_unqualified_functions Defaults to exports from base R packages
#'   listed in `stdlib_funs()`. Character vector of function names to ignore when
#'   attributing (unqualified) calls to Stan packages. Calls like `rstan::plot()`
#'   will NOT be ignored even if `plot` is in `ignore_unqualified_functions`, since
#'   they are namespaced.
#' @param strict If `TRUE` (default), only count unqualified function calls
#'   whose origin can be determined exactly from the static scan, including
#'   attachment-order tie-breaks when the winner is unambiguous from the file.
#'   Unresolved calls are warned about and omitted.
#' @param skip_dirs Defaults to directories listed in `scan_skip_dirs`. Character
#'   vector of directory names to skip when scanning a directory.
#' @param format One of "bibtex" or "bibentry", specifying the return format.
#' @param quiet Logical. If `TRUE`, suppresses status messages.
#' @return A BibTeX character vector or a bibentry object.
#' @export
#' @examples
#' path <- tempfile(fileext = ".R")
#' writeLines(
#'   c(
#'     "# one messy analysis file",
#'     "library(posterior)",
#'     "requireNamespace(\"brms\")",
#'     "use(\"cmdstanr\", c(\"cmdstan_model\", \"write_stan_json\"))",
#'     "draws <- as_draws(list(mu = rnorm(10)))",
#'     "posterior::rhat(draws)",
#'     "brms::mixture(0.4)",
#'     "cmdstanr::write_stan_json(list(N = 3), \"data.json\")"
#'   ),
#'   path
#' )
#'
#' stan_cite(path, quiet = TRUE)
#' stan_cite(path, format = "bibentry", quiet = TRUE)
#' unlink(path)
stan_cite <- function(
  path = ".",
  strict = TRUE,
  format = c("bibtex", "bibentry"),
  skip_dirs = .scan_skip_dirs,
  ignore_unqualified_functions = .stdlib_funs,
  quiet = getOption("stanflow.quiet", FALSE)
) {
  local_cli_quiet(quiet)

  scan_usage(
    path = path,
    ignore_unqualified_functions = ignore_unqualified_functions,
    strict = strict,
    skip_dirs = skip_dirs,
    allowed_packages = .stan_pkgs,
    export_index = .stan_export_index,
    origin_map = .stan_origin_map,
    quiet = quiet
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
        # environment generated in `data-raw/sysdata.R`, then appealing to `.pkg_cite()`.
        packages = mget(
          k$pkgs,
          envir = .stan_citation_pkgs,
          inherits = TRUE,
          ifnotfound = list(NULL)
        ) |>
          Map(
            \(pkg, entry) {
              if (is.null(entry)) .pkg_cite(pkg) else entry
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
#' This mostly matches how each Stan R package wants to be cited.
#' Some Stan packages have additional paper citations generated in
#' `data-raw/sysdata.R` and stored in `.stan_citation_pkg_extras`.
#'
#' @param pkg Stan package name as a character scalar.
#' @return Vector of bibentries for citing that package.
#' @keywords internal
.pkg_cite <- function(pkg) {
  pkg |>
    packageDescription() |>
    (\(meta) {
      c(
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
        ),
        mget(
          pkg,
          envir = .stan_citation_pkg_extras,
          inherits = TRUE,
          ifnotfound = list(NULL)
        )[[1L]]
      )
    })()
}
