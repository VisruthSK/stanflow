#' Cite Stan packages in a project/files
#'
#' `stan_cite()` finds Stan packages and functions used in a project, then
#' returns their citations as BibTeX or bibentry records.
#'
#' @inheritParams ascribe::scan_usage
#' @param format One of `"bibtex"` or `"bibentry"`.
#' @param quiet Logical. If `TRUE`, suppresses status messages. Defaults to
#'   `FALSE`.
#' @return A BibTeX character vector or a bibentry object.
#' @export
#' @examples
#' path <- tempfile(fileext = ".R")
#' writeLines(
#'   c(
#'     "# one messy analysis file",
#'     "library(posterior)",
#'     "requireNamespace(\"loo\")",
#'     "draws <- as_draws(list(mu = rnorm(10)))",
#'     "posterior::rhat(draws)",
#'     "loo::loo(matrix(1))"
#'   ),
#'   path
#' )
#'
#' stan_cite(path, quiet = TRUE)
#' stan_cite(path, format = "bibentry", quiet = TRUE)
#' unlink(path)
stan_cite <- function(
  path = ".",
  strict = FALSE,
  format = c("bibtex", "bibentry"),
  skip_dirs = ascribe::scan_skip_dirs(),
  ignore_unqualified_functions = ascribe::stdlib_funs(),
  use_knitr = FALSE,
  quiet = getOption("stanflow.quiet", FALSE)
) {
  local_cli_quiet(quiet)
  fmt <- match.arg(format)
  ascribe::scan_usage(
    path = path,
    universe = list(
      packages = .stan_pkgs,
      export_index = .stan_export_index,
      origin_map = .stan_origin_map
    ),
    ignore_unqualified_functions = ignore_unqualified_functions,
    strict = strict,
    skip_dirs = skip_dirs,
    metapackages = list(stanflow = core),
    use_knitr = use_knitr
  ) |>
    ascribe::cite_usage(
      package_citations = .stan_citation_pkgs,
      function_citations = .stan_citation_funs,
      package_citation = .pkg_cite,
      always_cite = "stanflow",
      format = "bibentry"
    ) |>
    (\(entries) {
      if (is.null(entries) || !length(entries)) {
        if (fmt == "bibentry") entries else character()
      } else if (fmt == "bibentry") {
        entries
      } else {
        utils::toBibtex(entries)
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
    utils::packageDescription() |>
    (\(meta) {
      c(
        utils::bibentry(
          bibtype = "Manual",
          key = pkg,
          title = meta[["Title"]],
          author = utils::citation(meta[["Package"]])[[1]]$author,
          year = format(utils::packageDate(pkg), "%Y"),
          note = sprintf(
            "R package version %s, https://discourse.mc-stan.org",
            meta$Version
          ),
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
