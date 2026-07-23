#' Cite Stan packages in a project/files
#'
#' `stan_cite()` finds Stan packages and functions used in a project, then
#' returns their citations as BibTeX or bibentry records.
#'
#' @inheritParams ascribe::scan_usage
#' @param format One of `"bibtex"` or `"bibentry"`.
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
  strict = TRUE,
  format = c("bibtex", "bibentry"),
  skip_dirs = ascribe::scan_skip_dirs(),
  ignore_unqualified_functions = ascribe::stdlib_funs(),
  use_knitr = FALSE,
  quiet = getOption("stanflow.quiet", FALSE)
) {
  local_cli_quiet(quiet)
  ascribe::scan_usage(
    path = path,
    allowed_packages = .stan_pkgs,
    export_index = .stan_export_index,
    origin_map = .stan_origin_map,
    ignore_unqualified_functions = ignore_unqualified_functions,
    strict = strict,
    skip_dirs = skip_dirs,
    metapackages = list(stanflow = core),
    use_knitr = use_knitr,
    quiet = quiet
  ) |>
    ascribe::cite_usage(
      package_citations = .stan_citation_pkgs,
      function_citations = .stan_citation_funs,
      package_citation = ascribe::cite_package(
        extras = .stan_citation_pkg_extras,
        url = \(pkg) sprintf("https://mc-stan.org/%s/", pkg),
        note = \(pkg, meta) {
          sprintf(
            "R package version %s, https://discourse.mc-stan.org",
            meta$Version
          )
        }
      ),
      always_cite = "stanflow",
      format = format
    )
}
