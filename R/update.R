#' List all stanflow dependencies
#'
#' @param recursive If `TRUE`, will also list all dependencies.
#' @param dev If `FALSE` (default), checks for updates in the R-multiverse or CRAN
#'   (stable releases). If `TRUE`, checks the Stan R-universe (dev versions).
#' @export
stanflow_deps <- function(recursive = FALSE, dev = FALSE) {
  pkgs <- utils::available.packages(repos = stan_repos(dev))
  pkg_deps <- tools::package_dependencies(
    "stanflow",
    pkgs,
    recursive = recursive
  ) |>
    unlist() |>
    sort() |>
    unique()

  ignored <- c(
    "base",
    "compiler",
    "datasets",
    "graphics",
    "grDevices",
    "grid",
    "methods",
    "parallel",
    "splines",
    "stats",
    "stats4",
    "tools",
    "tcltk",
    "utils",
    "cli"
  )
  pkg_deps <- setdiff(pkg_deps, ignored)

  repo_ver <- pkgs[match(pkg_deps, rownames(pkgs)), "Version"]
  local_ver <- vapply(
    pkg_deps,
    \(pkg) {
      if (is_installed(pkg)) {
        as.character(utils::packageVersion(pkg))
      } else {
        "0"
      }
    },
    character(1)
  )

  behind <- mapply(
    function(r_str, l_str) {
      if (is.na(r_str)) {
        return(FALSE)
      }
      if (l_str == "0") {
        return(TRUE)
      }
      package_version(r_str) > package_version(l_str)
    },
    repo_ver,
    local_ver
  )

  data.frame(
    package = pkg_deps,
    remote = ifelse(is.na(repo_ver), NA, as.character(repo_ver)),
    local = local_ver,
    behind = behind,
    stringsAsFactors = FALSE
  )
}

#' Update stanflow packages
#'
#' @inheritParams stanflow_deps
#' @export
stanflow_update <- function(recursive = FALSE, dev = FALSE) {
  deps <- stanflow_deps(recursive, dev = dev)
  behind <- deps[deps$behind, ]

  if (nrow(behind) == 0) {
    cli::cat_line("All stanflow packages up-to-date!")
    return(invisible())
  }

  cli::cat_line(cli::pluralize(
    "The following {cli::qty(nrow(behind))}package{?s} {?is/are} out of date:"
  ))
  cli::cat_line()

  cli::cat_bullet(
    format(behind$package),
    " (",
    behind$local,
    " -> ",
    behind$remote,
    ")"
  )

  cli::cat_line()

  cli::cat_line("Start a clean R session then run:")
  cli::cat_line(
    "install.packages(",
    paste0(deparse(behind$package), collapse = "\n"),
    ", ",
    sprintf('repos = c("%s", getOption("repos"))', stan_repos(dev)[1]),
    ")"
  )
  invisible(behind)
}
