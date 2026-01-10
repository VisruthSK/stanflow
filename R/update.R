# Portions of this file are adapted from the tidyverse package.
# Copyright (c) 2021 tidyverse authors. Licensed under the MIT license.
# See LICENSE.note for details.
#' List all stanflow dependencies
#'
#' @param recursive If `TRUE`, will also list all dependencies.
#' @param dev If `FALSE` (default), checks for updates in the R-multiverse or CRAN
#'   (stable releases). If `TRUE`, checks the Stan R-universe (dev versions). This is
#'   only cogent for Stan packages, and cannot compare two dev versions.
#' @export
stanflow_deps <- function(recursive = FALSE, dev = FALSE) {
  pkgs <- tryCatch(
    utils::available.packages(repos = stan_repos(dev)),
    error = function(e) {
      cli::cli_abort(c(
        "Unable to reach repositories to check for updates.",
        "x" = "Package metadata could not be downloaded.",
        "i" = "Check your internet connection and try again."
      ))
    }
  )
  pkg_deps <- tools::package_dependencies(
    "stanflow",
    pkgs,
    recursive = recursive
  )

  if (
    length(pkg_deps) == 0 ||
      all(lengths(pkg_deps) == 0) ||
      all(is.na(pkg_deps))
  ) {
    # TODO: Remove once stanflow is published to a repo used by available.packages()
    pkg_deps <- utils::packageDescription("stanflow") |>
      with(paste(Depends, Imports, Suggests, sep = ",")) |>
      strsplit(",") |>
      unlist(use.names = FALSE) |>
      gsub("\\s*\\(.*?\\)", "", x = _) |>
      trimws() |>
      Filter(function(x) x != "" && x != "R", x = _) |>
      tools::package_dependencies(
        pkgs,
        recursive = recursive
      )
  }

  pkg_deps <- pkg_deps |>
    unlist() |>
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
#' This function requires an interactive R session.
#'
#' @inheritParams stanflow_deps
#' @export
stanflow_update <- function(recursive = FALSE, dev = FALSE) {
  is_testing <- getOption("stanflow.testing", FALSE)

  if (!interactive() && !is_testing) {
    cli::cli_abort(c(
      "{.fn stanflow_update} must be run interactively.",
      "x" = "Refusing to update packages in a non-interactive session.",
      "i" = "Start an interactive R session and rerun."
    ))
  }

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

  repos <- stan_repos(dev)

  if (!is_testing) {
    cli::cli_alert_info("Updating via {.fn utils::install.packages}...")
    utils::install.packages(behind$package, repos = repos)
  } else {
    pkg_call <- paste0('c("', paste(behind$package, collapse = '", "'), '")')
    repos_call <- paste0('c("', repos[1], '", getOption("repos"))')
    cli::cat_line("Start a clean R session then run:")
    cli::cat_line(
      paste0("install.packages(", pkg_call, ", repos = ", repos_call, ")")
    )
  }

  invisible(behind)
}
