# Portions of this file are adapted from the tidyverse package.
# Copyright (c) 2021 tidyverse authors. Licensed under the MIT license.
# See LICENSE.note for details.
#' List all stanflow dependencies
#'
#' @param recursive If `TRUE`, will also list all dependencies.
#' @param dev If `FALSE` (default), checks for updates in the R-multiverse or CRAN
#'   (stable releases). If `TRUE`, checks the Stan R-universe (dev versions). This is
#'   only cogent for Stan packages, and cannot compare two dev versions.
#' @param check_updates Logical. If `FALSE`, skips checking for remote versions and
#'   only reports locally installed package versions.
#' @export
stanflow_deps <- function(
  recursive = FALSE,
  dev = FALSE,
  check_updates = TRUE
) {
  pkgs <- NULL
  if (check_updates) {
    pkgs <- tryCatch(
      withCallingHandlers(
        utils::available.packages(repos = stan_repos(dev)),
        warning = function(w) stop(w)
      ),
      error = function(e) {
        cli::cli_abort(
          c(
            "Unable to reach repositories to check for updates.",
            "x" = "Package metadata could not be downloaded.",
            "i" = "Set {.code check_updates = FALSE} to skip update checks."
          )
        )
      }
    )
  }

  if (!is.null(pkgs)) {
    pkg_deps <- tools::package_dependencies(
      "stanflow",
      pkgs,
      recursive = recursive
    )
  } else {
    pkg_deps <- utils::packageDescription("stanflow") |>
      with(paste(Depends, Imports, Suggests, sep = ",")) |>
      strsplit(",") |>
      unlist(use.names = FALSE) |>
      gsub("\\s*\\(.*?\\)", "", x = _) |>
      trimws() |>
      Filter(function(x) x != "" && x != "R", x = _)

    if (recursive) {
      installed_db <- utils::installed.packages()
      recursive_deps <- tools::package_dependencies(
        pkg_deps,
        installed_db,
        recursive = TRUE
      )
      pkg_deps <- c(pkg_deps, unlist(recursive_deps, use.names = FALSE))
    }
  }

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

  repo_ver <- if (is.null(pkgs)) {
    rep(NA_character_, length(pkg_deps))
  } else {
    pkgs[match(pkg_deps, rownames(pkgs)), "Version"]
  }
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

  behind <- if (is.null(pkgs)) {
    rep(FALSE, length(pkg_deps))
  } else {
    mapply(
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
  }

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

  if (!is_interactive_session()) {
    cli::cli_abort(
      c(
        "{.fn stanflow_update} must be run interactively.",
        "x" = "Refusing to update packages in a non-interactive session.",
        "i" = "Start an interactive R session and rerun."
      )
    )
  }

  deps <- stanflow_deps(recursive, dev = dev, check_updates = TRUE)
  behind <- deps[deps$behind, ]

  if (nrow(behind) == 0) {
    cli::cat_line("All stanflow packages up-to-date!")
    return(invisible())
  }

  cli::cat_line(
    cli::pluralize(
      "The following {cli::qty(nrow(behind))}package{?s} {?is/are} out of date:"
    )
  )
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

  pkgs_to_report <- if (is_testing) behind$package else character()

  withCallingHandlers(
    utils::install.packages(behind$package, repos = repos, quiet = TRUE),
    warning = function(w) {
      if (
        grepl(
          "cannot remove prior installation of package",
          w$message,
          fixed = TRUE
        )
      ) {
        m <- regexpr("[\u2018'](.+?)[\u2019']", w$message)
        if (m != -1) {
          pkg <- substring(w$message, m + 1, m + attr(m, "match.length") - 2)
          pkgs_to_report <<- c(pkgs_to_report, pkg)
        }
      } else {
        invokeRestart("muffleWarning")
      }
    }
  )

  if (length(pkgs_to_report) > 0) {
    pkgs_to_report <- unique(pkgs_to_report)
    pkg_call <- paste0('c("', paste(pkgs_to_report, collapse = '", "'), '")')
    repos_call <- paste0('c("', repos[1], '", getOption("repos"))')
    cli::cat_line("Start a clean R session then run:")
    cli::cat_line(
      paste0("install.packages(", pkg_call, ", repos = ", repos_call, ")")
    )
  }

  invisible(behind)
}
