# Portions of this file are adapted from the tidyverse package.
# Copyright (c) 2024 tidyverse authors.
# License: MIT; full notice preserved in LICENSE.note.
# Source: https://github.com/tidyverse/tidyverse/blob/0231aafbc56914ee5371dd6c7b60677f168d7154/R/update.R

#' List all stanflow dependencies
#'
#' @description
#' Returns a data frame of Stan workflow packages and their local/remote versions.
#' When `check_updates = FALSE`, remote versions are not queried and the `remote`
#' and `behind` columns are `NA` and `FALSE`, respectively.
#'
#' Adapted from [tidyverse::tidyverse_deps()].
#'
#' @param recursive If `TRUE`, will also list dependencies of dependencies. When
#'   `check_updates = TRUE`, the recursive traversal follows only "strong"
#'   dependencies (Depends/Imports/LinkingTo), so Suggests are not expanded
#'   recursively.
#' @param dev If `FALSE` (default), checks for updates in the R-multiverse or CRAN
#'   (stable releases). If `TRUE`, checks the Stan R-universe (dev versions). This is
#'   only cogent for Stan packages, and cannot compare two dev versions.
#' @param check_updates Logical. If `FALSE`, skips checking for remote versions and
#'   only reports locally installed package versions.
#' @return A data frame with columns:
#' \describe{
#'   \item{package}{Package name.}
#'   \item{remote}{Repository version (character, `NA` when not queried).}
#'   \item{local}{Installed version (character, `"0"` if not installed).}
#'   \item{behind}{Logical; `TRUE` when `remote` is newer than `local`.}
#' }
#' @examples
#' \dontrun{
#' # Full dependency check with remote versions
#' stanflow_deps(recursive = TRUE)
#'
#' # Local-only inventory (fast, no network)
#' stanflow_deps(check_updates = FALSE)
#' }
#' @export
stanflow_deps <- function(
  recursive = FALSE,
  dev = FALSE,
  check_updates = TRUE
) {
  pkgs <- NULL
  if (check_updates) {
    pkgs <- tryCatch(
      utils::available.packages(repos = stan_repos(dev)),
      error = function(e) {
        cli::cli_abort(
          c(
            "Unable to reach repositories to check for updates.",
            "x" = "Package metadata could not be downloaded."
          )
        )
      }
    )
  }

  if (!is.null(pkgs)) {
    pkg_deps <- tools::package_dependencies(
      "stanflow",
      pkgs,
      which = "most",
      recursive = if (recursive) "strong" else FALSE
    )
  } else {
    pkg_deps <- utils::packageDescription("stanflow") |>
      with(paste(Depends, Imports, Suggests, sep = ",")) |>
      strsplit(",") |>
      unlist(use.names = FALSE) |>
      gsub("\\s*\\(.*?\\)", "", x = _) |>
      trimws() |>
      Filter(function(x) x != "" && x != "R", x = _) |>
      (\(deps) {
        if (!recursive) {
          return(deps)
        }
        tools::package_dependencies(
          deps,
          utils::installed.packages(),
          recursive = TRUE
        ) |>
          unlist(use.names = FALSE) |>
          c(deps)
      })()
  }

  if (
    length(pkg_deps) == 0 ||
      all(lengths(pkg_deps) == 0) ||
      all(is.na(unlist(pkg_deps)))
  ) {
    # TODO: Remove once stanflow is published to a repo used by available.packages()
    pkg_deps <- utils::packageDescription("stanflow") |>
      with(paste(Depends, Imports, Suggests, sep = ",")) |>
      strsplit(",") |>
      unlist(use.names = FALSE) |>
      gsub("\\s*\\(.*?\\)", "", x = _) |>
      trimws() |>
      Filter(function(x) x != "" && x != "R", x = _) |>
      (\(deps) {
        if (!recursive) {
          return(deps)
        }
        tools::package_dependencies(deps, pkgs, recursive = TRUE) |>
          unlist(use.names = FALSE) |>
          c(deps)
      })()
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
    remote = as.character(repo_ver),
    local = local_ver,
    behind = behind,
    stringsAsFactors = FALSE
  )
}

#' Update stanflow packages
#'
#' @description
#' Checks for outdated Stan workflow packages and installs updates. This function
#' requires an interactive R session and will error otherwise.
#' Adapted from [tidyverse::tidyverse_update()].
#'
#' @return Invisibly returns a data frame of outdated packages (same columns as
#' \code{\link{stanflow_deps}}). Returns \code{NULL} invisibly when no updates are
#' needed.
#' @examples
#' \dontrun{
#' # Update direct dependencies only
#' stanflow_update()
#'
#' # Update full dependency tree (including suggests)
#' stanflow_update(recursive = TRUE)
#' }
#'
#' @inheritParams stanflow_deps
#' @export
stanflow_update <- function(recursive = FALSE, dev = FALSE) {
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

  if (is_interactive_session()) {
    title <- if (dev) {
      "Update packages from Stan Universe (Dev)?"
    } else {
      "Update packages from R-multiverse (Stable)?"
    }
    if (utils::menu(c("Yes", "No"), title = title) != 1) {
      cli::cli_abort("Update aborted by user.")
    }
  }

  repos <- stan_repos(dev)

  pkgs_to_report <- if (getOption("stanflow.testing", FALSE)) {
    behind$package
  } else {
    character()
  }

  # Muffle all warnings except cannot install
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

  if (length(pkgs_to_report)) {
    pkg_call <- paste0(
      'c("',
      paste(unique(pkgs_to_report), collapse = '", "'),
      '")'
    )
    repos_call <- paste0('c("', repos[1], '", getOption("repos"))')
    cli::cat_line("Start a clean R session then run:")
    cli::cat_line(
      paste0("install.packages(", pkg_call, ", repos = ", repos_call, ")")
    )
  }

  invisible(behind)
}
