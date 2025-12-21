# Portions of this file are adapted from the tidyverse package.
# Copyright (c) 2021 tidyverse authors. Licensed under the MIT license.
# See LICENSE.note for details.
find_unloaded <- function(pkgs) pkgs[!paste0("package:", pkgs) %in% search()]

#' Print stanflow status and conflicts
#'
#' Print a consolidated status report showing attached packages, available
#' interfaces, and any conflicts.
#'
#' @return Invisibly returns the character vector that was printed.
#' @export
flow_check <- function() {
  messages <- list(
    core_attach_message(show_all = TRUE),
    backends_attach_message(),
    stanflow_conflict_message(stanflow_conflicts())
  ) |>
    Filter(Negate(is.null), x = _)
  cli::cat_line(messages)
  invisible(unlist(messages))
}

core_attach_message <- function(show_all = FALSE) {
  core_unloaded <- find_unloaded(core)
  suppressPackageStartupMessages(lapply(core_unloaded, same_library))
  to_show <- if (show_all) core else core_unloaded
  if (length(to_show) == 0) {
    return(NULL)
  }

  versions <- vapply(to_show, package_version_h, character(1))

  packages <- paste0(
    cli::col_green(cli::symbol$tick),
    " ",
    cli::col_blue(format(to_show)),
    " ",
    cli::ansi_align(versions, max(cli::ansi_nchar(versions)))
  )

  header <- cli::rule(
    left = cli::style_bold("Attaching Stan processing packages"),
    right = paste0("stanflow ", package_version_h("stanflow"))
  )

  message_packages(packages, header)
}

backends_attach_message <- function() {
  versions <- vapply(
    backends,
    function(x) {
      if (is_installed(x)) package_version_h(x) else ""
    },
    character(1)
  )

  symbols <- vapply(
    backends,
    function(x) {
      if (is_attached(x)) {
        cli::col_green(cli::symbol$tick)
      } else if (is_installed(x)) {
        cli::col_blue(cli::symbol$bullet)
      } else {
        cli::col_red(cli::symbol$cross)
      }
    },
    character(1)
  )

  # Grey out names if not installed
  names <- format(backends)
  names <- ifelse(versions == "", cli::col_silver(names), cli::col_blue(names))

  packages <- paste0(
    symbols,
    " ",
    names,
    " ",
    cli::ansi_align(versions, max(cli::ansi_nchar(versions)))
  )

  header <- cli::rule(
    left = cli::style_bold("Available Stan interfaces"),
    right = "setup_interface()"
  )

  message_packages(packages, header)
}

message_packages <- function(packages, header) {
  if (length(packages) %% 2 == 1) {
    packages <- append(packages, "")
  }
  col1 <- seq_len(length(packages) / 2)
  info <- paste0(packages[col1], "     ", packages[-col1])

  paste0(header, "\n", paste(info, collapse = "\n"))
}

package_version_h <- function(pkg) {
  pkg |>
    utils::packageVersion() |>
    as.character() |>
    strsplit(".", fixed = TRUE) |>
    lapply(function(x) ifelse(x == "9000", cli::col_red(x), x)) |>
    vapply(paste, collapse = ".", FUN.VALUE = character(1))
}
