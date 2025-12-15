core <- c("bayesplot", "loo", "posterior", "projpred", "shinystan")
backends <- c("brms", "cmdstanr", "rstan", "rstanarm")

find_unloaded <- function(pkgs) pkgs[!paste0("package:", pkgs) %in% search()]

# Attach the package from the same package library it was loaded from before.
# https://github.com/tidyverse/tidyverse/issues/171
same_library <- function(pkg) {
  loc <- if (pkg %in% loadedNamespaces()) dirname(getNamespaceInfo(pkg, "path"))
  library(pkg, lib.loc = loc, character.only = TRUE, warn.conflicts = FALSE)
}

core_attach_message <- function() {
  core_unloaded <- find_unloaded(core)
  suppressPackageStartupMessages(lapply(core_unloaded, same_library))
  if (length(core_unloaded) == 0) {
    return(NULL)
  }

  versions <- vapply(core_unloaded, package_version_h, character(1))

  packages <- paste0(
    cli::col_green(cli::symbol$tick),
    " ",
    cli::col_blue(format(core_unloaded)),
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
    left = cli::style_bold("Available Stan interfaces")
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

# TODO: could rewrite as single pipeline
package_version_h <- function(pkg) {
  x <- as.character(utils::packageVersion(pkg))
  pieces <- strsplit(x, ".", fixed = TRUE)
  pieces <- lapply(pieces, function(x) ifelse(x == "9000", cli::col_red(x), x))
  vapply(pieces, paste, collapse = ".", FUN.VALUE = character(1))
}
