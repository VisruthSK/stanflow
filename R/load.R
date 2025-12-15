core <- c("bayesplot", "loo", "posterior", "projpred", "shinystan")
backends <- c("brms", "cmdstanr", "rstan", "rstanarm")

find_unloaded <- function(pkgs) pkgs[!paste0("package:", pkgs) %in% search()]
core_unloaded <- find_unloaded(core)
backends_unloaded <- find_unloaded(backends)

# Attach the package from the same package library it was loaded from before.
# https://github.com/tidyverse/tidyverse/issues/171
same_library <- function(pkg) {
  loc <- if (pkg %in% loadedNamespaces()) dirname(getNamespaceInfo(pkg, "path"))
  library(pkg, lib.loc = loc, character.only = TRUE, warn.conflicts = FALSE)
}

core_attach <- function() {
  to_load <- core_unloaded
  suppressPackageStartupMessages(lapply(to_load, same_library))
  invisible(to_load)
}

core_attach_message <- function(to_load) {
  if (length(to_load) == 0) {
    return(NULL)
  }

  header <- cli::rule(
    left = cli::style_bold("Attaching core Stan processing packages"),
    right = paste0("stanflow ", package_version_h("stanflow")) # TODO: move this elsewhere--didn't even see it before
  )

  to_load <- sort(to_load)
  versions <- vapply(to_load, package_version_h, character(1))

  packages <- paste0(
    cli::col_green(cli::symbol$tick),
    " ",
    cli::col_blue(format(to_load)),
    " ",
    cli::ansi_align(versions, max(cli::ansi_nchar(versions)))
  )

  # !NOTE: Two column layout a la tidyverse
  # if (length(packages) %% 2 == 1) {
  #   packages <- append(packages, "")
  # }
  # col1 <- seq_len(length(packages) / 2)
  # info <- paste0(packages[col1], "     ", packages[-col1])

  # paste0(header, "\n", paste(info, collapse = "\n"))
  paste0(header, "\n", paste(packages, collapse = "\n"))
}

package_version_h <- function(pkg) {
  x <- as.character(utils::packageVersion(pkg))

  pieces <- strsplit(x, ".", fixed = TRUE)
  pieces <- lapply(pieces, function(x) {
    ifelse(grepl(".9000", x, fixed = TRUE), cli::col_red(x), x)
  })
  vapply(pieces, paste, collapse = ".", FUN.VALUE = character(1))
}

# package_version_h <- function(pkg) {
#   pkg |>
#     utils::packageVersion() |>
#     as.character() |>
#     strsplit(".", fixed = TRUE) |>
#     lapply(\(x) ifelse(grepl(".9000", x, fixed = TRUE), cli::col_red(x), x)) |>
#     vapply(paste, collapse = ".", FUN.VALUE = character(1))
# }
