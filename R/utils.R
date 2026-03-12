# Portions of this file are adapted from the tidyverse package.
# Copyright (c) 2024 tidyverse authors.
# License: MIT; full notice preserved in LICENSE.note.
# Sources:
# - https://github.com/tidyverse/tidyverse/blob/0231aafbc56914ee5371dd6c7b60677f168d7154/R/utils.R
# - https://github.com/tidyverse/tidyverse/blob/0231aafbc56914ee5371dd6c7b60677f168d7154/R/attach.R
# - https://github.com/tidyverse/tidyverse/blob/0231aafbc56914ee5371dd6c7b60677f168d7154/R/zzz.R

# nocov start
compliance_imports <- \() {
  bayesplot::abline_01
  loo::compare
  posterior::as_draws
  projpred::augdat_ilink_binom
  shinystan::as.shinystan
}
# nocov end

wrapped_startup <- function(msg, ...) {
  if (is.null(msg) || isTRUE(getOption("stanflow.quiet"))) {
    return()
  }
  packageStartupMessage(msg, ...)
}

# Attach the package from the same package library it was loaded from before.
# https://github.com/tidyverse/tidyverse/issues/171
.same_library <- function(pkg) {
  library(
    pkg,
    lib.loc = if (pkg %in% loadedNamespaces()) {
      dirname(getNamespaceInfo(pkg, "path"))
    },
    character.only = TRUE,
    warn.conflicts = FALSE
  )
}

#' Stan package repositories
#'
#' @param dev Include the development r-universe repo--don't use this unless you need the latest commits.
#' @return Character vector of repository URLs.
#' @export
stan_repos <- function(dev = FALSE) {
  if (dev) {
    c(StanRUniverse = "https://stan-dev.r-universe.dev", getOption("repos"))
  } else {
    c(Multiverse = "https://community.r-multiverse.org", getOption("repos"))
  }
}

invert <- function(x) {
  if (length(x) == 0) {
    return(list())
  }
  unstacked <- utils::stack(x)
  split(as.character(unstacked$ind), unstacked$values)
}

is_attached <- function(x) paste0("package:", x) %in% search()
is_installed <- function(x) length(find.package(x, quiet = TRUE)) > 0

is_interactive_session <- function() {
  override <- getOption("stanflow.force_interactive", NULL)
  if (!is.null(override)) {
    return(isTRUE(override))
  }
  interactive()
}

local_cli_quiet <- function(quiet, env = parent.frame()) {
  if (!quiet) {
    return(invisible(NULL))
  }

  withr::local_options(
    list(cli.default_handler = function(...) invisible(NULL)),
    .local_envir = env
  )
  invisible(NULL)
}
