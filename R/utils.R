# Portions of this file are adapted from the tidyverse package.
# Copyright (c) 2021 tidyverse authors. Licensed under the MIT license.
# See LICENSE.note for details.
# nocov start
core <- c("bayesplot", "loo", "posterior", "projpred", "shinystan")
backends <- c("brms", "cmdstanr", "rstan", "rstanarm")
stanflow_pkgs <- c(core, backends)

compliance_imports <- function() {
  bayesplot::abline_01
  loo::compare
  posterior::as_draws
  projpred::augdat_ilink_binom
  shinystan::as.shinystan
}
# nocov end

wrapped_startup <- function(msg, ...) {
  if (is.null(msg)) {
    return()
  }
  if (isTRUE(getOption("stanflow.quiet"))) {
    return()
  }
  packageStartupMessage(msg, ...)
}

# Attach the package from the same package library it was loaded from before.
# https://github.com/tidyverse/tidyverse/issues/171
same_library <- function(pkg) {
  library(
    pkg,
    lib.loc = if (pkg %in% loadedNamespaces()) {
      dirname(getNamespaceInfo(pkg, "path"))
    },
    character.only = TRUE,
    warn.conflicts = FALSE
  )
}

grepl_fixed <- function(pattern, x) {
  specials <- c(
    ".",
    "^",
    "$",
    "*",
    "+",
    "?",
    "(",
    ")",
    "[",
    "]",
    "{",
    "}",
    "|",
    "\\"
  )
  if (any(strsplit(pattern, "", fixed = TRUE)[[1]] %in% specials)) {
    grepl(pattern, x)
  } else {
    grepl(pattern, x, fixed = TRUE)
  }
}

stan_repos <- function(dev = FALSE) {
  if (dev) {
    c("https://stan-dev.r-universe.dev", getOption("repos"))
  } else {
    c("https://community.r-multiverse.org", getOption("repos"))
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

pkg_version <- function(pkg) {
  if (is_installed(pkg)) {
    as.character(utils::packageVersion(pkg))
  } else {
    NA_character_
  }
}

add_planned <- function(planned, ...) c(planned, ...)
