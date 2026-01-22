# Portions of this file are adapted from the tidyverse package.
# See LICENSE.note for details.
# nocov start
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
  loc <- if (pkg %in% loadedNamespaces()) dirname(getNamespaceInfo(pkg, "path"))
  library(pkg, lib.loc = loc, character.only = TRUE, warn.conflicts = FALSE)
}

#' Stan package repositories
#'
#' @param dev Include the development r-universe repo.
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

  old <- options(cli.default_handler = function(...) invisible(NULL))
  restore_expr <- bquote(options(
    cli.default_handler = .(old$cli.default_handler)
  ))

  eval(call("on.exit", restore_expr, add = TRUE), envir = env)

  invisible(NULL)
}

.reset_citation_cache <- function(pkgs = NULL, env = .stan_citation_pkgs) {
  force(env)
  if (is.null(pkgs)) {
    pkgs <- ls(.stan_citation_builders, all.names = TRUE)
  }
  if (!length(pkgs)) {
    return(invisible(FALSE))
  }
  pkgs <- intersect(pkgs, ls(.stan_citation_builders, all.names = TRUE))
  if (!length(pkgs)) {
    return(invisible(FALSE))
  }
  for (pkg in pkgs) {
    if (exists(pkg, envir = env, inherits = FALSE)) {
      rm(list = pkg, envir = env)
    }
    pkg |>
      get(envir = .stan_citation_builders, inherits = FALSE) |>
      .lazy_cite(pkg, builder = _, env = env)
  }
  invisible(TRUE)
}
