# Portions of this file are adapted from the tidyverse package.
# See LICENSE.note for details.

#' Conflicts between stanflow and other packages
#'
#' This function lists all the conflicts between packages in stanflow
#' and other loaded packages.
#'
#' There are several conflicts that are deliberately ignored: `diag`,
#' `drop`, `match`, `\%in\%`, `mad`, `sd`, and `var` from posterior.
#'
#' @export
#' @param only Defaults to `NULL`. Set this to a character vector to restrict to conflicts only
#'   between the provided packages and loaded stanflow packages.
#' @examples
#' stanflow_conflicts()
#' stanflow_conflicts(c("base"))
stanflow_conflicts <- function(only = NULL) {
  envs <- grep("^package:", search(), value = TRUE)
  names(envs) <- envs

  if (!is.null(only)) {
    envs <- envs[
      names(envs) %in% paste0("package:", union(only, stanflow_pkgs))
    ]
  }

  conflicts <- invert(lapply(envs, ls_env)) |>
    Filter(function(x) length(x) > 1, x = _) |>
    Filter(
      function(pkg) any(pkg %in% paste0("package:", stanflow_pkgs)),
      x = _
    )

  conflict_funs <- Map(confirm_conflict, conflicts, names(conflicts)) |>
    Filter(Negate(is.null), x = _)

  class(conflict_funs) <- "stanflow_conflicts"
  conflict_funs
}

#' @export
print.stanflow_conflicts <- function(x, ...) {
  message <- stanflow_conflict_message(x)
  if (!is.null(message)) {
    cli::cat_line(message)
  }
  invisible(x)
}

#' Generate conflict message
#'
#' Pulled from tidyverse. Builds the conflict message.
#'
#' @param x A named list describing function-name conflicts.
#' @return Character vector of the conflict message to print.
#' @keywords internal
stanflow_conflict_message <- function(x) {
  if (length(x) == 0) {
    return(NULL)
  }

  header <- cli::rule(
    left = cli::style_bold("Conflicts"),
    right = "stanflow_conflicts()"
  )

  pkgs <- lapply(x, \(x) gsub("^package:", "", x))

  others <- lapply(pkgs, \(x) x[-1])

  other_calls <- mapply(
    function(others_vec, fun) {
      paste0(cli::col_blue(others_vec), "::", fun, "()", collapse = ", ")
    },
    others,
    names(others)
  )

  winner <- vapply(pkgs, "[", 1, FUN.VALUE = character(1))

  funs <- format(
    paste0(
      cli::col_blue(winner),
      "::",
      cli::col_green(paste0(names(x), "()"))
    )
  )

  bullets <- paste0(
    cli::col_red(cli::symbol$cross),
    " ",
    funs,
    " masks ",
    other_calls,
    collapse = "\n"
  )

  conflicted <- paste0(
    cli::col_cyan(cli::symbol$info),
    " ",
    "Use the ",
    cli::format_inline(
      "{.href [conflicted package](http://conflicted.r-lib.org/)}"
    ),
    " to force all conflicts to become errors"
  )

  paste0(header, "\n", bullets, "\n", conflicted)
}

#' Find function name conflicts
#' @keywords internal
confirm_conflict <- function(packages, name) {
  objs <- lapply(packages, \(pkg) get(name, pos = pkg)) |>
    Filter(is.function, x = _) |>
    unique()

  if (length(objs) <= 1) NULL else unique(packages)
}

#' Remove ignored conflicts
#' @keywords internal
ls_env <- function(env) {
  x <- ls(pos = env)

  if (env == "package:posterior") {
    x <- setdiff(x, c("diag", "drop", "match", "%in%", "mad", "sd", "var"))
  }
  # if (env == "package:bayesplot") {
  #   x <- setdiff(x, "rhat")
  # }

  x
}
