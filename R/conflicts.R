#' Conflicts between stanflow and other packages
#'
#' This function lists all the conflicts between packages in stanflow
#' and other loaded packages.
#'
#' There are several conflicts that are deliberately ignored: \code{diag},
#' \code{drop}, \code{match}, and \code{\%in\%} from posterior.
#'
#' @export
#' @param only Set this to a character vector to restrict to conflicts only
#'   with these packages.
#' @examples
#' stanflow_conflicts()
stanflow_conflicts <- function(only = NULL) {
  envs <- grep("^package:", search(), value = TRUE)
  names(envs) <- envs

  if (!is.null(only)) {
    only <- union(only, stanflow_pkgs)
    envs <- envs[names(envs) %in% paste0("package:", only)]
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
print.stanflow_conflicts <- function(x, ..., startup = FALSE) {
  cli::cat_line(stanflow_conflict_message(x))
  invisible(x)
}

stanflow_conflict_message <- function(x) {
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

  funs <- format(paste0(
    cli::col_blue(winner),
    "::",
    cli::col_green(paste0(names(x), "()"))
  ))

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

confirm_conflict <- function(packages, name) {
  objs <- lapply(packages, \(pkg) get(name, pos = pkg)) |>
    Filter(is.function, x = _)

  if (length(objs) <= 1) {
    return(NULL)
  }

  objs <- unique(objs)

  if (length(objs) == 1) {
    return(NULL)
  }

  packages[!duplicated(packages)]
}

ls_env <- function(env) {
  x <- ls(pos = env)

  if (env == "package:posterior") {
    x <- setdiff(x, c("diag", "drop", "match", "%in%"))
  }
  # if (env == "package:bayesplot") {
  #   x <- setdiff(x, "rhat")
  # }

  x
}
