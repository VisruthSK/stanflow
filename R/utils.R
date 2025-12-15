wrapped_startup <- function(msg, ...) {
  if (is.null(msg)) {
    return()
  }
  packageStartupMessage(msg, ...)
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
