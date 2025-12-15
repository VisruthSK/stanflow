wrapped_startup <- function(msg, ...) {
  if (is.null(msg)) {
    return()
  }
  packageStartupMessage(msg, ...)
}

# is_attached <- function(x) paste0("package:", x) %in% search()
