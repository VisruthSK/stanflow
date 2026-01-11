.stan_pkgs <- c(stanflow_pkgs, "rstantools")

#' Find Stan packages + Stan functions used
#'
#' @param path A project directory OR a single file (.R/.Rmd/.Qmd).
#' @return list(packages=character(), functions=character())
stan_usage <- function(path = ".") {
  files <- if (dir.exists(path)) {
    list.files(
      path,
      recursive = TRUE,
      full.names = TRUE,
      ignore.case = TRUE,
      pattern = "\\.(R|Rmd|Qmd)$"
    )
  } else {
    path
  }

  used_pkgs <- character()
  used_keys <- character()

  for (file in files) {
    code <- .tangle(file)
    hits <- .scan_tokens(code)
    used_pkgs <- c(used_pkgs, hits$pkgs)
    used_keys <- c(used_keys, hits$keys)
  }

  list(packages = sort(unique(used_pkgs)), functions = sort(unique(used_keys)))
}

.tangle <- function(file) {
  ext <- tolower(sub(".*\\.", "", file))
  if (ext == "r") {
    return(paste(readLines(file, warn = FALSE), collapse = "\n"))
  }

  tmp <- tempfile(fileext = ".R")
  on.exit(unlink(tmp), add = TRUE)

  if (ext == "rmd") {
    knitr::purl(file, tmp, quiet = TRUE, documentation = 0)
  }
  if (ext == "qmd") {
    quarto::qmd_to_r_script(file, tmp)
  }

  paste(readLines(tmp, warn = FALSE), collapse = "\n")
}

.scan_tokens <- function(code) {
  expr <- tryCatch(parse(text = code, keep.source = TRUE), error = function(e) {
    NULL
  })
  if (is.null(expr)) {
    return(list(pkgs = character(), keys = character()))
  }

  pd <- utils::getParseData(expr, includeText = TRUE)
  pd <- pd[order(pd$line1, pd$col1, pd$id), ]

  attached <- character()
  pkgs <- character()
  keys <- character()

  choose_attached <- function(candidates) {
    if (length(candidates) == 1L) {
      return(candidates)
    }
    if (!length(attached)) {
      return(candidates[1L])
    }
    pos <- vapply(
      candidates,
      function(p) {
        w <- which(attached == p)
        if (length(w)) max(w) else 0L
      },
      integer(1)
    )
    candidates[which.max(pos)]
  }

  n <- nrow(pd)
  for (i in seq_len(n)) {
    tok <- pd$token[i]
    txt <- pd$text[i]

    # Track attachment order: library()/require()/requireNamespace()
    if (
      tok == "SYMBOL_FUNCTION_CALL" &&
        txt %in% c("library", "require", "requireNamespace")
    ) {
      pkg <- ""
      for (j in (i + 1L):min(i + 20L, n)) {
        if (pd$token[j] == "RPAR") {
          break
        }
        if (pd$token[j] %in% c("SYMBOL", "STR_CONST")) {
          pkg <- gsub("^['\"]|['\"]$", "", pd$text[j])
          break
        }
      }
      if (nzchar(pkg) && pkg %in% .stan_pkgs) {
        pkgs <- c(pkgs, pkg)
      }
      if (nzchar(pkg) && txt %in% c("library", "require")) {
        attached <- c(attached, pkg)
      }
      next
    }

    # Namespaced calls: pkg::fun / pkg:::fun
    if (
      tok %in%
        c("SYMBOL_FUNCTION_CALL", "SYMBOL") &&
        i >= 3L &&
        pd$token[i - 1L] %in% c("NS_GET", "NS_GET_INT") &&
        pd$token[i - 2L] == "SYMBOL_PACKAGE"
    ) {
      pkg <- pd$text[i - 2L]
      fun <- gsub("^`(.*)`$", "\\1", pd$text[i])
      if (fun %in% .stdlib_funs) {
        next
      }

      if (pkg %in% .stan_pkgs) {
        pkgs <- c(pkgs, pkg)
        keys <- c(keys, paste0(pkg, "::", fun))
      }
      next
    }

    # Unqualified calls: resolve by attachment order (best-effort)
    if (tok == "SYMBOL_FUNCTION_CALL") {
      fun <- gsub("^`(.*)`$", "\\1", txt)
      if (fun %in% .stdlib_funs) {
        next
      }

      candidates <- .fun_to_pkgs[[fun]]
      if (length(candidates)) {
        pkg <- choose_attached(candidates)
        pkgs <- c(pkgs, pkg)
        keys <- c(keys, paste0(pkg, "::", fun))
      }
    }
  }

  pkgs <- pkgs[pkgs %in% .stan_pkgs]
  keys <- keys[sub("::.*$", "", keys) %in% .stan_pkgs]
  list(pkgs = pkgs, keys = keys)
}

.fun_to_pkgs <- local({
  funs <- unlist(.stan_exports, use.names = FALSE)
  pkgs <- rep(names(.stan_exports), lengths(.stan_exports))
  split(pkgs, funs)
})

# stdlib function names to never attribute to Stan pkgs (even if re-exported)
.stdlib_funs <- local({
  stdlib <- c("base", "stats", "utils", "graphics", "grDevices", "methods")
  unique(unlist(lapply(stdlib, getNamespaceExports), use.names = FALSE))
})
