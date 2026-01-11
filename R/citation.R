.stan_pkgs <- c(stanflow_pkgs, "rstantools")

#' Find Stan packages + Stan functions used
#'
#' @param path A single project directory (searched recursively) or a vector of
#'   files (.R/.Rmd/.Qmd).
#' @param ignore Path to a .gitignore-style file used to exclude paths while
#'   searching a directory.
#' @param quiet Silence informational output.
#' @return list(packages=character(), functions=character())
stan_usage <- function(path = ".", ignore = NULL, quiet = FALSE) {
  local_cli_quiet(quiet)
  paths <- normalizePath(path, winslash = "/", mustWork = TRUE)
  dir_flags <- dir.exists(paths)

  files <- character()
  if (length(paths) == 1L && dir_flags) {
    dir_path <- paths[[1L]]
    cli::cli_alert_info("Searching directory {.path {dir_path}}")
    files <- list.files(
      dir_path,
      recursive = TRUE,
      full.names = TRUE,
      ignore.case = TRUE,
      pattern = "\\.(R|Rmd|Qmd)$"
    )
    if (!is.null(ignore)) {
      if (!file.exists(ignore)) {
        cli::cli_abort("{.arg ignore} must point to an existing file.")
      }
      files <- files |>
        .filter_ignored(
          dir_path,
          normalizePath(ignore, winslash = "/", mustWork = TRUE)
        )
    }
  } else {
    if (any(dir_flags)) {
      cli::cli_abort(
        "{.arg path} must be a single directory or a vector of files."
      )
    }
    if (!is.null(ignore)) {
      cli::cli_abort(
        "{.arg ignore} can only be used when {.arg path} is a directory."
      )
    }
    for (file_path in paths) {
      cli::cli_alert_info("Searching {.path {file_path}}")
    }
    files <- paths
  }

  if (!length(files)) {
    cli::cli_abort("No files found.")
  }

  files <- unique(files)

  used_pkgs <- vector("list", length(files))
  used_keys <- vector("list", length(files))

  for (i in seq_along(files)) {
    code <- .extract_code(files[[i]])
    hits <- .scan_tokens(code)
    used_pkgs[[i]] <- hits$pkgs
    used_keys[[i]] <- hits$keys
  }

  used_pkgs <- unlist(used_pkgs, use.names = FALSE)
  used_keys <- unlist(used_keys, use.names = FALSE)
  if (!length(used_pkgs)) {
    used_pkgs <- character()
  }
  if (!length(used_keys)) {
    used_keys <- character()
  }
  list(packages = sort(unique(used_pkgs)), functions = sort(unique(used_keys)))
}

.read_ignore_patterns <- function(ignore_path) {
  readLines(ignore_path, warn = FALSE) |>
    trimws() |>
    (\(x) x[nzchar(x) & !startsWith(x, "#")])()
}

.filter_ignored <- function(files, dir_path, ignore_path) {
  patterns <- .read_ignore_patterns(ignore_path)
  if (!length(patterns) || !length(files)) {
    return(files)
  }

  root <- normalizePath(dir_path, winslash = "/", mustWork = TRUE)
  files <- files |> normalizePath(winslash = "/", mustWork = FALSE)
  rel <- files |> sub(paste0("^", root, "/?"), "", x = _)
  ignored <- rep(FALSE, length(rel))

  for (pattern in patterns) {
    negate <- startsWith(pattern, "!")
    if (negate) {
      pattern <- substring(pattern, 2L)
    }
    if (!nzchar(pattern)) {
      next
    }

    anchored <- startsWith(pattern, "/")
    if (anchored) {
      pattern <- substring(pattern, 2L)
    }
    dir_only <- endsWith(pattern, "/")
    if (dir_only) {
      pattern <- substr(pattern, 1L, nchar(pattern) - 1L)
    }
    if (!nzchar(pattern)) {
      next
    }

    has_slash <- grepl("/", pattern, fixed = TRUE)
    rx <- utils::glob2rx(pattern) |>
      sub("^\\^", "", x = _) |>
      sub("\\$$", "", x = _)

    if (anchored) {
      if (dir_only) {
        rx <- paste0("^", rx, "(/|$)")
      } else {
        rx <- paste0("^", rx, "$")
      }
    } else if (has_slash) {
      if (dir_only) {
        rx <- paste0("(^|.*/)", rx, "(/|$)")
      } else {
        rx <- paste0("(^|.*/)", rx, "$")
      }
    } else {
      if (dir_only) {
        rx <- paste0("(^|.*/)", rx, "(/|$)")
      } else {
        rx <- paste0("(^|.*/)", rx, "$")
      }
    }

    matches <- grepl(rx, rel)
    if (negate) {
      ignored[matches] <- FALSE
    } else {
      ignored[matches] <- TRUE
    }
  }

  files[!ignored]
}

.extract_code <- function(file) {
  ext <- file |>
    sub(".*\\.", "", x = _) |>
    tolower()
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
  pd <- pd |> (\(x) x[order(x$line1, x$col1, x$id), ])()
  token <- pd$token
  text <- pd$text

  attached_pos <- integer()
  pkgs <- character()
  keys <- character()

  choose_attached <- function(candidates) {
    if (length(candidates) == 1L) {
      return(candidates)
    }
    if (!length(attached_pos)) {
      return(candidates[1L])
    }
    pos <- attached_pos[candidates]
    if (all(is.na(pos))) {
      return(candidates[1L])
    }
    pos[is.na(pos)] <- 0L
    candidates[which.max(pos)]
  }

  n <- length(token)
  for (i in seq_len(n)) {
    tok <- token[i]
    txt <- text[i]

    # Track attachment order: library()/require()/requireNamespace()
    if (
      tok == "SYMBOL_FUNCTION_CALL" &&
        txt %in% c("library", "require", "requireNamespace")
    ) {
      pkg <- ""
      if (i < n) {
        j_end <- min(i + 20L, n)
        for (j in seq.int(i + 1L, j_end)) {
          if (token[j] %in% c("RPAR", "')'", ")")) {
            break
          }
          if (token[j] %in% c("SYMBOL", "STR_CONST")) {
            pkg <- gsub("^['\"]|['\"]$", "", text[j])
            break
          }
        }
      }
      if (nzchar(pkg) && pkg %in% .stan_pkgs) {
        pkgs <- c(pkgs, pkg)
      }
      if (nzchar(pkg) && txt %in% c("library", "require")) {
        attach_idx <- if (length(attached_pos)) max(attached_pos) + 1L else 1L
        attached_pos[pkg] <- attach_idx
      }
      next
    }

    # Namespaced calls: pkg::fun / pkg:::fun
    if (
      tok %in%
        c("SYMBOL_FUNCTION_CALL", "SYMBOL") &&
        i >= 2L &&
        token[i - 1L] %in% c("NS_GET", "NS_GET_INT")
    ) {
      pkg_idx <- i - 2L
      while (pkg_idx >= 1L && token[pkg_idx] == "expr") {
        pkg_idx <- pkg_idx - 1L
      }
      if (pkg_idx >= 1L && token[pkg_idx] == "SYMBOL_PACKAGE") {
        pkg <- text[pkg_idx]
        fun <- gsub("^`(.*)`$", "\\1", text[i])
        if (fun %in% .stdlib_funs) {
          next
        }

        if (pkg %in% .stan_pkgs) {
          pkgs <- c(pkgs, pkg)
          keys <- c(keys, paste0(pkg, "::", fun))
        }
        next
      }
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
  funs <- .stan_exports |> unlist(use.names = FALSE)
  pkgs <- rep(names(.stan_exports), lengths(.stan_exports))
  split(pkgs, funs)
})

# stdlib function names to never attribute to Stan pkgs (even if re-exported)
.stdlib_funs <- local({
  c("base", "stats", "utils", "graphics", "grDevices", "methods") |>
    lapply(getNamespaceExports) |>
    unlist(use.names = FALSE) |>
    unique()
})
