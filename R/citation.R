#' Collect BibTeX citations for Stan usage
#'
#' `stan_scan_usage()` is primarily for developers; most users should call
#' `stan_cite()`.
#'
#' @param path A single project directory (searched recursively) or a vector of
#'   files (.R/.Rmd/.Qmd).
#' @param ignore_files Path to a .gitignore-style file used to exclude paths while
#'   searching a directory.
#' @param ignore_functions Character vector of function names to ignore when
#'   attributing calls to Stan packages. Defaults to exports from
#'   base R packages listed in `stdlib_funs()`.
#' @param quiet Silence informational output.
#' @param format One of "bibtex" or "bibentry".
#' @return A BibTeX character vector or a bibentry object.
#' @export
stan_cite <- function(
  path = ".",
  ignore_files = NULL,
  ignore_functions = stdlib_funs(),
  quiet = FALSE,
  format = c("bibtex", "bibentry")
) {
  stan_scan_usage(
    path = path,
    ignore_files = ignore_files,
    ignore_functions = ignore_functions,
    quiet = quiet
  ) |>
    (\(usage) c(usage$packages, usage$functions, "stan", "stanflow"))() |>
    unique() |>
    (\(keys) {
      mget(
        keys,
        envir = .stan_citation_funs,
        inherits = TRUE,
        ifnotfound = list(NULL)
      )
    })() |>
    (\(entries) {
      entries <- entries[!vapply(entries, is.null, logical(1))]
      if (!length(entries)) {
        character()
      } else {
        entries <- do.call(what = c, args = entries)
        if (identical(match.arg(format, c("bibtex", "bibentry")), "bibentry")) {
          entries
        } else {
          toBibtex(entries)
        }
      }
    })()
}

#' Find Stan packages + Stan functions used
#'
#' @return list(packages=character(), functions=character())
#' @rdname stan_cite
stan_scan_usage <- function(
  path = ".",
  ignore_files = NULL,
  ignore_functions = stdlib_funs(),
  quiet = FALSE
) {
  local_cli_quiet(quiet)
  paths <- normalizePath(path, winslash = "/", mustWork = TRUE)
  dir_flags <- dir.exists(paths)

  files <- if (length(paths) == 1L && dir_flags) {
    dir_path <- paths[[1L]]
    cli::cli_alert_info("Searching directory {.path {dir_path}}")
    list.files(
      dir_path,
      recursive = TRUE,
      full.names = TRUE,
      ignore.case = TRUE,
      pattern = "\\.(R|Rmd|Qmd)$"
    ) |>
      (\(files) {
        if (is.null(ignore_files)) {
          files
        } else {
          if (!file.exists(ignore_files)) {
            cli::cli_abort(
              "{.arg ignore_files} must point to an existing file."
            )
          }
          files |>
            .filter_ignored(
              dir_path,
              normalizePath(ignore_files, winslash = "/", mustWork = TRUE)
            )
        }
      })()
  } else {
    if (any(dir_flags)) {
      cli::cli_abort(
        "{.arg path} must be a single directory or a vector of files."
      )
    }
    if (!is.null(ignore_files)) {
      cli::cli_abort(
        "{.arg ignore_files} can only be used when {.arg path} is a directory."
      )
    }
    paths |>
      (\(paths) {
        lapply(
          paths,
          \(file_path) cli::cli_alert_info("Searching {.path {file_path}}")
        )
        paths
      })()
  }

  if (!length(files)) {
    cli::cli_abort("No files found.")
  }

  hits <- unique(files) |>
    lapply(
      \(file) {
        file |>
          .extract_code() |>
          .scan_tokens(ignore_functions = ignore_functions)
      }
    )

  list(
    packages = hits |>
      lapply(`[[`, "pkgs") |>
      unlist(use.names = FALSE) |>
      unique() |>
      sort(),
    functions = hits |>
      lapply(`[[`, "keys") |>
      unlist(use.names = FALSE) |>
      unique() |>
      sort()
  )
}

.filter_ignored <- function(files, dir_path, ignore_path) {
  patterns <- ignore_path |>
    readLines(warn = FALSE) |>
    trimws() |>
    (\(x) x[nzchar(x) & !startsWith(x, "#")])()

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

    rx <- glob2rx(pattern) |>
      sub("^\\^", "", x = _) |>
      sub("\\$$", "", x = _)

    if (anchored) {
      if (dir_only) {
        rx <- paste0("^", rx, "(/|$)")
      } else {
        rx <- paste0("^", rx, "$")
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
  } else if (ext == "qmd") {
    quarto::qmd_to_r_script(file, tmp)
  }

  paste(readLines(tmp, warn = FALSE), collapse = "\n")
}

.scan_tokens <- function(code, ignore_functions) {
  expr <- tryCatch(parse(text = code, keep.source = TRUE), error = function(e) {
    NULL
  })
  if (is.null(expr)) {
    return(list(pkgs = character(), keys = character()))
  }

  pd <- getParseData(expr, includeText = TRUE) |>
    (\(x) x[order(x$line1, x$col1, x$id), ])()
  token <- pd$token
  text <- pd$text

  attached_pos <- integer()
  pkgs <- character()
  keys <- character()
  ignore_functions <- unique(ignore_functions)

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
        if (fun %in% ignore_functions) {
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
      if (fun %in% ignore_functions) {
        next
      }

      candidates <- split(
        rep(names(.stan_exports), lengths(.stan_exports)),
        .stan_exports |> unlist(use.names = FALSE)
      )[[fun]]
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

#' Standard-library function names to never attribute to Stan packages
#'
#' This includes exports from: base, stats, utils, graphics, grDevices, methods.
#'
#' @return Character vector of standard-library function names.
#' @export
stdlib_funs <- function() {
  c("base", "stats", "utils", "graphics", "grDevices", "methods") |>
    lapply(getNamespaceExports) |>
    unlist(use.names = FALSE) |>
    unique()
}
