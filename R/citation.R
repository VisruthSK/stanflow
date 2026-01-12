#' Collect citations for Stan usage
#'
#' `stan_scan_usage()` is primarily for developers; most users should call
#' `stan_cite()`.
#'
#' @param path A single project directory (searched recursively) or a vector of
#'   files (.R/.Rmd/.Qmd).
#' @param ignore_functions Character vector of function names to ignore when
#'   attributing calls to Stan packages. Defaults to exports from
#'   base R packages listed in `stdlib_funs()`.
#' @param quiet Silence informational output.
#' @param strict If `TRUE`, only count unqualified function calls that resolve
#'   to a single Stan package.
#' @param format One of "bibtex" or "bibentry".
#' @return A BibTeX character vector or a bibentry object.
#' @export
stan_cite <- function(
  path = ".",
  ignore_functions = stdlib_funs(),
  quiet = FALSE,
  strict = FALSE,
  format = c("bibtex", "bibentry")
) {
  stan_scan_usage(
    path = path,
    ignore_functions = ignore_functions,
    quiet = quiet,
    strict = strict
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
#' @export
#' @return list(packages=character(), functions=character())
#' @rdname stan_cite
stan_scan_usage <- function(
  path = ".",
  ignore_functions = stdlib_funs(),
  quiet = FALSE,
  strict = FALSE
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
    )
  } else {
    if (any(dir_flags)) {
      cli::cli_abort(
        "{.arg path} must be a single directory or a vector of files."
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
          .scan_tokens(ignore_functions = ignore_functions, strict = strict)
      }
    )

  ambiguous <- .collect_unique(hits, "ambiguous")
  if (strict && length(ambiguous)) {
    cli::cli_alert_warning(
      ambiguous |>
        (\(funs) paste0("{.code ", funs, "()}"))() |>
        paste(collapse = ", ") |>
        (\(calls) {
          paste0(
            "Cannot reliably detect which packages these functions are from: ",
            calls,
            ". Please namespace them ({.code pkg::function()}) and re-run stan_cite()."
          )
        })()
    )
  }

  structure(
    list(
      packages = .collect_unique(hits, "pkgs"),
      functions = .collect_unique(hits, "keys"),
      ambiguous = ambiguous
    ),
    class = "stan_scan_usage"
  )
}

.collect_unique <- function(hits, field) {
  hits |>
    lapply(`[[`, field) |>
    unlist(use.names = FALSE) |>
    unique() |>
    sort()
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

.scan_tokens <- function(code, ignore_functions, strict = FALSE) {
  expr <- tryCatch(parse(text = code, keep.source = TRUE), error = function(e) {
    NULL
  })
  if (is.null(expr)) {
    return(list(
      pkgs = character(),
      keys = character(),
      ambiguous = character()
    ))
  }

  pd <- getParseData(expr, includeText = TRUE)
  if (is.null(pd) || nrow(pd) == 0) {
    return(list(
      pkgs = character(),
      keys = character(),
      ambiguous = character()
    ))
  }

  pd <- pd[pd$terminal, ]
  token <- pd$token
  text <- pd$text
  n <- length(token)

  lib_res <- .find_library_calls(token, text, n)
  ns_res <- .find_namespaced_calls(token, text, n, ignore_functions)
  resolved <- .find_unqualified_calls(
    token,
    text,
    n,
    lib_res$idx,
    ignore_functions
  ) |>
    .resolve_candidates(lib_res$data, strict)

  pkgs <- c(
    lib_res$data$pkg,
    ns_res$pkgs,
    resolved$pkgs
  )
  keys <- c(ns_res$keys, resolved$keys)

  list(
    pkgs = sort(unique(pkgs)),
    keys = sort(unique(keys)),
    ambiguous = sort(unique(resolved$ambiguous))
  )
}

.find_library_calls <- function(token, text, n) {
  idx <- which(
    token == "SYMBOL_FUNCTION_CALL" &
      text %in% c("library", "require", "requireNamespace")
  )

  if (!length(idx)) {
    return(list(idx = integer(), data = NULL))
  }

  data <- do.call(
    rbind,
    lapply(idx, function(i) {
      pkg <- ""
      for (j in (i + 1):min(i + 10L, n)) {
        if (token[j] %in% c("RPAR", "')'", ")")) {
          break
        }
        if (token[j] %in% c("SYMBOL", "STR_CONST")) {
          pkg <- gsub("^['\"]|['\"]$", "", text[j])
          break
        }
      }
      if (nzchar(pkg) && pkg %in% .stan_pkgs) {
        data.frame(
          pos = i,
          pkg = pkg,
          is_attach = text[i] != "requireNamespace",
          stringsAsFactors = FALSE
        )
      } else {
        NULL
      }
    })
  )

  list(idx = idx, data = data)
}

.find_namespaced_calls <- function(token, text, n, ignore_functions) {
  idx <- which(token %in% c("NS_GET", "NS_GET_INT"))

  if (!length(idx)) {
    return(list(pkgs = character(), keys = character()))
  }

  valid <- idx > 1 & idx < n
  idx <- idx[valid]

  pkg_idx <- idx - 1L
  fun_idx <- idx + 1L

  is_stan_pkg <- text[pkg_idx] %in% .stan_pkgs
  pkg_idx <- pkg_idx[is_stan_pkg]
  fun_idx <- fun_idx[is_stan_pkg]

  if (!length(pkg_idx)) {
    return(list(pkgs = character(), keys = character()))
  }

  funs <- gsub("^`(.*)`$", "\\1", text[fun_idx])
  keep <- !(funs %in% ignore_functions)

  if (!any(keep)) {
    return(list(pkgs = character(), keys = character()))
  }

  pkgs <- text[pkg_idx[keep]]
  keys <- paste0(pkgs, "::", funs[keep])

  list(pkgs = pkgs, keys = keys)
}

.find_unqualified_calls <- function(token, text, n, lib_idx, ignore_functions) {
  is_ns_get <- token %in% c("NS_GET", "NS_GET_INT")
  prev_is_ns <- c(FALSE, is_ns_get[-n])

  is_lib_call <- logical(n)
  if (length(lib_idx)) {
    is_lib_call[lib_idx] <- TRUE
  }

  idx <- which(
    token == "SYMBOL_FUNCTION_CALL" & !prev_is_ns & !is_lib_call
  )

  if (!length(idx)) {
    return(list(funs = character(), idx = integer()))
  }

  funs <- gsub("^`(.*)`$", "\\1", text[idx])
  keep <- !(funs %in% ignore_functions)

  list(funs = funs[keep], idx = idx[keep])
}

.resolve_candidates <- function(unqual, lib_data, strict) {
  if (!length(unqual$funs)) {
    return(list(
      pkgs = character(),
      keys = character(),
      ambiguous = character()
    ))
  }

  candidates_list <- .stan_export_index[unqual$funs]
  has_candidates <- !vapply(candidates_list, is.null, logical(1))

  if (!any(has_candidates)) {
    return(list(
      pkgs = character(),
      keys = character(),
      ambiguous = character()
    ))
  }

  idx <- unqual$idx[has_candidates]
  funs <- unqual$funs[has_candidates]
  candidates_list <- candidates_list[has_candidates]

  n_cand <- lengths(candidates_list)
  is_ambig <- n_cand > 1

  pkgs <- character()
  keys <- character()
  ambiguous <- character()

  # Unambiguous calls
  if (!all(is_ambig)) {
    best_pkgs <- unlist(candidates_list[!is_ambig], use.names = FALSE)
    pkgs <- c(pkgs, best_pkgs)
    keys <- c(keys, paste0(best_pkgs, "::", funs[!is_ambig]))
  }

  # Ambiguous calls
  if (any(is_ambig)) {
    ambig_idx <- idx[is_ambig]
    ambig_funs <- funs[is_ambig]
    ambig_cands <- candidates_list[is_ambig]
    ambiguous <- sort(unique(ambig_funs))

    if (!strict) {
      attaching_pkgs <- character(0)
      lib_pos <- integer(0)
      if (!is.null(lib_data) && any(lib_data$is_attach)) {
        attaching_pkgs <- lib_data$pkg[lib_data$is_attach]
        lib_pos <- lib_data$pos[lib_data$is_attach]
      }

      intervals <- findInterval(ambig_idx, lib_pos)

      for (i in seq_along(ambig_idx)) {
        k <- intervals[i]
        cands <- ambig_cands[[i]]

        if (k == 0) {
          pkg <- cands[1]
        } else {
          attached_before <- attaching_pkgs[seq_len(k)]
          matches <- match(cands, attached_before)
          if (all(is.na(matches))) {
            pkg <- cands[1]
          } else {
            pkg <- cands[which.max(ifelse(is.na(matches), -1L, matches))]
          }
        }
        pkgs <- c(pkgs, pkg)
        keys <- c(keys, paste0(pkg, "::", ambig_funs[i]))
      }
    }
  }

  list(pkgs = pkgs, keys = keys, ambiguous = ambiguous)
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

#' @export
print.stan_scan_usage <- function(x, ...) {
  pkg_count <- length(x$packages)
  fun_count <- length(x$functions)

  if (!pkg_count && !fun_count) {
    cli::cli_alert_info(
      "No Stan function calls found."
    )
    return(invisible(x))
  }

  header <- cli::rule(
    left = cli::style_bold("Stan usage"),
    right = "stan_scan_usage()"
  )
  cli::cat_line(header)

  if (pkg_count) {
    cli::cat_line(cli::col_blue("Packages"), " (", pkg_count, "):")
    cli::cat_line("  ", paste(x$packages, collapse = ", "))
  } else {
    cli::cat_line(cli::col_blue("Packages"), ": <none>")
  }

  if (fun_count) {
    cli::cat_line(cli::col_blue("Functions"), " (", fun_count, "):")

    x$functions |>
      (\(funs) {
        split(
          sub("^.*::", "", funs),
          sub("::.*$", "", funs)
        ) |>
          lapply(sort) |>
          (\(funs_by_pkg) {
            vapply(
              sort(names(funs_by_pkg)),
              \(pkg_name) {
                fun_calls <- paste0(
                  "{.code ",
                  funs_by_pkg[[pkg_name]],
                  "()}"
                ) |>
                  paste(collapse = ", ")
                cli::format_inline(paste0(
                  "  ",
                  pkg_name,
                  " (",
                  length(funs_by_pkg[[pkg_name]]),
                  "): ",
                  fun_calls
                ))
              },
              character(1)
            ) |>
              paste(collapse = "\n")
          })()
      })() |>
      cli::cat_line()
  } else {
    cli::cat_line(cli::col_blue("Functions"), ": <none>")
  }

  invisible(x)
}
