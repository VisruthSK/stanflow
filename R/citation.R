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
  ignore_functions = .stdlib_funs,
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
  ignore_functions = .stdlib_funs,
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

.ast_walk <- function(x, acc, ignore_functions, lib_funs) {
  if (is.null(x)) {
    return(invisible(NULL))
  } else if (is.call(x)) {
    acc$pos <- acc$pos + 1L

    head <- x[[1L]]
    head_name <- if (is.symbol(head)) as.character(head) else NULL

    if (!is.null(head_name) && head_name %in% c("::", ":::")) {
      if (length(x) >= 3L) {
        pkg <- .ast_lit_name(x[[2L]])
        fun <- .ast_lit_name(x[[3L]])

        if (
          !is.null(pkg) &&
            !is.null(fun) &&
            pkg %in% .stan_pkgs &&
            !(fun %in% ignore_functions)
        ) {
          acc$ns_pkgs <- c(acc$ns_pkgs, pkg)
          acc$ns_keys <- c(acc$ns_keys, paste0(pkg, "::", fun))
        }
      }
    } else if (!is.null(head_name) && head_name %in% lib_funs) {
      pkg <- .ast_get_lib_pkg(x)
      if (!is.null(pkg) && pkg %in% .stan_pkgs) {
        acc$lib_pkgs <- c(acc$lib_pkgs, pkg)
        acc$lib_pos <- c(acc$lib_pos, acc$pos)
        acc$lib_is_attach <- c(
          acc$lib_is_attach,
          head_name != "requireNamespace"
        )
      }
    } else if (!is.null(head_name)) {
      if (!(head_name %in% ignore_functions)) {
        acc$unqual_funs <- c(acc$unqual_funs, head_name)
        acc$unqual_pos <- c(acc$unqual_pos, acc$pos)
      }
    }

    if (is.call(head)) {
      .ast_walk(head, acc, ignore_functions, lib_funs)
    }

    if (length(x) >= 2L) {
      for (i in 2L:length(x)) {
        .ast_walk(x[[i]], acc, ignore_functions, lib_funs)
      }
    }

    return(invisible(NULL))
  } else if (is.expression(x)) {
    for (i in seq_along(x)) {
      .ast_walk(x[[i]], acc, ignore_functions, lib_funs)
    }
    return(invisible(NULL))
  } else if (is.pairlist(x) || is.list(x)) {
    for (i in seq_along(x)) {
      .ast_walk(x[[i]], acc, ignore_functions, lib_funs)
    }
    return(invisible(NULL))
  }

  invisible(NULL)
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

.ast_lit_name <- function(x) {
  if (is.symbol(x)) {
    return(as.character(x))
  }
  if (is.character(x) && length(x) == 1L) {
    return(x)
  }
  NULL
}

.ast_get_lib_pkg <- function(call) {
  args <- as.list(call)[-1L]
  if (!length(args)) {
    return(NULL)
  }

  nms <- names(args)
  arg <- if (!is.null(nms) && "package" %in% nms) {
    args[[match("package", nms)]]
  } else {
    args[[1L]]
  }

  .ast_lit_name(arg)
}

.scan_tokens <- function(code, ignore_functions, strict = FALSE) {
  empty <- list(pkgs = character(), keys = character(), ambiguous = character())
  expr <- tryCatch(
    parse(text = code, keep.source = FALSE),
    error = function(e) NULL
  )
  if (is.null(expr)) {
    return(empty)
  }

  acc <- new.env(parent = emptyenv())
  acc$pos <- 0L
  acc$lib_pkgs <- character()
  acc$lib_pos <- integer()
  acc$lib_is_attach <- logical()
  acc$ns_pkgs <- character()
  acc$ns_keys <- character()
  acc$unqual_funs <- character()
  acc$unqual_pos <- integer()

  lib_funs <- c("library", "require", "requireNamespace")

  for (i in seq_along(expr)) {
    .ast_walk(expr[[i]], acc, ignore_functions, lib_funs)
  }

  lib_data <- if (length(acc$lib_pkgs)) {
    data.frame(
      pos = acc$lib_pos,
      pkg = acc$lib_pkgs,
      is_attach = acc$lib_is_attach,
      stringsAsFactors = FALSE
    )
  } else {
    NULL
  }

  resolved <- .resolve_candidates(
    list(funs = acc$unqual_funs, idx = acc$unqual_pos),
    lib_data,
    strict
  )

  pkgs <- c(acc$lib_pkgs, acc$ns_pkgs, resolved$pkgs)
  keys <- c(acc$ns_keys, resolved$keys)

  list(
    pkgs = sort(unique(pkgs)),
    keys = sort(unique(keys)),
    ambiguous = sort(unique(resolved$ambiguous))
  )
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
  # lapply(
  #   c("base", "stats", "utils", "graphics", "grDevices", "methods"),
  #   getNamespaceExports
  # ) |>
  #   unlist(use.names = FALSE) |>
  #   unique() |>
  #   sort()
  .stdlib_funs
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
