#' Find used functions and packages
#'
#' This function is primarily exported for developers. The scanner itself is
#' generic and requires an explicit package universe; `stan_cite()` is the
#' Stan-configured entry point.
#' The scanning is wholly static (AST parsing), so there are
#' a number of restrictions on what calls are recognized:
#' calls to `library()`, `require()`, `requireNamespace()`,
#' or `use()` are all recognized as attaching a package.
#'
#' Explicit package references from `library()`, `require()`,
#' `requireNamespace()`, `use()`, and `pkg::fun` are only recorded when their
#' package is included in `allowed_packages`. Unqualified function calls are
#' only attributed when a package is attached via `library()` or `require()` in
#' the same file and `allowed_packages`, `export_index`, and `origin_map`
#' describe how to resolve them. Attaching a metapackage can also be treated as
#' attaching additional packages when `metapackages` is supplied. When multiple
#' attached packages export the same unqualified function, attachment order is
#' respected: the most recently attached matching package whose attachment
#' appears before the call is treated as the winner. Known reexports are
#' remapped to their origin packages; missing mappings fall back to the
#' resolved package.
#'
#' @inheritParams stan_cite
#' @param allowed_packages Character vector of package namespaces to attribute
#'   calls to.
#' @param export_index Named list mapping function names to packages.
#' @param origin_map Named character vector mapping `pkg::fun` keys to the
#'   origin package.
#' @param metapackages Named list mapping attached package names to additional
#'   packages that should be treated as co-attached for unqualified resolution.
#'   Defaults to `NULL`.
#' @param skip_dirs Character vector of directory names to skip when scanning a
#'   directory. Defaults to `.scan_skip_dirs`.
#' @return A list of packages, resolved functions, and ambiguous function calls.
#' @export
#' @examples
#' path <- tempfile(fileext = ".R")
#' writeLines(
#'   c(
#'     "# one messy analysis file",
#'     "library(stats)",
#'     "requireNamespace(\"utils\")",
#'     "filter(1:10, rep(1, 3))",
#'     "utils::head(letters)"
#'   ),
#'   path
#' )
#' scan_usage(
#'   path,
#'   allowed_packages = c("stats", "utils"),
#'   export_index = list(filter = "stats"),
#'   origin_map = c("stats::filter" = "stats"),
#'   ignore_unqualified_functions = character(),
#'   quiet = TRUE
#' )
#' unlink(path)
scan_usage <- function(
  path = ".",
  allowed_packages,
  export_index,
  origin_map,
  ignore_unqualified_functions = .stdlib_funs,
  strict = FALSE,
  skip_dirs = .scan_skip_dirs,
  metapackages = NULL,
  quiet = getOption("stanflow.quiet", FALSE)
) {
  local_cli_quiet(quiet)
  resolver_index <- .scan_resolver_index(export_index, origin_map)
  metapackages <- .normalize_metapackages(metapackages, allowed_packages)

  paths <- normalizePath(path, winslash = "/", mustWork = TRUE)
  dir_flags <- dir.exists(paths)

  files <- if (length(paths) == 1L && dir_flags) {
    dir_path <- paths[[1L]]
    cli::cli_alert_info("Searching directory {.path {dir_path}}")
    .scan_dir_files(dir_path, skip_dirs)
  } else {
    if (any(dir_flags)) {
      cli::cli_abort(c(
        "{.arg path} must be a single directory or a vector of files.",
        "x" = "Mixed directories and files or multiple directories are not supported."
      ))
    }
    lapply(
      paths,
      \(file_path) cli::cli_alert_info("Searching {.path {file_path}}")
    )
    paths
  }

  if (!length(files)) {
    cli::cli_abort(c(
      "No files found.",
      "i" = "Check the {.arg path} and {.arg skip_dirs} arguments."
    ))
  }

  hits <- lapply(
    unique(files),
    \(file) {
      .scan_tokens(
        .extract_code(
          file,
          allowed_packages = c(allowed_packages, names(metapackages))
        ),
        ignore_unqualified_functions = ignore_unqualified_functions,
        allowed_packages = allowed_packages,
        export_index = export_index,
        origin_map = origin_map,
        resolver_index = resolver_index,
        metapackages = metapackages,
        file_path = file
      )
    }
  )

  ambiguous <- .collect_unique(hits, "ambiguous")
  if (length(ambiguous)) {
    msg <- c(
      "Cannot reliably detect which packages some functions are from.",
      "x" = paste0(
        "Ambiguous functions: ",
        paste0("{.fun ", ambiguous, "}", collapse = ", ")
      ),
      "i" = "Please namespace them ({.code pkg::function()}) and rerun or set {.code strict = FALSE}."
    )

    if (strict) cli::cli_abort(msg) else cli::cli_warn(msg)
  }

  structure(
    list(
      packages = .collect_unique(hits, "pkgs"),
      functions = .collect_unique(hits, "keys"),
      ambiguous = ambiguous
    ),
    class = "scan_usage"
  )
}

.scan_skip_regex <- function(skip_dirs) {
  escaped <- vapply(
    skip_dirs,
    \(x) gsub("([][{}()+*^$|\\\\.?])", "\\\\\\1", x),
    character(1)
  )
  paste0("(^|/)(?:", paste(escaped, collapse = "|"), ")(/|$)")
}

.scan_dir_walk <- function(path, skip_dirs, file_cb) {
  entries <- list.files(
    path,
    all.files = TRUE,
    full.names = TRUE,
    no.. = TRUE
  )
  if (!length(entries)) {
    return(invisible(NULL))
  }

  is_dir <- dir.exists(entries)

  if (any(is_dir)) {
    dirs <- entries[is_dir]
    if (length(skip_dirs)) {
      keep <- is.na(fastmatch::fmatch(basename(dirs), skip_dirs))
      dirs <- dirs[keep]
    }
    if (length(dirs)) {
      for (dir in dirs) {
        .scan_dir_walk(dir, skip_dirs, file_cb)
      }
    }
  }

  if (!all(is_dir)) {
    file_cb(entries[!is_dir])
  }

  invisible(NULL)
}

.scan_dir_files <- function(dir_path, skip_dirs) {
  dir_path <- normalizePath(dir_path, winslash = "/", mustWork = TRUE)
  chunks <- list()
  n_chunks <- 0L

  .scan_dir_walk(dir_path, skip_dirs, function(paths) {
    code_files <- paths[grepl("\\.(R|Rmd|Qmd)$", paths, ignore.case = TRUE)]
    if (!length(code_files)) {
      return(invisible(NULL))
    }

    n_chunks <<- n_chunks + 1L
    chunks[[n_chunks]] <<- code_files

    invisible(NULL)
  })
  files <- if (n_chunks) unlist(chunks, use.names = FALSE) else character()
  normalizePath(files, winslash = "/", mustWork = FALSE)
}

.collect_unique <- function(hits, field) {
  hits |>
    lapply(`[[`, field) |>
    unlist(use.names = FALSE) |>
    unique() |>
    sort()
}

.normalize_metapackages <- function(metapackages, allowed_packages) {
  if (is.null(metapackages)) {
    return(NULL)
  }

  lapply(
    metapackages,
    \(pkgs) unique(pkgs[!is.na(fastmatch::fmatch(pkgs, allowed_packages))])
  )
}

.extract_code <- function(file, allowed_packages = NULL) {
  ext <- file |>
    sub(".*\\.", "", x = _) |>
    tolower()

  if (!ext %in% c("r", "rmd", "qmd")) {
    cli::cli_abort(c(
      "Unsupported file extension: {.val {ext}}.",
      "i" = "Supported extensions are {.file .R}, {.file .Rmd}, and {.file .qmd}."
    ))
  }

  raw <- paste(readLines(file, warn = FALSE), collapse = "\n")
  if (
    !is.null(allowed_packages) &&
      !any(vapply(allowed_packages, grepl, logical(1), x = raw, fixed = TRUE))
  ) {
    return("")
  }

  if (ext == "r") {
    return(raw)
  }

  code <- .extract_markdown_code(strsplit(raw, "\n", fixed = TRUE)[[1L]])
  if (
    !nzchar(code) ||
      !inherits(
        try(parse(text = code, keep.source = FALSE), silent = TRUE),
        "try-error"
      )
  ) {
    return(code)
  }
  if (!requireNamespace("knitr", quietly = TRUE)) {
    return(code)
  }

  tmp <- withr::local_tempfile(fileext = ".R")
  knitr::purl(file, tmp, quiet = TRUE, documentation = 0)

  paste(readLines(tmp, warn = FALSE), collapse = "\n")
}

.extract_markdown_code <- function(lines) {
  n <- length(lines)
  if (!n) {
    return("")
  }
  fence_rows <- grep("^\\s*[`~]{3,}", lines, perl = TRUE)
  if (!length(fence_rows)) {
    return("")
  }

  out <- character()
  k <- 1L
  while (k <= length(fence_rows)) {
    i <- fence_rows[[k]]
    cap <- regmatches(
      lines[[i]],
      regexec(
        "^\\s*([`~]{3,})\\s*\\{\\s*[rR]\\b[^}]*\\}\\s*$",
        lines[[i]],
        perl = TRUE
      )
    )[[1L]]
    if (!length(cap)) {
      k <- k + 1L
      next
    }

    fence <- cap[[2L]]
    fence_char <- substr(fence, 1L, 1L)
    close_pat <- paste0(
      "^\\s*",
      gsub("([][{}()+*^$|\\\\.?])", "\\\\\\1", fence_char),
      "{",
      nchar(fence),
      ",}\\s*$"
    )

    start <- i + 1L
    k <- k + 1L
    close_row <- n + 1L
    while (k <= length(fence_rows)) {
      i <- fence_rows[[k]]
      if (grepl(close_pat, lines[[i]], perl = TRUE)) {
        close_row <- i
        break
      }
      k <- k + 1L
    }

    if (close_row > start) {
      out <- c(out, lines[start:(close_row - 1L)], "")
    }
    k <- k + 1L
  }

  paste(out, collapse = "\n")
}

.scan_tokens <- function(
  code,
  ignore_unqualified_functions,
  allowed_packages = .stan_pkgs,
  export_index = .stan_export_index,
  origin_map = .stan_origin_map,
  resolver_index = NULL,
  metapackages = NULL,
  file_path = NULL
) {
  empty <- list(pkgs = character(), keys = character(), ambiguous = character())
  expr <- tryCatch(
    parse(text = code, keep.source = FALSE),
    error = function(e) NULL
  )
  if (is.null(expr)) {
    path_label <- if (!is.null(file_path) && nzchar(file_path)) {
      file_path
    } else {
      "<unknown file>"
    }
    msg <- c(
      "Failed to parse {.path {path_label}}.",
      "x" = "Syntax error in file."
    )
    cli::cli_warn(msg)
    return(empty)
  }

  acc <- new.env(parent = emptyenv())
  acc$visit_idx <- 0L
  acc$lib_pkgs <- character()
  acc$lib_visit_idx <- integer()
  acc$lib_is_attach <- logical()
  acc$ns_pkgs <- character()
  acc$ns_keys <- character()
  acc$unqual_funs <- character()
  acc$unqual_visit_idx <- integer()

  lib_funs <- c("library", "require", "requireNamespace")
  ns_ops <- c("::", ":::")
  use_heads <- c("c", "list")
  ignore_heads <- c(
    "if",
    "for",
    "while",
    "repeat",
    "function",
    "return",
    "next",
    "break",
    "{",
    "(",
    "<-",
    "<<-",
    "->",
    "->>",
    "=",
    "+",
    "-",
    "*",
    "/",
    "^",
    "%%",
    "%/%",
    "%*%",
    "%>%",
    ":",
    "|",
    "&",
    "||",
    "&&",
    "!",
    "~",
    "|>",
    "$",
    "@",
    "[",
    "[["
  )
  export_names <- names(export_index)
  if (is.null(export_names)) {
    export_names <- character()
  }
  if (is.null(resolver_index)) {
    resolver_index <- .scan_resolver_index(export_index, origin_map)
  }

  for (i in seq_along(expr)) {
    .ast_walk(
      expr[[i]],
      acc,
      ignore_unqualified_functions,
      lib_funs,
      allowed_packages,
      ns_ops,
      use_heads,
      ignore_heads,
      export_names,
      metapackages
    )
  }

  lib_data <- if (length(acc$lib_pkgs)) {
    data.frame(
      visit_idx = acc$lib_visit_idx,
      pkg = acc$lib_pkgs,
      is_attach = acc$lib_is_attach,
      stringsAsFactors = FALSE
    )
  } else {
    NULL
  }

  if (is.null(lib_data) || !any(lib_data$is_attach)) {
    return(list(
      pkgs = c(acc$lib_pkgs, acc$ns_pkgs),
      keys = acc$ns_keys,
      ambiguous = character()
    ))
  }

  resolved <- .resolve_candidates(
    list(funs = acc$unqual_funs, idx = acc$unqual_visit_idx),
    lib_data,
    allowed_packages,
    export_index,
    origin_map,
    resolver_index = resolver_index
  )

  list(
    pkgs = c(acc$lib_pkgs, acc$ns_pkgs, resolved$pkgs),
    keys = c(acc$ns_keys, resolved$keys),
    ambiguous = resolved$ambiguous
  )
}

.ast_walk <- function(
  x,
  acc,
  ignore_unqualified_functions,
  lib_funs,
  allowed_packages,
  ns_ops,
  use_heads,
  ignore_heads,
  export_names,
  metapackages
) {
  if (is.null(x)) {
    return(invisible(NULL))
  }

  if (is.call(x)) {
    acc$visit_idx <- acc$visit_idx + 1L

    head <- x[[1L]]
    head_name <- if (is.symbol(head)) as.character(head) else NULL
    member_fun <- .ast_member_fun(head)

    # Member calls (e.g., obj$sample()) are package API methods, not
    # language-level calls; don't suppress them via stdlib ignore lists.
    if (
      !is.null(member_fun) &&
        !is.na(fastmatch::fmatch(member_fun, export_names))
    ) {
      acc$unqual_funs <- c(acc$unqual_funs, member_fun)
      acc$unqual_visit_idx <- c(acc$unqual_visit_idx, acc$visit_idx)
    }

    if (!is.null(head_name)) {
      if (!is.na(fastmatch::fmatch(head_name, ns_ops)) && length(x) >= 3L) {
        pkg <- .ast_lit_name(x[[2L]])
        fun <- .ast_lit_name(x[[3L]])
        if (
          !is.null(pkg) &&
            !is.null(fun) &&
            !is.na(fastmatch::fmatch(pkg, allowed_packages))
        ) {
          acc$ns_pkgs <- c(acc$ns_pkgs, pkg)
          acc$ns_keys <- c(acc$ns_keys, paste0(pkg, "::", fun))
        }
      } else if (head_name == "use") {
        pkg <- .ast_get_lib_pkg(x)
        if (!is.null(pkg) && !is.na(fastmatch::fmatch(pkg, allowed_packages))) {
          acc$ns_pkgs <- c(acc$ns_pkgs, pkg)
          funs <- .ast_get_use_funs(x, use_heads)
          if (length(funs)) {
            acc$ns_keys <- c(acc$ns_keys, paste0(pkg, "::", funs))
          }
        }
      } else if (!is.na(fastmatch::fmatch(head_name, lib_funs))) {
        pkg <- .ast_get_lib_pkg(x)
        if (!is.null(pkg)) {
          is_allowed <- !is.na(fastmatch::fmatch(pkg, allowed_packages))
          is_attach <- head_name != "requireNamespace"

          if (is_allowed) {
            acc$lib_pkgs <- c(acc$lib_pkgs, pkg)
            acc$lib_visit_idx <- c(acc$lib_visit_idx, acc$visit_idx)
            acc$lib_is_attach <- c(acc$lib_is_attach, is_attach)
          }

          if (is_attach && !is.null(metapackages)) {
            expanded_pkgs <- metapackages[[pkg]]
            if (length(expanded_pkgs)) {
              acc$lib_pkgs <- c(acc$lib_pkgs, expanded_pkgs)
              acc$lib_visit_idx <- c(
                acc$lib_visit_idx,
                rep.int(acc$visit_idx, length(expanded_pkgs))
              )
              acc$lib_is_attach <- c(
                acc$lib_is_attach,
                rep.int(TRUE, length(expanded_pkgs))
              )
            }
          }
        }
      } else if (!is.na(fastmatch::fmatch(head_name, ignore_heads))) {
        # Skip language keywords, operators, and subsetting.
      } else if (is.na(fastmatch::fmatch(head_name, export_names))) {
        # Ignore non-Stan calls before consulting broader ignore lists.
      } else if (
        is.na(fastmatch::fmatch(head_name, ignore_unqualified_functions))
      ) {
        acc$unqual_funs <- c(acc$unqual_funs, head_name)
        acc$unqual_visit_idx <- c(acc$unqual_visit_idx, acc$visit_idx)
      }
    }

    if (is.call(head)) {
      .ast_walk(
        head,
        acc,
        ignore_unqualified_functions,
        lib_funs,
        allowed_packages,
        ns_ops,
        use_heads,
        ignore_heads,
        export_names,
        metapackages
      )
    }
    n <- length(x)
    if (n >= 2L) {
      for (i in 2L:n) {
        .ast_walk(
          x[[i]],
          acc,
          ignore_unqualified_functions,
          lib_funs,
          allowed_packages,
          ns_ops,
          use_heads,
          ignore_heads,
          export_names,
          metapackages
        )
      }
    }
    return(invisible(NULL))
  }

  if (is.expression(x) || is.pairlist(x) || is.list(x)) {
    for (i in seq_along(x)) {
      .ast_walk(
        x[[i]],
        acc,
        ignore_unqualified_functions,
        lib_funs,
        allowed_packages,
        ns_ops,
        use_heads,
        ignore_heads,
        export_names,
        metapackages
      )
    }
    return(invisible(NULL))
  }

  invisible(NULL)
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

.ast_member_fun <- function(head) {
  if (!is.call(head) || !length(head)) {
    return(NULL)
  }

  op <- head[[1L]]
  if (!is.symbol(op)) {
    return(NULL)
  }

  op_name <- as.character(op)

  if (op_name %in% c("$", "@") && length(head) >= 3L) {
    return(.ast_lit_name(head[[3L]]))
  }

  if (op_name == "(" && length(head) >= 2L) {
    return(.ast_member_fun(head[[2L]]))
  }

  NULL
}

.ast_get_lib_pkg <- function(call) {
  n <- length(call)
  if (n <= 1L) {
    return(NULL)
  }

  nms <- names(call)
  arg_nms <- if (!is.null(nms) && n >= 2L) nms[-1L] else NULL
  pkg_i <- if (!is.null(arg_nms)) {
    fastmatch::fmatch("package", arg_nms)
  } else {
    NA_integer_
  }
  pkg_j <- if (!is.null(arg_nms)) {
    fastmatch::fmatch("pkg", arg_nms)
  } else {
    NA_integer_
  }

  arg_idx <- if (!is.na(pkg_i)) {
    pkg_i + 1L
  } else if (!is.na(pkg_j)) {
    pkg_j + 1L
  } else {
    2L
  }

  .ast_lit_name(call[[arg_idx]])
}

.ast_collect_use_funs <- function(x, use_heads) {
  if (is.null(x)) {
    return(character())
  }

  lit <- .ast_lit_name(x)
  if (!is.null(lit)) {
    return(lit)
  }

  if (is.call(x)) {
    head <- x[[1L]]
    head_name <- if (is.symbol(head)) as.character(head) else NULL
    if (
      !is.null(head_name) && !is.na(fastmatch::fmatch(head_name, use_heads))
    ) {
      n <- length(x)
      if (n <= 1L) {
        return(character())
      }
      out <- vector("list", n - 1L)
      for (i in 2L:n) {
        out[[i - 1L]] <- .ast_collect_use_funs(x[[i]], use_heads = use_heads)
      }
      return(unlist(out, use.names = FALSE))
    }
  }

  character()
}

.ast_get_use_funs <- function(call, use_heads) {
  n <- length(call)
  if (n <= 2L) {
    return(character())
  }

  nms <- names(call)
  arg_nms <- if (!is.null(nms)) nms[-1L] else NULL
  pkg_i <- if (!is.null(arg_nms)) {
    fastmatch::fmatch("pkg", arg_nms)
  } else {
    NA_integer_
  }
  pkg_j <- if (!is.null(arg_nms)) {
    fastmatch::fmatch("package", arg_nms)
  } else {
    NA_integer_
  }

  pkg_idx <- if (!is.na(pkg_i)) {
    pkg_i
  } else if (!is.na(pkg_j)) {
    pkg_j
  } else {
    1L
  }

  out <- vector("list", n - 2L)
  j <- 0L
  for (i in 2L:n) {
    arg_idx <- i - 1L
    if (arg_idx == pkg_idx) {
      next
    }
    j <- j + 1L
    out[[j]] <- .ast_collect_use_funs(call[[i]], use_heads = use_heads)
  }
  funs <- unlist(out, use.names = FALSE)
  funs[nzchar(funs)]
}

.scan_resolver_index <- function(
  export_index = .stan_export_index,
  origin_map = .stan_origin_map
) {
  funs <- names(export_index)
  if (is.null(funs)) {
    return(list())
  }

  stats::setNames(
    lapply(
      funs,
      \(fun) {
        providers <- export_index[[fun]]
        if (is.null(providers) || !length(providers)) {
          return(NULL)
        }

        list(
          provider = providers,
          origin = vapply(
            providers,
            \(pkg) {
              origin <- unname(origin_map[paste0(pkg, "::", fun)])
              if (is.na(origin)) pkg else origin
            },
            character(1),
            USE.NAMES = FALSE
          )
        )
      }
    ),
    funs
  )
}

.resolve_meta <- function(
  fun,
  attached,
  allowed_packages,
  resolver_index
) {
  meta <- resolver_index[[fun]]
  if (is.null(meta) || !length(meta$provider)) {
    return(NULL)
  }

  keep <- !is.na(fastmatch::fmatch(meta$provider, allowed_packages)) &
    !is.na(fastmatch::fmatch(meta$provider, attached$pkg))
  if (!any(keep)) {
    return(NULL)
  }

  origin <- meta$origin[keep]
  origin_allowed <- !is.na(fastmatch::fmatch(origin, allowed_packages))
  if (!any(origin_allowed)) {
    return(NULL)
  }

  list(
    provider = meta$provider[keep],
    origin = origin,
    origin_allowed = origin_allowed
  )
}

.resolve_calls <- function(
  meta,
  attached,
  attached_rows,
  visit_idx,
  allowed_packages
) {
  allowed_origins <- unique(meta$origin[meta$origin_allowed])
  if (length(allowed_origins) == 1L) {
    return(rep.int(allowed_origins[[1L]], length(visit_idx)))
  }

  attached_match_idx <- do.call(
    cbind,
    lapply(
      meta$provider,
      \(pkg) {
        provider_rows <- attached_rows[[pkg]]
        hits <- findInterval(visit_idx, attached$visit_idx[provider_rows])
        out <- rep.int(-1L, length(visit_idx))
        matched <- hits > 0L
        out[matched] <- provider_rows[hits[matched]]
        out
      }
    )
  )

  best_provider <- max.col(attached_match_idx, ties.method = "first")
  matched <- attached_match_idx[
    cbind(seq_along(best_provider), best_provider)
  ]
  resolved <- rep.int("", length(visit_idx))
  keep <- matched > 0L
  if (!any(keep)) {
    return(resolved)
  }

  resolved_provider <- meta$provider[best_provider[keep]]
  resolved_origin <- meta$origin[best_provider[keep]]
  resolved[keep] <- ifelse(
    is.na(fastmatch::fmatch(resolved_origin, allowed_packages)),
    resolved_provider,
    resolved_origin
  )
  resolved
}

.resolve_candidates <- function(
  unqual,
  lib_data,
  allowed_packages = .stan_pkgs,
  export_index = .stan_export_index,
  origin_map = .stan_origin_map,
  resolver_index = NULL
) {
  empty <- list(pkgs = character(), keys = character(), ambiguous = character())
  if (!length(unqual$funs) || !length(allowed_packages)) {
    return(empty)
  }
  if (is.null(resolver_index)) {
    resolver_index <- .scan_resolver_index(export_index, origin_map)
  }
  if (is.null(lib_data) || !any(lib_data$is_attach)) {
    return(empty)
  }

  attached <- list(
    visit_idx = lib_data$visit_idx[lib_data$is_attach],
    pkg = lib_data$pkg[lib_data$is_attach]
  )
  if (length(attached$visit_idx) > 1L) {
    ord <- order(attached$visit_idx, seq_along(attached$visit_idx))
    attached$visit_idx <- attached$visit_idx[ord]
    attached$pkg <- attached$pkg[ord]
  }
  attached_rows <- split(seq_along(attached$pkg), attached$pkg)

  resolved_pkgs <- rep.int("", length(unqual$funs))
  considered <- logical(length(unqual$funs))
  call_groups <- split(seq_along(unqual$funs), unqual$funs)
  for (fun in names(call_groups)) {
    idx <- call_groups[[fun]]
    meta <- .resolve_meta(
      fun = fun,
      attached = attached,
      allowed_packages = allowed_packages,
      resolver_index = resolver_index
    )
    if (is.null(meta)) {
      next
    }

    considered[idx] <- TRUE
    resolved_pkgs[idx] <- .resolve_calls(
      meta = meta,
      attached = attached,
      attached_rows = attached_rows,
      visit_idx = unqual$idx[idx],
      allowed_packages = allowed_packages
    )
  }
  if (!any(considered)) {
    return(empty)
  }

  resolved <- nzchar(resolved_pkgs)
  list(
    pkgs = resolved_pkgs[resolved],
    keys = if (any(resolved)) {
      paste0(resolved_pkgs[resolved], "::", unqual$funs[resolved])
    } else {
      character()
    },
    ambiguous = if (all(!considered | resolved)) {
      character()
    } else {
      sort(unique(unqual$funs[considered & !resolved]))
    }
  )
}

#' Ignored functions/directories used by scanner
#'
#' @name internal_data
#' @rdname internal_data
#' @keywords internal
NULL

#' Default ignored functions
#'
#' Vector of functions to be ignored when parsing.
#' Generated in `data-raw/sysdata.R` from exports of base R packages.
#'
#' @rdname internal_data
#' @export
#' @examples
#' head(stdlib_funs())
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

#' Default skip directories
#'
#' Vector of directories skipped when recursively searching
#' a project. Generated in `data-raw/sysdata.R`.
#'
#' @rdname internal_data
#' @export
#' @examples
#' scan_skip_dirs()
scan_skip_dirs <- function() {
  # c(
  #   "renv",
  #   "packrat",
  #   "rv",
  #   ".Rcheck",
  #   "revdep",
  #   "_site",
  #   "_book",
  #   "_bookdown_files",
  #   "_freeze",
  #   ".quarto",
  #   ".quarto_cache",
  #   ".knitr_cache",
  #   "_cache",
  #   ".cache"
  # )
  .scan_skip_dirs
}
