#' Find used functions and packages
#'
#' This function is primarily exported for developers. The scanner itself is
#' generic and requires an explicit package universe; `stan_cite()` is the
#' Stan-configured entry point.
#' The scanning is wholly static (tree-sitter parsing), so there are
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
#'     "library(posterior)",
#'     "requireNamespace(\"loo\")",
#'     "draws <- as_draws(list(mu = rnorm(10)))",
#'     "posterior::rhat(draws)",
#'     "loo::loo(matrix(1))"
#'   ),
#'   path
#' )
#' scan_usage(
#'   path,
#'   allowed_packages = c("posterior", "loo"),
#'   export_index = list(as_draws = "posterior"),
#'   origin_map = c("posterior::as_draws" = "posterior"),
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
        .extract_code(file),
        ignore_unqualified_functions = ignore_unqualified_functions,
        allowed_packages = allowed_packages,
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

.extract_code <- function(file) {
  ext <- file |>
    sub(".*\\.", "", x = _) |>
    tolower()

  if (!ext %in% c("r", "rmd", "qmd")) {
    cli::cli_abort(c(
      "Unsupported file extension: {.val {ext}}.",
      "i" = "Supported extensions are {.file .R}, {.file .Rmd}, and {.file .qmd}."
    ))
  }

  if (ext == "r") {
    return(paste(readLines(file, warn = FALSE), collapse = "\n"))
  }
  if (!requireNamespace("knitr", quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg knitr} is required to parse R Markdown ({.file .Rmd}) or Quarto ({.file .qmd}) files.",
      "i" = "Please install it with {.code install.packages('knitr')}."
    ))
  }

  tmp <- withr::local_tempfile(fileext = ".R")
  knitr::purl(file, tmp, quiet = TRUE, documentation = 0)

  paste(readLines(tmp, warn = FALSE), collapse = "\n")
}

.scan_tokens <- function(
  code,
  ignore_unqualified_functions,
  allowed_packages,
  resolver_index,
  metapackages = NULL,
  file_path = NULL
) {
  empty <- list(pkgs = character(), keys = character(), ambiguous = character())
  if (!nzchar(code)) {
    return(empty)
  }

  scan_state <- .scan_treesitter()
  tree <- treesitter::parser_parse(scan_state$parser, code)
  root <- treesitter::tree_root_node(tree)

  if (treesitter::node_has_error(root)) {
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

  attached <- .scan_collect_attached(
    root = root,
    collector = scan_state$collectors$attached,
    allowed_packages = allowed_packages,
    metapackages = metapackages
  )
  explicit <- .scan_collect_explicit(
    root = root,
    collector = scan_state$collectors$explicit,
    allowed_packages = allowed_packages
  )
  if (!any(attached$is_attach)) {
    return(list(
      pkgs = c(attached$pkg, explicit$pkg),
      keys = explicit$key[nzchar(explicit$key)],
      ambiguous = character()
    ))
  }

  export_names <- names(resolver_index)
  if (is.null(export_names)) {
    export_names <- character()
  }
  candidates <- .scan_collect_candidates(
    root = root,
    collector = scan_state$collectors$candidate,
    export_names = export_names,
    ignore_unqualified_functions = ignore_unqualified_functions
  )

  resolved <- .resolve_candidates(
    candidates = candidates,
    attached = attached,
    allowed_packages = allowed_packages,
    resolver_index = resolver_index
  )

  list(
    pkgs = c(
      attached$pkg,
      explicit$pkg,
      resolved$pkgs
    ),
    keys = c(explicit$key[nzchar(explicit$key)], resolved$keys),
    ambiguous = resolved$ambiguous
  )
}

.scan_treesitter_cache <- new.env(parent = emptyenv())

.scan_treesitter <- function() {
  bundle <- .scan_treesitter_cache$bundle
  if (!is.null(bundle)) {
    return(bundle)
  }

  language <- treesitter.r::language()
  bundle <- list(
    parser = treesitter::parser(language),
    collectors = lapply(
      .scan_collector_specs,
      \(spec) {
        list(
          query = treesitter::query(
            language,
            .scan_query_sources[[spec$query]]
          ),
          order_capture = spec$order_capture
        )
      }
    )
  )
  .scan_treesitter_cache$bundle <- bundle
  bundle
}

.scan_attached_pkgs <- function(
  visit_idx,
  pkg,
  is_attach,
  allowed_packages,
  metapackages = NULL
) {
  if (is.null(pkg) || !nzchar(pkg)) {
    return(NULL)
  }

  pkgs <- if (!is.na(fastmatch::fmatch(pkg, allowed_packages))) {
    pkg
  } else {
    character()
  }
  attach_flags <- if (length(pkgs)) is_attach else logical()

  expanded <- if (is_attach && !is.null(metapackages)) {
    metapackages[[pkg]]
  } else {
    NULL
  }
  if (length(expanded)) {
    pkgs <- c(pkgs, expanded)
    attach_flags <- c(attach_flags, rep.int(TRUE, length(expanded)))
  }

  if (!length(pkgs)) {
    return(NULL)
  }

  list(
    visit_idx = rep.int(as.integer(visit_idx), length(pkgs)),
    pkg = pkgs,
    is_attach = attach_flags
  )
}

.scan_subset_records <- function(records, keep) {
  stats::setNames(lapply(records, `[`, keep), names(records))
}

.scan_bind_records <- function(rows, type) {
  rows <- Filter(\(row) !is.null(row), rows)
  out <- switch(
    type,
    attached = list(
      visit_idx = integer(),
      pkg = character(),
      is_attach = logical()
    ),
    explicit = list(
      pkg = character(),
      key = character()
    ),
    candidate = list(
      visit_idx = integer(),
      fun = character()
    )
  )
  if (!length(rows)) {
    return(out)
  }

  for (field in names(out)) {
    out[[field]] <- unlist(lapply(rows, `[[`, field), use.names = FALSE)
  }

  out
}

.scan_collect <- function(root, collector, type, build_row) {
  .scan_bind_records(
    lapply(
      .scan_matches(root, collector$query, collector$order_capture),
      build_row
    ),
    type
  )
}

.scan_collect_attached <- function(
  root,
  collector,
  allowed_packages,
  metapackages = NULL
) {
  .scan_collect(
    root = root,
    collector = collector,
    type = "attached",
    \(match) {
      call <- .scan_capture(match, "call")
      .scan_attached_pkgs(
        visit_idx = treesitter::node_start_byte(call),
        pkg = .scan_name(.scan_capture(match, "pkg")),
        is_attach = !identical(
          .scan_name(.scan_capture(match, "head")),
          "requireNamespace"
        ),
        allowed_packages = allowed_packages,
        metapackages = metapackages
      )
    }
  )
}

.scan_collect_explicit <- function(root, collector, allowed_packages) {
  .scan_collect(
    root = root,
    collector = collector,
    type = "explicit",
    \(match) {
      pkg <- .scan_name(.scan_capture(match, "pkg"))
      if (is.null(pkg) || is.na(fastmatch::fmatch(pkg, allowed_packages))) {
        return(NULL)
      }

      fun <- .scan_name(.scan_capture(match, "fun"))
      list(
        pkg = pkg,
        key = if (is.null(fun) || !nzchar(fun)) "" else paste0(pkg, "::", fun)
      )
    }
  )
}

.scan_collect_candidates <- function(
  root,
  collector,
  export_names,
  ignore_unqualified_functions
) {
  .scan_collect(
    root = root,
    collector = collector,
    type = "candidate",
    \(match) {
      call <- .scan_capture(match, "call")
      fun <- .scan_name(.scan_capture(match, "fun"))
      if (is.null(fun) || is.na(fastmatch::fmatch(fun, export_names))) {
        return(NULL)
      }

      fn_node <- treesitter::node_child_by_field_name(call, "function")
      is_plain_call <- !is.null(fn_node) &&
        identical(treesitter::node_type(fn_node), "identifier")
      if (
        is_plain_call &&
          !is.na(fastmatch::fmatch(fun, ignore_unqualified_functions))
      ) {
        return(NULL)
      }

      list(
        visit_idx = as.integer(treesitter::node_start_byte(call)),
        fun = fun
      )
    }
  )
}

.scan_capture <- function(match, name) {
  idx <- fastmatch::fmatch(name, match$name)
  if (is.na(idx)) {
    return(NULL)
  }

  match$node[[idx]]
}

.scan_matches <- function(root, query, capture_name) {
  matches <- unlist(
    treesitter::query_matches(query, root),
    recursive = FALSE,
    use.names = FALSE
  )
  if (length(matches) <= 1L) {
    return(matches)
  }

  matches[order(
    vapply(
      matches,
      \(match) treesitter::node_start_byte(.scan_capture(match, capture_name)),
      numeric(1)
    ),
    seq_along(matches)
  )]
}

.scan_name <- function(node) {
  if (is.null(node)) {
    return(NULL)
  }

  if (treesitter::node_type(node) == "identifier") {
    text <- treesitter::node_text(node)
    n <- nchar(text, type = "chars")

    if (
      n >= 2L &&
        startsWith(text, "`") &&
        substr(text, n, n) == "`"
    ) {
      return(substr(text, 2L, n - 1L))
    }

    return(text)
  }
  if (treesitter::node_type(node) == "string") {
    content <- treesitter::node_child_by_field_name(node, "content")
    return(if (is.null(content)) "" else treesitter::node_text(content))
  }

  NULL
}

.scan_resolver_index <- function(export_index, origin_map) {
  if (
    identical(export_index, .stan_export_index) &&
      identical(origin_map, .stan_origin_map)
  ) {
    return(.stan_resolver_index)
  }

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
  candidates,
  attached,
  allowed_packages,
  resolver_index
) {
  empty <- list(pkgs = character(), keys = character(), ambiguous = character())
  if (!length(candidates$fun) || !length(allowed_packages)) {
    return(empty)
  }

  if (!length(attached$pkg) || !any(attached$is_attach)) {
    return(empty)
  }
  attached <- .scan_subset_records(
    list(
      visit_idx = attached$visit_idx,
      pkg = attached$pkg
    ),
    attached$is_attach
  )
  attached_rows <- split(seq_along(attached$pkg), attached$pkg)

  resolved_pkgs <- rep.int("", length(candidates$fun))
  considered <- logical(length(candidates$fun))
  call_groups <- split(seq_along(candidates$fun), candidates$fun)
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
      visit_idx = candidates$visit_idx[idx],
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
      paste0(resolved_pkgs[resolved], "::", candidates$fun[resolved])
    } else {
      character()
    },
    ambiguous = if (all(!considered | resolved)) {
      character()
    } else {
      sort(unique(candidates$fun[considered & !resolved]))
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
