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
#'     "requireNamespace(\"brms\")",
#'     "use(\"cmdstanr\", c(\"cmdstan_model\", \"write_stan_json\"))",
#'     "draws <- as_draws(list(mu = rnorm(10)))",
#'     "posterior::rhat(draws)",
#'     "brms::mixture(0.4)",
#'     "cmdstanr::write_stan_json(list(N = 3), \"data.json\")"
#'   ),
#'   path
#' )
#' scan_usage(
#'   path,
#'   allowed_packages = c("posterior", "brms", "cmdstanr"),
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
    cli::cli_abort(c(
      "No files found.",
      "i" = "Check the {.arg path} and {.arg skip_dirs} arguments."
    ))
  }

  hits <- unique(files) |>
    lapply(
      \(file) {
        file |>
          .extract_code() |>
          .scan_tokens(
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
  files <- character()
  n <- 0L

  add_code_files <- function(paths) {
    code_files <- paths[grepl("\\.(R|Rmd|Qmd)$", paths, ignore.case = TRUE)]
    if (!length(code_files)) {
      return(invisible(NULL))
    }

    idx <- seq.int(n + 1L, n + length(code_files))
    files[idx] <<- code_files
    n <<- idx[length(idx)]

    invisible(NULL)
  }

  .scan_dir_walk(dir_path, skip_dirs, add_code_files)
  normalizePath(files, winslash = "/", mustWork = FALSE)
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
  export_index,
  origin_map,
  resolver_index = NULL,
  metapackages = NULL,
  file_path = NULL
) {
  empty <- list(pkgs = character(), keys = character(), ambiguous = character())
  if (!nzchar(code)) {
    return(empty)
  }
  if (is.null(resolver_index)) {
    resolver_index <- .scan_resolver_index(export_index, origin_map)
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

  export_names <- names(resolver_index)
  if (is.null(export_names)) {
    export_names <- character()
  }

  lib_pkgs <- character()
  lib_visit_idx <- integer()
  lib_is_attach <- logical()
  ns_pkgs <- character()
  ns_keys <- character()
  unqual_funs <- character()
  unqual_idx <- integer()

  for (match in .scan_matches(root, scan_state$attach_calls, "call")) {
    visit_idx <- as.integer(
      treesitter::node_start_byte(.scan_capture(match, "call"))
    )
    head <- .scan_name(.scan_capture(match, "head"))
    pkg <- .scan_name(.scan_capture(match, "pkg"))
    attached <- .scan_attached_pkgs(
      pkg = pkg,
      is_attach = head != "requireNamespace",
      allowed_packages = allowed_packages,
      metapackages = metapackages
    )
    if (!is.null(attached)) {
      lib_pkgs <- c(lib_pkgs, attached$pkg)
      lib_visit_idx <- c(
        lib_visit_idx,
        rep.int(visit_idx, length(attached$pkg))
      )
      lib_is_attach <- c(lib_is_attach, attached$is_attach)
    }
  }

  for (match in .scan_matches(root, scan_state$use_calls, "call")) {
    pkg <- .scan_name(.scan_capture(match, "pkg"))
    fun <- .scan_name(.scan_capture(match, "fun"))

    if (
      !is.null(pkg) &&
        !is.na(fastmatch::fmatch(pkg, allowed_packages))
    ) {
      ns_pkgs <- c(ns_pkgs, pkg)
      if (!is.null(fun) && nzchar(fun)) {
        ns_keys <- c(ns_keys, paste0(pkg, "::", fun))
      }
    }
  }

  for (match in .scan_matches(root, scan_state$namespace_uses, "ns")) {
    pkg <- .scan_name(.scan_capture(match, "pkg"))
    fun <- .scan_name(.scan_capture(match, "fun"))

    if (!is.na(fastmatch::fmatch(pkg, allowed_packages))) {
      ns_pkgs <- c(ns_pkgs, pkg)
      ns_keys <- c(ns_keys, paste0(pkg, "::", fun))
    }
  }

  for (match in .scan_matches(root, scan_state$plain_calls, "call")) {
    call <- .scan_capture(match, "call")
    head <- .scan_name(.scan_capture(match, "head"))

    if (
      is.na(fastmatch::fmatch(head, export_names)) ||
        !is.na(fastmatch::fmatch(head, ignore_unqualified_functions))
    ) {
      next
    }

    unqual_funs <- c(unqual_funs, head)
    unqual_idx <- c(unqual_idx, as.integer(treesitter::node_start_byte(call)))
  }

  for (match in .scan_matches(root, scan_state$member_calls, "call")) {
    call <- .scan_capture(match, "call")
    fun <- .scan_name(.scan_capture(match, "member"))

    if (!is.na(fastmatch::fmatch(fun, export_names))) {
      unqual_funs <- c(unqual_funs, fun)
      unqual_idx <- c(unqual_idx, as.integer(treesitter::node_start_byte(call)))
    }
  }

  lib_data <- if (length(lib_pkgs)) {
    data.frame(
      visit_idx = lib_visit_idx,
      pkg = lib_pkgs,
      is_attach = lib_is_attach,
      stringsAsFactors = FALSE
    )
  } else {
    NULL
  }

  resolved <- .resolve_candidates(
    unqual = list(funs = unqual_funs, idx = unqual_idx),
    lib_data,
    allowed_packages,
    resolver_index = resolver_index
  )

  list(
    pkgs = c(
      if (is.null(lib_data)) character() else lib_data$pkg,
      ns_pkgs,
      resolved$pkgs
    ),
    keys = c(ns_keys, resolved$keys),
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
  bundle <- c(
    list(parser = treesitter::parser(language)),
    lapply(.scan_query_sources, \(source) treesitter::query(language, source))
  )
  .scan_treesitter_cache$bundle <- bundle
  bundle
}

.scan_attached_pkgs <- function(
  pkg,
  is_attach,
  allowed_packages,
  metapackages = NULL
) {
  pkgs <- pkg
  attach_flags <- is_attach

  expanded <- if (is_attach && !is.null(metapackages)) {
    metapackages[[pkg]]
  } else {
    NULL
  }
  if (length(expanded)) {
    pkgs <- c(pkgs, expanded)
    attach_flags <- c(attach_flags, rep.int(TRUE, length(expanded)))
  }

  keep <- !is.na(fastmatch::fmatch(pkgs, allowed_packages))
  if (!any(keep)) {
    return(NULL)
  }

  list(
    pkg = pkgs[keep],
    is_attach = attach_flags[keep]
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
  if (!length(matches)) {
    return(list())
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

.origin_pkg <- function(pkg, fun, origin_map) {
  origin <- unname(origin_map[paste0(pkg, "::", fun)])
  if (is.na(origin)) pkg else origin
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

  setNames(
    lapply(
      funs,
      \(fun) {
        providers <- export_index[[fun]]
        if (is.null(providers) || !length(providers)) {
          return(NULL)
        }

        data.frame(
          provider = providers,
          origin = vapply(
            providers,
            .origin_pkg,
            character(1),
            fun = fun,
            origin_map = origin_map
          ),
          stringsAsFactors = FALSE
        )
      }
    ),
    funs
  )
}

.scan_attached_libs <- function(lib_data) {
  if (is.null(lib_data) || !any(lib_data$is_attach)) {
    return(NULL)
  }

  attached <- lib_data[lib_data$is_attach, c("visit_idx", "pkg"), drop = FALSE]
  if (nrow(attached) <= 1L) {
    return(attached)
  }

  attached[order(attached$visit_idx, seq_len(nrow(attached))), , drop = FALSE]
}

.resolve_meta <- function(
  fun,
  attached,
  allowed_packages,
  resolver_index
) {
  meta <- resolver_index[[fun]]
  if (is.null(meta)) {
    return(NULL)
  }

  keep <- !is.na(fastmatch::fmatch(meta$provider, allowed_packages)) &
    !is.na(fastmatch::fmatch(meta$provider, attached$pkg))
  if (!any(keep)) {
    return(NULL)
  }

  meta <- meta[keep, , drop = FALSE]
  allowed_origins <- unique(
    meta$origin[!is.na(fastmatch::fmatch(meta$origin, allowed_packages))]
  )
  if (!length(allowed_origins)) {
    return(NULL)
  }

  list(meta = meta, allowed_origins = allowed_origins)
}

.resolve_call <- function(
  meta,
  visit_idx,
  attached,
  allowed_packages
) {
  if (length(meta$allowed_origins) == 1L) {
    return(meta$allowed_origins[[1L]])
  }

  k <- findInterval(visit_idx, attached$visit_idx)
  if (k == 0L) {
    return("")
  }

  matches <- fastmatch::fmatch(meta$meta$provider, attached$pkg[seq_len(k)])
  if (all(is.na(matches))) {
    return("")
  }

  resolved <- meta$meta[
    which.max(ifelse(is.na(matches), -1L, matches)),
    ,
    drop = FALSE
  ]
  if (is.na(fastmatch::fmatch(resolved$origin, allowed_packages))) {
    resolved$provider
  } else {
    resolved$origin
  }
}

.resolve_candidates <- function(
  unqual,
  lib_data,
  allowed_packages,
  export_index,
  origin_map,
  resolver_index = NULL
) {
  empty <- list(pkgs = character(), keys = character(), ambiguous = character())
  if (!length(unqual$funs) || !length(allowed_packages)) {
    return(empty)
  }
  if (is.null(resolver_index)) {
    resolver_index <- .scan_resolver_index(export_index, origin_map)
  }

  attached <- .scan_attached_libs(lib_data)
  if (is.null(attached)) {
    return(empty)
  }

  calls <- data.frame(
    fun = unqual$funs,
    visit_idx = unqual$idx,
    stringsAsFactors = FALSE
  )
  if (nrow(calls) > 1L) {
    calls <- calls[order(calls$visit_idx, seq_len(nrow(calls))), , drop = FALSE]
  }

  meta_by_fun <- setNames(
    lapply(
      unique(calls$fun),
      \(fun) {
        .resolve_meta(
          fun = fun,
          attached = attached,
          allowed_packages = allowed_packages,
          resolver_index = resolver_index
        )
      }
    ),
    unique(calls$fun)
  )
  has_meta <- !vapply(meta_by_fun[calls$fun], is.null, logical(1))
  if (!any(has_meta)) {
    return(empty)
  }

  calls <- calls[has_meta, , drop = FALSE]
  resolved_pkgs <- vapply(
    seq_len(nrow(calls)),
    \(i) {
      .resolve_call(
        meta = meta_by_fun[[calls$fun[[i]]]],
        visit_idx = calls$visit_idx[[i]],
        attached = attached,
        allowed_packages = allowed_packages
      )
    },
    character(1)
  )
  resolved <- nzchar(resolved_pkgs)

  list(
    pkgs = resolved_pkgs[resolved],
    keys = if (any(resolved)) {
      paste0(resolved_pkgs[resolved], "::", calls$fun[resolved])
    } else {
      character()
    },
    ambiguous = if (all(resolved)) {
      character()
    } else {
      sort(unique(calls$fun[!resolved]))
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
