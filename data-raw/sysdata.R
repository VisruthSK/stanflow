.stan_pkgs <- c(
  "bayesplot",
  "brms",
  "cmdstanr",
  "loo",
  "posterior",
  "projpred",
  "rstan",
  "rstanarm",
  "rstantools",
  "shinystan",
  "stanflow"
)

message(
  "Make sure you use `stanflow_update()` to make sure your packages are up to date before generating the sysdata file."
)

missing <- .stan_pkgs[
  !vapply(.stan_pkgs, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing) > 0) {
  stop(
    sprintf(
      "Missing required package%s: %s",
      if (length(missing) == 1) "" else "s",
      paste(missing, collapse = ", ")
    ),
    call. = FALSE
  )
}

.stan_citation_pkgs <- new.env(parent = emptyenv())
.stan_citation_funs <- new.env(parent = emptyenv())
.stan_citation_pkg_extras <- new.env(parent = emptyenv())
.stan_citation_pkgs$brms <- c(
  bibentry(
    bibtype = "Article",
    title = "{brms}: An {R} Package for {Bayesian} Multilevel Models Using {Stan}",
    author = person(given = "Paul-Christian", family = "B\\u00fcrkner"),
    journal = "Journal of Statistical Software",
    year = "2017",
    volume = "80",
    number = "1",
    pages = "1--28",
    doi = "10.18637/jss.v080.i01",
    textVersion = paste(
      "Paul-Christian B\\u00fcrkner (2017).",
      "brms: An R Package for Bayesian Multilevel Models Using Stan.",
      "Journal of Statistical Software, 80(1), 1-28.",
      "doi:10.18637/jss.v080.i01"
    ),
    encoding = "UTF-8"
  ),
  bibentry(
    bibtype = "Article",
    title = "Advanced {Bayesian} Multilevel Modeling with the {R} Package {brms}",
    author = person(given = "Paul-Christian", family = "B\\u00fcrkner"),
    journal = "The R Journal",
    year = "2018",
    volume = "10",
    number = "1",
    pages = "395--411",
    doi = "10.32614/RJ-2018-017",
    textVersion = paste(
      "Paul-Christian B\\u00fcrkner (2018).",
      "Advanced Bayesian Multilevel Modeling with the R Package brms.",
      "The R Journal, 10(1), 395-411.",
      "doi:10.32614/RJ-2018-017"
    ),
    encoding = "UTF-8"
  ),
  bibentry(
    bibtype = "Article",
    title = "Bayesian Item Response Modeling in {R} with {brms} and {Stan}",
    author = person(given = "Paul-Christian", family = "B\\u00fcrkner"),
    journal = "Journal of Statistical Software",
    year = "2021",
    volume = "100",
    number = "5",
    pages = "1--54",
    doi = "10.18637/jss.v100.i05",
    textVersion = paste(
      "Paul-Christian B\\u00fcrkner (2021).",
      "Bayesian Item Response Modeling in R with brms and Stan.",
      "Journal of Statistical Software, 100(5), 1-54.",
      "doi:10.18637/jss.v100.i05"
    ),
    encoding = "UTF-8"
  )
)

# Record versions and generation time (UTC)
.stan_pkg_versions <- .stan_pkgs |>
  lapply(\(pkg) packageVersion(pkg) |> as.character()) |>
  setNames(.stan_pkgs)

# Precompute standard library functions
.stdlib_funs <- lapply(
  c("base", "stats", "utils", "graphics", "grDevices", "methods"),
  getNamespaceExports
) |>
  unlist(use.names = FALSE) |>
  unique() |>
  sort()

# Default skip directories
.scan_skip_dirs <- c(
  "renv",
  "packrat",
  "rv",
  ".Rcheck",
  "revdep",
  "_site",
  "_book",
  "_bookdown_files",
  "_freeze",
  ".quarto",
  ".quarto_cache",
  ".knitr_cache",
  "_cache",
  ".cache"
)

# Scanner query sources and helper names
.scan_special_heads <- c("library", "require", "requireNamespace", "use")
.scan_pkg_arg_names <- c("package", "pkg")

.scan_query_sources <- list(
  attach_calls = paste(
    "(call",
    "  function: (identifier) @head",
    "  arguments: (arguments) @args",
    "  (#eq? @head \"library\")",
    ") @call",
    "",
    "(call",
    "  function: (identifier) @head",
    "  arguments: (arguments) @args",
    "  (#eq? @head \"require\")",
    ") @call",
    "",
    "(call",
    "  function: (identifier) @head",
    "  arguments: (arguments) @args",
    "  (#eq? @head \"requireNamespace\")",
    ") @call",
    sep = "\n"
  ),
  use_calls = paste(
    "(call",
    "  function: (identifier) @head",
    "  arguments: (arguments) @args",
    "  (#eq? @head \"use\")",
    ") @call",
    sep = "\n"
  ),
  plain_calls = paste(
    "(call",
    "  function: (identifier) @head",
    "  arguments: (arguments) @args",
    ") @call",
    sep = "\n"
  ),
  namespace_uses = paste(
    "(namespace_operator",
    "  lhs: (identifier) @pkg",
    "  rhs: (identifier) @fun",
    ") @ns",
    sep = "\n"
  ),
  member_calls = paste(
    "(call",
    "  function: (extract_operator",
    "    rhs: (identifier) @member",
    "  )",
    "  arguments: (arguments) @args",
    ") @call",
    "",
    "(call",
    "  function: (extract_operator",
    "    rhs: (string) @member",
    "  )",
    "  arguments: (arguments) @args",
    ") @call",
    "",
    "(call",
    "  function: (parenthesized_expression",
    "    body: (extract_operator",
    "      rhs: (identifier) @member",
    "    )",
    "  )",
    "  arguments: (arguments) @args",
    ") @call",
    "",
    "(call",
    "  function: (parenthesized_expression",
    "    body: (extract_operator",
    "      rhs: (string) @member",
    "    )",
    "  )",
    "  arguments: (arguments) @args",
    ") @call",
    sep = "\n"
  )
)

assign_citation <- function(pkg, funs, entries) {
  for (fun in funs) {
    .stan_citation_funs[[paste0(pkg, "::", fun)]] <- entries
  }
}

source("data-raw/papers.R")
.stan_citation_pkg_extras$bayesplot <- gabry2019_vis
.stan_citation_pkg_extras$posterior <- vehtari2021_rhat
source("data-raw/bayesplot-citations.R")
source("data-raw/brms-citations.R")
source("data-raw/cmdstanr-citations.R")
source("data-raw/loo-citations.R")
source("data-raw/posterior-citations.R")
source("data-raw/rstan-citations.R")
source("data-raw/rstanarm-citations.R")
source("data-raw/shinystan-citations.R")
source("data-raw/projpred-citations.R")
source("data-raw/rstantools-citations.R")

# Helper to determine origin
get_origin <- function(pkg, name) {
  obj <- tryCatch(getExportedValue(pkg, name), error = function(e) NULL)
  if (!is.function(obj)) {
    return(NA_character_)
  }

  env <- environment(obj)
  origin <- if (is.null(env)) "" else environmentName(env)
  if (!nzchar(origin)) {
    return(NA_character_)
  }
  sub("^namespace:", "", origin)
}

is_r6_generator <- function(x) {
  inherits(x, "R6ClassGenerator")
}

get_r6_generator <- function(pkg, class_name) {
  asNamespace(pkg) |>
    (\(ns) {
      if (!exists(class_name, envir = ns, inherits = FALSE)) {
        return(NULL)
      }
      get(class_name, envir = ns, inherits = FALSE)
    })() |>
    (\(obj) if (is_r6_generator(obj)) obj else NULL)()
}

collect_r6_methods <- function(pkg, export_names) {
  ns <- asNamespace(pkg)

  exported_r6 <- export_names |>
    Filter(
      \(name) {
        obj <- tryCatch(getExportedValue(pkg, name), error = function(e) NULL)
        is_r6_generator(obj)
      },
      x = _
    )

  namespace_r6 <- ls(ns, all.names = TRUE) |>
    Filter(
      \(name) {
        obj <- tryCatch(
          get(name, envir = ns, inherits = FALSE),
          error = function(e) NULL
        )
        is_r6_generator(obj)
      },
      x = _
    )

  unique(c(exported_r6, namespace_r6)) |>
    lapply(\(class_name) get_r6_generator(pkg, class_name)) |>
    Filter(\(gen) !is.null(gen), x = _) |>
    lapply(\(gen) names(gen$public_methods)) |>
    unlist(use.names = FALSE) |>
    (\(methods) methods[!is.na(methods) & nzchar(methods)])() |>
    unique()
}

collect_pkg_funs <- function(pkg) {
  export_names <- getNamespaceExports(pkg)

  exported_funs <- export_names |>
    Filter(
      \(x) {
        is.function(tryCatch(getExportedValue(pkg, x), error = function(e) {
          NULL
        }))
      },
      x = _
    )

  r6_methods <- collect_r6_methods(pkg, export_names)
  unique(c(exported_funs, r6_methods))
}

# Extraction: Get exported functions for each package
.stan_exports <- lapply(.stan_pkgs, collect_pkg_funs) |>
  setNames(.stan_pkgs)

# Indexing: Create inverted index (function -> packages)
all_funs <- unlist(.stan_exports, use.names = FALSE)
all_stan_pkgs <- rep(names(.stan_exports), lengths(.stan_exports))
.stan_export_index <- split(all_stan_pkgs, all_funs)

# Origin Resolution: Map pkg::fun -> origin_pkg
keys <- paste0(all_stan_pkgs, "::", all_funs)
.stan_origin_map <- mapply(
  get_origin,
  all_stan_pkgs,
  all_funs,
  USE.NAMES = FALSE
)

# If origin is undetermined (NA), assume it is the provider package
.stan_origin_map[is.na(.stan_origin_map)] <- all_stan_pkgs[is.na(
  .stan_origin_map
)]
names(.stan_origin_map) <- keys

save(
  .stan_exports,
  .stan_export_index,
  .stan_origin_map,
  .stan_citation_pkgs,
  .stan_citation_funs,
  .stan_citation_pkg_extras,
  .stan_pkgs,
  .stdlib_funs,
  .stan_pkg_versions,
  .scan_skip_dirs,
  .scan_pkg_arg_names,
  .scan_query_sources,
  .scan_special_heads,
  file = "R/sysdata.rda",
  compress = "xz"
)

message("Saved sysdata.rda")
