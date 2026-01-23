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

.stan_citation_pkgs <- new.env(parent = emptyenv())
.stan_citation_funs <- new.env(parent = .stan_citation_pkgs)

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

source("data-raw/papers.R")
source("data-raw/bayesplot-citations.R")
source("data-raw/cmdstanr-citations.R")
source("data-raw/loo-citations.R")

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

# Extraction: Get exported functions for each package
.stan_exports <- lapply(.stan_pkgs, function(pkg) {
  getNamespaceExports(pkg) |>
    Filter(
      \(x) {
        is.function(tryCatch(getExportedValue(pkg, x), error = function(e) {
          NULL
        }))
      },
      x = _
    )
}) |>
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

.date_generated <- Sys.Date()

save(
  .stan_exports,
  .stan_export_index,
  .stan_origin_map,
  .stan_citation_pkgs,
  .stan_citation_funs,
  .stan_pkgs,
  .stdlib_funs,
  .stan_pkg_versions,
  .scan_skip_dirs,
  .date_generated,
  file = "R/sysdata.rda",
  compress = "xz"
)

message("Saved sysdata.rda")
