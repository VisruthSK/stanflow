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
  "Make sure you use `stanflow_update()` to make sure your packages are up to date."
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

.date_generated <- Sys.Date()

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

save(
  .stan_exports,
  .stan_export_index,
  .stan_origin_map,
  .stan_pkgs,
  .stdlib_funs,
  .stan_pkg_versions,
  .scan_skip_dirs,
  .date_generated,
  file = "R/sysdata.rda",
  compress = "xz"
)

message(
  "Export list, index, origin map, and stdlib functions saved to R/sysdata.rda"
)
