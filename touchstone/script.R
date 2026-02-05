library(touchstone)

# In CI, ignore Additional_repositories from DESCRIPTION
if (Sys.getenv("GITHUB_ACTIONS") == "true") {
  ns <- asNamespace("remotes")
  if (exists("load_pkg_description", envir = ns, inherits = FALSE)) {
    locked <- bindingIsLocked("load_pkg_description", ns)
    if (locked) {
      unlockBinding("load_pkg_description", ns)
    }
    orig <- get("load_pkg_description", envir = ns)
    assign(
      "load_pkg_description",
      function(path) {
        desc <- orig(path)
        desc$additional_repositories <- NULL
        desc
      },
      envir = ns
    )
    if (locked) {
      lockBinding("load_pkg_description", ns)
    }
  }
}

# Ensure R-multiverse is available for dependency resolution.
options(
  repos = c(
    Multiverse = "https://production.r-multiverse.org/2025-12-15",
    CRAN = "https://packagemanager.posit.co/cran/__linux__/noble/2025-10-15"
  )
)

# Install both branches to benchmark
branch_install()

# Benchmark settings
benchmark_n <- 10
run_cite <- function(name, path) {
  do.call(
    benchmark_run,
    c(
      list(
        expr_before_benchmark = {
          library(stanflow)
        },
        n = benchmark_n
      ),
      setNames(
        list(
          stan_cite(
            path = path,
            strict = FALSE,
            quiet = TRUE
          )
        ),
        name
      )
    )
  )
}

repo_dir <- \(url) sub("\\.git$", "", basename(url))

# Clone pinned repositories for benchmarking
base_dir <- file.path("touchstone", "sources")
repos <- list(
  list(
    url = "https://github.com/paul-buerkner/brms.git",
    ref = "v2.22.0"
  ),
  list(
    url = "https://github.com/stan-dev/bayesplot.git",
    ref = "v1.15.0"
  ),
  list(
    url = "https://github.com/stan-dev/rstan.git",
    ref = "v2.32.2"
  ),
  list(
    url = "https://github.com/tidyverse/ggplot2.git",
    ref = "v4.0.2"
  ),
  list(
    url = "https://github.com/stan-dev/projpred.git",
    ref = "v2.10.0"
  ),
  list(
    url = "https://github.com/stan-dev/loo.git",
    ref = "v2.9.0"
  ),
  list(
    url = "https://github.com/stan-dev/posterior.git",
    ref = "v1.6.1"
  )
)

dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)

clone_repo <- function(url, ref, dir) {
  repo_path <- file.path(base_dir, dir)
  if (!dir.exists(repo_path)) {
    message("Cloning ", dir, " at ", ref)
    system2("git", c("clone", "--depth", "1", "--branch", ref, url, repo_path))
  }
}

for (repo in repos) {
  dir <- repo_dir(repo$url)
  clone_repo(repo$url, repo$ref, dir)
  run_cite(paste0("cite_", dir), file.path(base_dir, dir))
}

# Analyze and report the results
benchmark_analyze()
