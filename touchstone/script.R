library(touchstone)

# Install both branches to benchmark
branch_install()

# Clone pinned repositories for benchmarking
pin_repos <- function() {
  repos <- list(
    list(
      url = "https://github.com/paul-buerkner/brms.git",
      ref = "v2.22.0",
      dir = "brms"
    ),
    list(
      url = "https://github.com/stan-dev/bayesplot.git",
      ref = "v1.15.0",
      dir = "bayesplot"
    ),
    list(
      url = "https://github.com/stan-dev/rstan.git",
      ref = "v2.9.0-3",
      dir = "rstan"
    ),
    list(
      url = "https://github.com/tidyverse/ggplot2.git",
      ref = "v4.0.2",
      dir = "ggplot2"
    )
  )
  
  base_dir <- "touchstone/sources"
  dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)
  
  for (repo in repos) {
    repo_path <- file.path(base_dir, repo$dir)
    if (!dir.exists(repo_path)) {
      message("Cloning ", repo$dir, " at ", repo$ref)
      system2("git", c("clone", "--depth", "1", "--branch", repo$ref, repo$url, repo_path))
    }
  }
  
  invisible(base_dir)
}

# Setup: Clone the pinned repositories
pin_repos()

# Benchmark stan_cite() on the pinned repositories
benchmark_run(
  expr_before_benchmark = {
    library(stanflow)
  },
  cite_brms = stan_cite(path = "touchstone/sources/brms", strict = FALSE, quiet = TRUE),
  n = 10
)

benchmark_run(
  expr_before_benchmark = {
    library(stanflow)
  },
  cite_bayesplot = stan_cite(path = "touchstone/sources/bayesplot", strict = FALSE, quiet = TRUE),
  n = 10
)

benchmark_run(
  expr_before_benchmark = {
    library(stanflow)
  },
  cite_rstan = stan_cite(path = "touchstone/sources/rstan", strict = FALSE, quiet = TRUE),
  n = 10
)

benchmark_run(
  expr_before_benchmark = {
    library(stanflow)
  },
  cite_ggplot2 = stan_cite(path = "touchstone/sources/ggplot2", strict = FALSE, quiet = TRUE),
  n = 10
)

# Analyze and report the results
benchmark_analyze()
