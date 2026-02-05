library(touchstone)

# Ensure R-multiverse is available for dependency resolution.
options(
  repos = c(
    Multiverse = "https://production.r-multiverse.org/2025-12-15",
    CRAN = "https://packagemanager.posit.co/cran/__linux__/noble/2025-10-15"
  )
)

# Install both branches to benchmark
branch_install()

# Clone pinned repositories for benchmarking
repos <- list(
  list(
    url = "https://github.com/paul-buerkner/brms.git",
    ref = "v2.22.0",
    dir = "brms",
    use_commit = FALSE
  ),
  list(
    url = "https://github.com/stan-dev/bayesplot.git",
    ref = "v1.15.0",
    dir = "bayesplot",
    use_commit = FALSE
  ),
  list(
    url = "https://github.com/stan-dev/rstan.git",
    ref = "0e3dd7a97dd8de2d2b923ed43cb9e3a989d4a612",
    dir = "rstan",
    use_commit = TRUE
  ),
  list(
    url = "https://github.com/tidyverse/ggplot2.git",
    ref = "v4.0.2",
    dir = "ggplot2",
    use_commit = FALSE
  ),
  list(
    url = "https://github.com/stan-dev/projpred.git",
    ref = "v2.10.0",
    dir = "projpred",
    use_commit = FALSE
  ),
  list(
    url = "https://github.com/stan-dev/loo.git",
    ref = "v2.9.0",
    dir = "loo",
    use_commit = FALSE
  ),
  list(
    url = "https://github.com/stan-dev/posterior.git",
    ref = "v1.6.1",
    dir = "posterior",
    use_commit = FALSE
  )
)

base_dir <- "touchstone/sources"
dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)

for (repo in repos) {
  repo_path <- file.path(base_dir, repo$dir)
  if (!dir.exists(repo_path)) {
    message("Cloning ", repo$dir, " at ", repo$ref)
    if (repo$use_commit) {
      # For commits, clone and checkout
      system2("git", c("clone", "--depth", "50", repo$url, repo_path))
      system2("git", c("-C", repo_path, "checkout", repo$ref))
    } else {
      # For tags/branches, use --branch
      system2(
        "git",
        c("clone", "--depth", "1", "--branch", repo$ref, repo$url, repo_path)
      )
    }
  }
}

# Benchmark stan_cite() on the pinned repositories
benchmark_run(
  expr_before_benchmark = {
    library(stanflow)
  },
  cite_brms = stan_cite(
    path = "touchstone/sources/brms",
    strict = FALSE,
    quiet = TRUE
  ),
  n = 10
)

benchmark_run(
  expr_before_benchmark = {
    library(stanflow)
  },
  cite_bayesplot = stan_cite(
    path = "touchstone/sources/bayesplot",
    strict = FALSE,
    quiet = TRUE
  ),
  n = 10
)

benchmark_run(
  expr_before_benchmark = {
    library(stanflow)
  },
  cite_rstan = stan_cite(
    path = "touchstone/sources/rstan",
    strict = FALSE,
    quiet = TRUE
  ),
  n = 10
)

benchmark_run(
  expr_before_benchmark = {
    library(stanflow)
  },
  cite_ggplot2 = stan_cite(
    path = "touchstone/sources/ggplot2",
    strict = FALSE,
    quiet = TRUE
  ),
  n = 10
)

benchmark_run(
  expr_before_benchmark = {
    library(stanflow)
  },
  cite_projpred = stan_cite(
    path = "touchstone/sources/projpred",
    strict = FALSE,
    quiet = TRUE
  ),
  n = 10
)

benchmark_run(
  expr_before_benchmark = {
    library(stanflow)
  },
  cite_loo = stan_cite(
    path = "touchstone/sources/loo",
    strict = FALSE,
    quiet = TRUE
  ),
  n = 10
)

benchmark_run(
  expr_before_benchmark = {
    library(stanflow)
  },
  cite_posterior = stan_cite(
    path = "touchstone/sources/posterior",
    strict = FALSE,
    quiet = TRUE
  ),
  n = 10
)

# Analyze and report the results
benchmark_analyze()
