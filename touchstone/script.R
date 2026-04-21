library(touchstone)

# Install both branches to benchmark
branch_install()

# Clone pinned repositories for benchmarking
base_dir <- file.path("touchstone", "sources")
dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)

repos <- list(
  list(
    url = "https://github.com/tidyverse/ggplot2.git",
    ref = "v4.0.2"
  ),
  list(
    url = "https://github.com/ASKurz/Statistical_Rethinking_with_brms_ggplot2_and_the_tidyverse.git",
    ref = "1.4.0"
  ),
  list(
    url = "https://github.com/stan-dev/loo.git",
    ref = "v2.9.0"
  )
)

clone_repo <- function(url, ref, dir) {
  repo_path <- file.path(base_dir, dir)
  if (!dir.exists(repo_path)) {
    message("Cloning ", dir, " at ", ref)
    system2("git", c("clone", "--depth", "1", "--branch", ref, url, repo_path))
  }
}

for (repo in repos) {
  dir <- sub("\\.git$", "", basename(repo$url))
  repo_path <- file.path(base_dir, dir)
  clone_repo(repo$url, repo$ref, dir)
  benchmark_run(
    expr_before_benchmark = {
      library(stanflow)
    },
    n = 5,
    !!dir := stan_cite(
      path = !!repo_path,
      strict = FALSE,
      quiet = TRUE
    )
  )
}

# Analyze and report the results
benchmark_analyze()
