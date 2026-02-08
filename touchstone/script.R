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

# Install both branches to benchmark
branch_install()

# Clone pinned repositories for benchmarking
base_dir <- file.path("touchstone", "sources")
repos <- list(
  list(
    url = "https://github.com/paul-buerkner/brms.git",
    ref = "v2.22.0"
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

benchmark_n <- 10
for (repo in repos) {
  dir <- sub("\\.git$", "", basename(repo$url))
  repo_path <- file.path(base_dir, dir)
  clone_repo(repo$url, repo$ref, dir)
  benchmark_run(
    expr_before_benchmark = {
      library(stanflow)
    },
    n = benchmark_n,
    !!dir := stan_cite(
      path = !!repo_path,
      strict = FALSE,
      quiet = TRUE
    )
  )
}

# touchstone analysis -----------------------------------------------------

# Ensure plot/comment dependencies are available for benchmark_analyze().
plot_pkgs <- c("ggplot2", "dplyr", "glue")
missing_plot_pkgs <- plot_pkgs[
  !vapply(plot_pkgs, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_plot_pkgs) > 0) {
  stop(
    "touchstone: missing packages required for plots/comments: ",
    paste(missing_plot_pkgs, collapse = ", "),
    call. = FALSE
  )
}

# Analyze and report the results
benchmark_analyze()
