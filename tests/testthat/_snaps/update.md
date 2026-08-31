# stanflow_update reports when nothing is behind

    All stanflow packages up-to-date!

# stanflow_update lists behind packages

    The following packages are out of date:
    
    * cmdstanr  (1.1.0 -> 1.2.0)
    * posterior (1.5.0 -> 1.6.0)
    
    Start a clean R session then run:
    install.packages(c("cmdstanr", "posterior"), repos = c("https://community.r-multiverse.org", getOption("repos")))

# stanflow_update dry_run reports package installs without installing

    Code
      with_mocked_bindings(stanflow_deps = function(recursive, dev, check_updates) {
        observed <<- check_updates
        behind
      }, is_interactive_session = function() FALSE, with_mocked_bindings(menu = function(
        ...) stop("should not prompt"), install.packages = function(...) {
        installed <<- TRUE
        invisible(NULL)
      }, stanflow_update(dry_run = TRUE), .package = "utils"), .package = "stanflow")
    Output
      The following packages are out of date:
      
      * cmdstanr  (1.1.0 -> 1.2.0)
      * posterior (1.5.0 -> 1.6.0)
      
    Message
      i Would install cmdstanr, posterior: utils::install.packages(c("cmdstanr", "posterior"), repos = stanflow::stan_repos(FALSE), quiet = TRUE).

# stanflow_update surfaces transitive dependencies (loo -> matrixStats)

    The following packages are out of date:
    
    * cmdstanr    (1.1.0 -> 1.2.0)
    * posterior   (1.5.0 -> 1.6.0)
    * matrixStats (1.2.8 -> 1.3.0)
    
    Start a clean R session then run:
    install.packages(c("cmdstanr", "posterior", "matrixStats"), repos = c("https://community.r-multiverse.org", getOption("repos")))

# stanflow_update reports packages that need reinstall after warnings

    The following packages are out of date:
    
    * cmdstanr  (1.1.0 -> 1.2.0)
    * posterior (1.5.0 -> 1.6.0)
    
    Start a clean R session then run:
    install.packages(c("cmdstanr", "posterior"), repos = c("https://community.r-multiverse.org", getOption("repos")))

# stanflow_update uses Stan universe when dev = TRUE

    The following packages are out of date:
    
    * cmdstanr  (1.1.0 -> 1.2.0)
    * posterior (1.5.0 -> 1.6.0)
    
    Start a clean R session then run:
    install.packages(c("cmdstanr", "posterior"), repos = c("https://stan-dev.r-universe.dev", getOption("repos")))

