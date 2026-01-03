# setup_interface warns when prefer_cmdstanr adds cmdstanr

    

# setup_interface dry_run logs install message when force = TRUE

    Planned commands:
    utils::install.packages("cmdstanr", repos = c("https://community.r-multiverse.org", getOption("repos")), quiet = TRUE)
    library("cmdstanr", lib.loc = if ("cmdstanr" %in% loadedNamespaces()) dirname(getNamespaceInfo("cmdstanr", "path")) else NULL, character.only = TRUE, warn.conflicts = FALSE)

# setup_interface dry_run logs install and attach steps when verbose

    Planned commands:
    cli::cli_abort("Package \"cmdstanr\" is missing.")
    library("cmdstanr", lib.loc = if ("cmdstanr" %in% loadedNamespaces()) dirname(getNamespaceInfo("cmdstanr", "path")) else NULL, character.only = TRUE, warn.conflicts = FALSE)

# setup_rstan reports message when quiet = FALSE

    

# setup_brms reports message when quiet = FALSE

    

# setup_rstanarm reports message when quiet = FALSE

    

