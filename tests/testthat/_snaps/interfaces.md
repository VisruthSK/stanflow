# setup_interface dry_run reports missing package setup without side effects

    Code
      setup_interface(interface = "brms", brms_backend = "rstan", force = FALSE,
        cores = 2, quiet = FALSE, dry_run = TRUE)
    Message
      ! Package brms is not installed.
      i Would install brms: utils::install.packages("brms", repos = stanflow::stan_repos(FALSE), quiet = FALSE).
      i Would configure brms: options(mc.cores = 2); options(brms.backend = "rstan").
      i Would attach brms: library("brms", lib.loc = if ("brms" %in% loadedNamespaces()) dirname(getNamespaceInfo("brms", "path")), character.only = TRUE, warn.conflicts = FALSE).

# setup_interface dry_run output is stable for brms

    Code
      setup_interface(interface = interface, brms_backend = "rstan", cores = 1,
        quiet = FALSE, dry_run = TRUE)
    Message
      i Would configure brms: options(mc.cores = 1); options(brms.backend = "rstan").
      i Would attach brms: library("brms", lib.loc = if ("brms" %in% loadedNamespaces()) dirname(getNamespaceInfo("brms", "path")), character.only = TRUE, warn.conflicts = FALSE).

# setup_interface dry_run output is stable for cmdstanr

    Code
      setup_interface(interface = interface, brms_backend = "rstan", cores = 1,
        quiet = FALSE, dry_run = TRUE)
    Message
      i Would check the CmdStan toolchain: cmdstanr::check_cmdstan_toolchain(quiet = FALSE).
      ! CmdStan binaries are missing or force-reinstall requested.
      i Would install or upgrade CmdStan: cmdstanr::install_cmdstan(quiet = FALSE, overwrite = TRUE, cores = 1).
      i Would configure cmdstanr: options(mc.cores = 1).
      i Would attach cmdstanr: library("cmdstanr", lib.loc = if ("cmdstanr" %in% loadedNamespaces()) dirname(getNamespaceInfo("cmdstanr", "path")), character.only = TRUE, warn.conflicts = FALSE).

# setup_interface dry_run output is stable for rstan

    Code
      setup_interface(interface = interface, brms_backend = "rstan", cores = 1,
        quiet = FALSE, dry_run = TRUE)
    Message
      i Would configure rstan: options(mc.cores = 1); rstan::rstan_options(auto_write = TRUE).
      i Would attach rstan: library("rstan", lib.loc = if ("rstan" %in% loadedNamespaces()) dirname(getNamespaceInfo("rstan", "path")), character.only = TRUE, warn.conflicts = FALSE).

# setup_interface dry_run output is stable for rstanarm

    Code
      setup_interface(interface = interface, brms_backend = "rstan", cores = 1,
        quiet = FALSE, dry_run = TRUE)
    Message
      i Would configure rstanarm: options(mc.cores = 1).
      i Would attach rstanarm: library("rstanarm", lib.loc = if ("rstanarm" %in% loadedNamespaces()) dirname(getNamespaceInfo("rstanarm", "path")), character.only = TRUE, warn.conflicts = FALSE).

# setup_interface dry_run output is stable for brms, cmdstanr

    Code
      setup_interface(interface = interface, brms_backend = "rstan", cores = 1,
        quiet = FALSE, dry_run = TRUE)
    Message
      i Would configure brms: options(mc.cores = 1); options(brms.backend = "rstan").
      i Would attach brms: library("brms", lib.loc = if ("brms" %in% loadedNamespaces()) dirname(getNamespaceInfo("brms", "path")), character.only = TRUE, warn.conflicts = FALSE).
      i Would check the CmdStan toolchain: cmdstanr::check_cmdstan_toolchain(quiet = FALSE).
      ! CmdStan binaries are missing or force-reinstall requested.
      i Would install or upgrade CmdStan: cmdstanr::install_cmdstan(quiet = FALSE, overwrite = TRUE, cores = 1).
      i Would configure cmdstanr: options(mc.cores = 1).
      i Would attach cmdstanr: library("cmdstanr", lib.loc = if ("cmdstanr" %in% loadedNamespaces()) dirname(getNamespaceInfo("cmdstanr", "path")), character.only = TRUE, warn.conflicts = FALSE).

# setup_interface dry_run output is stable for brms, rstan

    Code
      setup_interface(interface = interface, brms_backend = "rstan", cores = 1,
        quiet = FALSE, dry_run = TRUE)
    Message
      i Would configure brms: options(mc.cores = 1); options(brms.backend = "rstan").
      i Would attach brms: library("brms", lib.loc = if ("brms" %in% loadedNamespaces()) dirname(getNamespaceInfo("brms", "path")), character.only = TRUE, warn.conflicts = FALSE).
      i Would configure rstan: options(mc.cores = 1); rstan::rstan_options(auto_write = TRUE).
      i Would attach rstan: library("rstan", lib.loc = if ("rstan" %in% loadedNamespaces()) dirname(getNamespaceInfo("rstan", "path")), character.only = TRUE, warn.conflicts = FALSE).

# setup_interface dry_run output is stable for brms, rstanarm

    Code
      setup_interface(interface = interface, brms_backend = "rstan", cores = 1,
        quiet = FALSE, dry_run = TRUE)
    Message
      i Would configure brms: options(mc.cores = 1); options(brms.backend = "rstan").
      i Would attach brms: library("brms", lib.loc = if ("brms" %in% loadedNamespaces()) dirname(getNamespaceInfo("brms", "path")), character.only = TRUE, warn.conflicts = FALSE).
      i Would configure rstanarm: options(mc.cores = 1).
      i Would attach rstanarm: library("rstanarm", lib.loc = if ("rstanarm" %in% loadedNamespaces()) dirname(getNamespaceInfo("rstanarm", "path")), character.only = TRUE, warn.conflicts = FALSE).

# setup_interface dry_run output is stable for cmdstanr, rstan

    Code
      setup_interface(interface = interface, brms_backend = "rstan", cores = 1,
        quiet = FALSE, dry_run = TRUE)
    Message
      i Would check the CmdStan toolchain: cmdstanr::check_cmdstan_toolchain(quiet = FALSE).
      ! CmdStan binaries are missing or force-reinstall requested.
      i Would install or upgrade CmdStan: cmdstanr::install_cmdstan(quiet = FALSE, overwrite = TRUE, cores = 1).
      i Would configure cmdstanr: options(mc.cores = 1).
      i Would attach cmdstanr: library("cmdstanr", lib.loc = if ("cmdstanr" %in% loadedNamespaces()) dirname(getNamespaceInfo("cmdstanr", "path")), character.only = TRUE, warn.conflicts = FALSE).
      i Would configure rstan: options(mc.cores = 1); rstan::rstan_options(auto_write = TRUE).
      i Would attach rstan: library("rstan", lib.loc = if ("rstan" %in% loadedNamespaces()) dirname(getNamespaceInfo("rstan", "path")), character.only = TRUE, warn.conflicts = FALSE).

# setup_interface dry_run output is stable for cmdstanr, rstanarm

    Code
      setup_interface(interface = interface, brms_backend = "rstan", cores = 1,
        quiet = FALSE, dry_run = TRUE)
    Message
      i Would check the CmdStan toolchain: cmdstanr::check_cmdstan_toolchain(quiet = FALSE).
      ! CmdStan binaries are missing or force-reinstall requested.
      i Would install or upgrade CmdStan: cmdstanr::install_cmdstan(quiet = FALSE, overwrite = TRUE, cores = 1).
      i Would configure cmdstanr: options(mc.cores = 1).
      i Would attach cmdstanr: library("cmdstanr", lib.loc = if ("cmdstanr" %in% loadedNamespaces()) dirname(getNamespaceInfo("cmdstanr", "path")), character.only = TRUE, warn.conflicts = FALSE).
      i Would configure rstanarm: options(mc.cores = 1).
      i Would attach rstanarm: library("rstanarm", lib.loc = if ("rstanarm" %in% loadedNamespaces()) dirname(getNamespaceInfo("rstanarm", "path")), character.only = TRUE, warn.conflicts = FALSE).

# setup_interface dry_run output is stable for rstan, rstanarm

    Code
      setup_interface(interface = interface, brms_backend = "rstan", cores = 1,
        quiet = FALSE, dry_run = TRUE)
    Message
      i Would configure rstan: options(mc.cores = 1); rstan::rstan_options(auto_write = TRUE).
      i Would attach rstan: library("rstan", lib.loc = if ("rstan" %in% loadedNamespaces()) dirname(getNamespaceInfo("rstan", "path")), character.only = TRUE, warn.conflicts = FALSE).
      i Would configure rstanarm: options(mc.cores = 1).
      i Would attach rstanarm: library("rstanarm", lib.loc = if ("rstanarm" %in% loadedNamespaces()) dirname(getNamespaceInfo("rstanarm", "path")), character.only = TRUE, warn.conflicts = FALSE).

# setup_interface dry_run output is stable for brms, cmdstanr, rstan

    Code
      setup_interface(interface = interface, brms_backend = "rstan", cores = 1,
        quiet = FALSE, dry_run = TRUE)
    Message
      i Would configure brms: options(mc.cores = 1); options(brms.backend = "rstan").
      i Would attach brms: library("brms", lib.loc = if ("brms" %in% loadedNamespaces()) dirname(getNamespaceInfo("brms", "path")), character.only = TRUE, warn.conflicts = FALSE).
      i Would check the CmdStan toolchain: cmdstanr::check_cmdstan_toolchain(quiet = FALSE).
      ! CmdStan binaries are missing or force-reinstall requested.
      i Would install or upgrade CmdStan: cmdstanr::install_cmdstan(quiet = FALSE, overwrite = TRUE, cores = 1).
      i Would configure cmdstanr: options(mc.cores = 1).
      i Would attach cmdstanr: library("cmdstanr", lib.loc = if ("cmdstanr" %in% loadedNamespaces()) dirname(getNamespaceInfo("cmdstanr", "path")), character.only = TRUE, warn.conflicts = FALSE).
      i Would configure rstan: options(mc.cores = 1); rstan::rstan_options(auto_write = TRUE).
      i Would attach rstan: library("rstan", lib.loc = if ("rstan" %in% loadedNamespaces()) dirname(getNamespaceInfo("rstan", "path")), character.only = TRUE, warn.conflicts = FALSE).

# setup_interface dry_run output is stable for brms, cmdstanr, rstanarm

    Code
      setup_interface(interface = interface, brms_backend = "rstan", cores = 1,
        quiet = FALSE, dry_run = TRUE)
    Message
      i Would configure brms: options(mc.cores = 1); options(brms.backend = "rstan").
      i Would attach brms: library("brms", lib.loc = if ("brms" %in% loadedNamespaces()) dirname(getNamespaceInfo("brms", "path")), character.only = TRUE, warn.conflicts = FALSE).
      i Would check the CmdStan toolchain: cmdstanr::check_cmdstan_toolchain(quiet = FALSE).
      ! CmdStan binaries are missing or force-reinstall requested.
      i Would install or upgrade CmdStan: cmdstanr::install_cmdstan(quiet = FALSE, overwrite = TRUE, cores = 1).
      i Would configure cmdstanr: options(mc.cores = 1).
      i Would attach cmdstanr: library("cmdstanr", lib.loc = if ("cmdstanr" %in% loadedNamespaces()) dirname(getNamespaceInfo("cmdstanr", "path")), character.only = TRUE, warn.conflicts = FALSE).
      i Would configure rstanarm: options(mc.cores = 1).
      i Would attach rstanarm: library("rstanarm", lib.loc = if ("rstanarm" %in% loadedNamespaces()) dirname(getNamespaceInfo("rstanarm", "path")), character.only = TRUE, warn.conflicts = FALSE).

# setup_interface dry_run output is stable for brms, rstan, rstanarm

    Code
      setup_interface(interface = interface, brms_backend = "rstan", cores = 1,
        quiet = FALSE, dry_run = TRUE)
    Message
      i Would configure brms: options(mc.cores = 1); options(brms.backend = "rstan").
      i Would attach brms: library("brms", lib.loc = if ("brms" %in% loadedNamespaces()) dirname(getNamespaceInfo("brms", "path")), character.only = TRUE, warn.conflicts = FALSE).
      i Would configure rstan: options(mc.cores = 1); rstan::rstan_options(auto_write = TRUE).
      i Would attach rstan: library("rstan", lib.loc = if ("rstan" %in% loadedNamespaces()) dirname(getNamespaceInfo("rstan", "path")), character.only = TRUE, warn.conflicts = FALSE).
      i Would configure rstanarm: options(mc.cores = 1).
      i Would attach rstanarm: library("rstanarm", lib.loc = if ("rstanarm" %in% loadedNamespaces()) dirname(getNamespaceInfo("rstanarm", "path")), character.only = TRUE, warn.conflicts = FALSE).

# setup_interface dry_run output is stable for cmdstanr, rstan, rstanarm

    Code
      setup_interface(interface = interface, brms_backend = "rstan", cores = 1,
        quiet = FALSE, dry_run = TRUE)
    Message
      i Would check the CmdStan toolchain: cmdstanr::check_cmdstan_toolchain(quiet = FALSE).
      ! CmdStan binaries are missing or force-reinstall requested.
      i Would install or upgrade CmdStan: cmdstanr::install_cmdstan(quiet = FALSE, overwrite = TRUE, cores = 1).
      i Would configure cmdstanr: options(mc.cores = 1).
      i Would attach cmdstanr: library("cmdstanr", lib.loc = if ("cmdstanr" %in% loadedNamespaces()) dirname(getNamespaceInfo("cmdstanr", "path")), character.only = TRUE, warn.conflicts = FALSE).
      i Would configure rstan: options(mc.cores = 1); rstan::rstan_options(auto_write = TRUE).
      i Would attach rstan: library("rstan", lib.loc = if ("rstan" %in% loadedNamespaces()) dirname(getNamespaceInfo("rstan", "path")), character.only = TRUE, warn.conflicts = FALSE).
      i Would configure rstanarm: options(mc.cores = 1).
      i Would attach rstanarm: library("rstanarm", lib.loc = if ("rstanarm" %in% loadedNamespaces()) dirname(getNamespaceInfo("rstanarm", "path")), character.only = TRUE, warn.conflicts = FALSE).

# setup_interface dry_run output is stable for brms, cmdstanr, rstan, rstanarm

    Code
      setup_interface(interface = interface, brms_backend = "rstan", cores = 1,
        quiet = FALSE, dry_run = TRUE)
    Message
      i Would configure brms: options(mc.cores = 1); options(brms.backend = "rstan").
      i Would attach brms: library("brms", lib.loc = if ("brms" %in% loadedNamespaces()) dirname(getNamespaceInfo("brms", "path")), character.only = TRUE, warn.conflicts = FALSE).
      i Would check the CmdStan toolchain: cmdstanr::check_cmdstan_toolchain(quiet = FALSE).
      ! CmdStan binaries are missing or force-reinstall requested.
      i Would install or upgrade CmdStan: cmdstanr::install_cmdstan(quiet = FALSE, overwrite = TRUE, cores = 1).
      i Would configure cmdstanr: options(mc.cores = 1).
      i Would attach cmdstanr: library("cmdstanr", lib.loc = if ("cmdstanr" %in% loadedNamespaces()) dirname(getNamespaceInfo("cmdstanr", "path")), character.only = TRUE, warn.conflicts = FALSE).
      i Would configure rstan: options(mc.cores = 1); rstan::rstan_options(auto_write = TRUE).
      i Would attach rstan: library("rstan", lib.loc = if ("rstan" %in% loadedNamespaces()) dirname(getNamespaceInfo("rstan", "path")), character.only = TRUE, warn.conflicts = FALSE).
      i Would configure rstanarm: options(mc.cores = 1).
      i Would attach rstanarm: library("rstanarm", lib.loc = if ("rstanarm" %in% loadedNamespaces()) dirname(getNamespaceInfo("rstanarm", "path")), character.only = TRUE, warn.conflicts = FALSE).

# setup_interface dry_run emits output even when quiet = TRUE

    Code
      setup_interface(interface = "brms", brms_backend = "rstan", cores = 2, quiet = TRUE,
        dry_run = TRUE)
    Message
      i Would configure brms: options(mc.cores = 2); options(brms.backend = "rstan").
      i Would attach brms: library("brms", lib.loc = if ("brms" %in% loadedNamespaces()) dirname(getNamespaceInfo("brms", "path")), character.only = TRUE, warn.conflicts = FALSE).

# setup_brms emits configuration message when quiet = FALSE

    Code
      setup_brms(quiet = FALSE, brms_backend = "rstan", cores = 4)
    Message
      i Configured brms: set `options(mc.cores = 4)` and `options(brms.backend = 'rstan')`

# setup_cmdstanr dry_run does not require cmdstanr to be installed

    Code
      setup_cmdstanr(quiet = FALSE, force = FALSE, cores = 2, dry_run = TRUE)
    Message
      i Would check the CmdStan toolchain: cmdstanr::check_cmdstan_toolchain(quiet = FALSE).
      ! CmdStan binaries are missing or force-reinstall requested.
      i Would install or upgrade CmdStan: cmdstanr::install_cmdstan(quiet = FALSE, overwrite = TRUE, cores = 2).
      i Would configure cmdstanr: options(mc.cores = 2).

# setup_cmdstanr dry_run skips mutations but runs detection

    Code
      setup_cmdstanr(quiet = FALSE, force = FALSE, cores = 2, dry_run = TRUE)
    Message
      i Would check the CmdStan toolchain: cmdstanr::check_cmdstan_toolchain(quiet = FALSE).
      ! CmdStan binaries are missing or force-reinstall requested.
      i Would install or upgrade CmdStan: cmdstanr::install_cmdstan(quiet = FALSE, overwrite = TRUE, cores = 2).
      i Would configure cmdstanr: options(mc.cores = 2).

# setup_cmdstanr dry_run does not perform update checks

    Code
      setup_cmdstanr(quiet = FALSE, force = FALSE, check_updates = TRUE, cores = 2,
        dry_run = TRUE)
    Message
      i Would check the CmdStan toolchain: cmdstanr::check_cmdstan_toolchain(quiet = FALSE).
      i Found CmdStan v2.31.0 at '/tmp'
      i Would install or upgrade CmdStan if a newer release is found.
      i Would configure cmdstanr: options(mc.cores = 2).

# setup_interface warns when brms_backend adds cmdstanr

    Code
      setup_interface(interface = c("brms"), brms_backend = "cmdstanr", cores = 2,
      quiet = FALSE)
    Message
      i Adding cmdstanr to setup because `brms_backend = 'cmdstanr'`
      i Attaching brms...
      i Attaching cmdstanr...
      v Setup complete. brms, cmdstanr are attached; you do not need to run `library()`.

