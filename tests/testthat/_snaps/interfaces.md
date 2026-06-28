# setup_interface dry_run output is stable for brms

    Code
      setup_interface(interface = interface, brms_backend = "rstan", cores = 1,
        quiet = FALSE, dry_run = TRUE)
    Message
      i Would configure brms: set `options(mc.cores = 1)` and `options(brms.backend = 'rstan')`: options(mc.cores = 1); options(brms.backend = "rstan").
      i Would attach brms: stanflow:::.same_library("brms").

# setup_interface dry_run output is stable for cmdstanr

    Code
      setup_interface(interface = interface, brms_backend = "rstan", cores = 1,
        quiet = FALSE, dry_run = TRUE)
    Message
      i Would check and fix the CmdStan toolchain: cmdstanr::check_cmdstan_toolchain(fix = TRUE, quiet = FALSE).
      i Would configure cmdstanr: set `options(mc.cores = 1)`: options(mc.cores = 1).
      i Would attach cmdstanr: stanflow:::.same_library("cmdstanr").

# setup_interface dry_run output is stable for rstan

    Code
      setup_interface(interface = interface, brms_backend = "rstan", cores = 1,
        quiet = FALSE, dry_run = TRUE)
    Message
      i Would configure rstan: set `options(mc.cores = 1)` and `rstan::rstan_options(auto_write = TRUE)`: options(mc.cores = 1); rstan::rstan_options(auto_write = TRUE).
      i Would attach rstan: stanflow:::.same_library("rstan").

# setup_interface dry_run output is stable for rstanarm

    Code
      setup_interface(interface = interface, brms_backend = "rstan", cores = 1,
        quiet = FALSE, dry_run = TRUE)
    Message
      i Would configure rstanarm: set `options(mc.cores = 1)`: options(mc.cores = 1).
      i Would attach rstanarm: stanflow:::.same_library("rstanarm").

# setup_interface dry_run output is stable for brms, cmdstanr

    Code
      setup_interface(interface = interface, brms_backend = "rstan", cores = 1,
        quiet = FALSE, dry_run = TRUE)
    Message
      i Would configure brms: set `options(mc.cores = 1)` and `options(brms.backend = 'rstan')`: options(mc.cores = 1); options(brms.backend = "rstan").
      i Would attach brms: stanflow:::.same_library("brms").
      i Would check and fix the CmdStan toolchain: cmdstanr::check_cmdstan_toolchain(fix = TRUE, quiet = FALSE).
      i Would configure cmdstanr: set `options(mc.cores = 1)`: options(mc.cores = 1).
      i Would attach cmdstanr: stanflow:::.same_library("cmdstanr").

# setup_interface dry_run output is stable for brms, rstan

    Code
      setup_interface(interface = interface, brms_backend = "rstan", cores = 1,
        quiet = FALSE, dry_run = TRUE)
    Message
      i Would configure brms: set `options(mc.cores = 1)` and `options(brms.backend = 'rstan')`: options(mc.cores = 1); options(brms.backend = "rstan").
      i Would attach brms: stanflow:::.same_library("brms").
      i Would configure rstan: set `options(mc.cores = 1)` and `rstan::rstan_options(auto_write = TRUE)`: options(mc.cores = 1); rstan::rstan_options(auto_write = TRUE).
      i Would attach rstan: stanflow:::.same_library("rstan").

# setup_interface dry_run output is stable for brms, rstanarm

    Code
      setup_interface(interface = interface, brms_backend = "rstan", cores = 1,
        quiet = FALSE, dry_run = TRUE)
    Message
      i Would configure brms: set `options(mc.cores = 1)` and `options(brms.backend = 'rstan')`: options(mc.cores = 1); options(brms.backend = "rstan").
      i Would attach brms: stanflow:::.same_library("brms").
      i Would configure rstanarm: set `options(mc.cores = 1)`: options(mc.cores = 1).
      i Would attach rstanarm: stanflow:::.same_library("rstanarm").

# setup_interface dry_run output is stable for cmdstanr, rstan

    Code
      setup_interface(interface = interface, brms_backend = "rstan", cores = 1,
        quiet = FALSE, dry_run = TRUE)
    Message
      i Would check and fix the CmdStan toolchain: cmdstanr::check_cmdstan_toolchain(fix = TRUE, quiet = FALSE).
      i Would configure cmdstanr: set `options(mc.cores = 1)`: options(mc.cores = 1).
      i Would attach cmdstanr: stanflow:::.same_library("cmdstanr").
      i Would configure rstan: set `options(mc.cores = 1)` and `rstan::rstan_options(auto_write = TRUE)`: options(mc.cores = 1); rstan::rstan_options(auto_write = TRUE).
      i Would attach rstan: stanflow:::.same_library("rstan").

# setup_interface dry_run output is stable for cmdstanr, rstanarm

    Code
      setup_interface(interface = interface, brms_backend = "rstan", cores = 1,
        quiet = FALSE, dry_run = TRUE)
    Message
      i Would check and fix the CmdStan toolchain: cmdstanr::check_cmdstan_toolchain(fix = TRUE, quiet = FALSE).
      i Would configure cmdstanr: set `options(mc.cores = 1)`: options(mc.cores = 1).
      i Would attach cmdstanr: stanflow:::.same_library("cmdstanr").
      i Would configure rstanarm: set `options(mc.cores = 1)`: options(mc.cores = 1).
      i Would attach rstanarm: stanflow:::.same_library("rstanarm").

# setup_interface dry_run output is stable for rstan, rstanarm

    Code
      setup_interface(interface = interface, brms_backend = "rstan", cores = 1,
        quiet = FALSE, dry_run = TRUE)
    Message
      i Would configure rstan: set `options(mc.cores = 1)` and `rstan::rstan_options(auto_write = TRUE)`: options(mc.cores = 1); rstan::rstan_options(auto_write = TRUE).
      i Would attach rstan: stanflow:::.same_library("rstan").
      i Would configure rstanarm: set `options(mc.cores = 1)`: options(mc.cores = 1).
      i Would attach rstanarm: stanflow:::.same_library("rstanarm").

# setup_interface dry_run output is stable for brms, cmdstanr, rstan

    Code
      setup_interface(interface = interface, brms_backend = "rstan", cores = 1,
        quiet = FALSE, dry_run = TRUE)
    Message
      i Would configure brms: set `options(mc.cores = 1)` and `options(brms.backend = 'rstan')`: options(mc.cores = 1); options(brms.backend = "rstan").
      i Would attach brms: stanflow:::.same_library("brms").
      i Would check and fix the CmdStan toolchain: cmdstanr::check_cmdstan_toolchain(fix = TRUE, quiet = FALSE).
      i Would configure cmdstanr: set `options(mc.cores = 1)`: options(mc.cores = 1).
      i Would attach cmdstanr: stanflow:::.same_library("cmdstanr").
      i Would configure rstan: set `options(mc.cores = 1)` and `rstan::rstan_options(auto_write = TRUE)`: options(mc.cores = 1); rstan::rstan_options(auto_write = TRUE).
      i Would attach rstan: stanflow:::.same_library("rstan").

# setup_interface dry_run output is stable for brms, cmdstanr, rstanarm

    Code
      setup_interface(interface = interface, brms_backend = "rstan", cores = 1,
        quiet = FALSE, dry_run = TRUE)
    Message
      i Would configure brms: set `options(mc.cores = 1)` and `options(brms.backend = 'rstan')`: options(mc.cores = 1); options(brms.backend = "rstan").
      i Would attach brms: stanflow:::.same_library("brms").
      i Would check and fix the CmdStan toolchain: cmdstanr::check_cmdstan_toolchain(fix = TRUE, quiet = FALSE).
      i Would configure cmdstanr: set `options(mc.cores = 1)`: options(mc.cores = 1).
      i Would attach cmdstanr: stanflow:::.same_library("cmdstanr").
      i Would configure rstanarm: set `options(mc.cores = 1)`: options(mc.cores = 1).
      i Would attach rstanarm: stanflow:::.same_library("rstanarm").

# setup_interface dry_run output is stable for brms, rstan, rstanarm

    Code
      setup_interface(interface = interface, brms_backend = "rstan", cores = 1,
        quiet = FALSE, dry_run = TRUE)
    Message
      i Would configure brms: set `options(mc.cores = 1)` and `options(brms.backend = 'rstan')`: options(mc.cores = 1); options(brms.backend = "rstan").
      i Would attach brms: stanflow:::.same_library("brms").
      i Would configure rstan: set `options(mc.cores = 1)` and `rstan::rstan_options(auto_write = TRUE)`: options(mc.cores = 1); rstan::rstan_options(auto_write = TRUE).
      i Would attach rstan: stanflow:::.same_library("rstan").
      i Would configure rstanarm: set `options(mc.cores = 1)`: options(mc.cores = 1).
      i Would attach rstanarm: stanflow:::.same_library("rstanarm").

# setup_interface dry_run output is stable for cmdstanr, rstan, rstanarm

    Code
      setup_interface(interface = interface, brms_backend = "rstan", cores = 1,
        quiet = FALSE, dry_run = TRUE)
    Message
      i Would check and fix the CmdStan toolchain: cmdstanr::check_cmdstan_toolchain(fix = TRUE, quiet = FALSE).
      i Would configure cmdstanr: set `options(mc.cores = 1)`: options(mc.cores = 1).
      i Would attach cmdstanr: stanflow:::.same_library("cmdstanr").
      i Would configure rstan: set `options(mc.cores = 1)` and `rstan::rstan_options(auto_write = TRUE)`: options(mc.cores = 1); rstan::rstan_options(auto_write = TRUE).
      i Would attach rstan: stanflow:::.same_library("rstan").
      i Would configure rstanarm: set `options(mc.cores = 1)`: options(mc.cores = 1).
      i Would attach rstanarm: stanflow:::.same_library("rstanarm").

# setup_interface dry_run output is stable for brms, cmdstanr, rstan, rstanarm

    Code
      setup_interface(interface = interface, brms_backend = "rstan", cores = 1,
        quiet = FALSE, dry_run = TRUE)
    Message
      i Would configure brms: set `options(mc.cores = 1)` and `options(brms.backend = 'rstan')`: options(mc.cores = 1); options(brms.backend = "rstan").
      i Would attach brms: stanflow:::.same_library("brms").
      i Would check and fix the CmdStan toolchain: cmdstanr::check_cmdstan_toolchain(fix = TRUE, quiet = FALSE).
      i Would configure cmdstanr: set `options(mc.cores = 1)`: options(mc.cores = 1).
      i Would attach cmdstanr: stanflow:::.same_library("cmdstanr").
      i Would configure rstan: set `options(mc.cores = 1)` and `rstan::rstan_options(auto_write = TRUE)`: options(mc.cores = 1); rstan::rstan_options(auto_write = TRUE).
      i Would attach rstan: stanflow:::.same_library("rstan").
      i Would configure rstanarm: set `options(mc.cores = 1)`: options(mc.cores = 1).
      i Would attach rstanarm: stanflow:::.same_library("rstanarm").

# setup_interface warns when brms_backend adds cmdstanr

    Code
      setup_interface(interface = c("brms"), brms_backend = "cmdstanr", cores = 2,
      quiet = FALSE)
    Message
      i Adding cmdstanr to setup because `brms_backend = 'cmdstanr'`
      i Attaching brms...
      i Attaching cmdstanr...
      v Setup complete. brms, cmdstanr are attached; you do not need to run `library()`.

