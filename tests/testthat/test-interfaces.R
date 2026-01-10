test_that("setup_interface adds cmdstanr when brms_backend = cmdstanr", {
  libraries <- character()
  local_mocked_bindings(
    is_installed = function(pkg) TRUE,
    install_backend_package = function(...) {
      stop("install_backend_package should not run")
    },
    same_library = function(pkg) libraries <<- c(libraries, pkg),
    setup_cmdstanr = function(...) invisible(NULL),
    setup_brms = function(...) invisible(NULL),
    .package = "stanflow"
  )

  setup_interface(
    interface = "brms",
    brms_backend = "cmdstanr",
    cores = 2,
    quiet = FALSE
  )

  expect_equal(libraries, c("brms", "cmdstanr"))
})

test_that("setup_interface aborts when cores are missing", {
  withr::local_options(list(mc.cores = NULL))
  expect_error(
    setup_interface(interface = "cmdstanr", quiet = TRUE),
    "cores"
  )
})

test_that("setup_interface installs backends when reinstall = TRUE", {
  installed <- character()

  local_mocked_bindings(
    is_installed = function(pkg) TRUE,
    install_backend_package = function(pkg, ...) {
      installed <<- c(installed, pkg)
    },
    same_library = function(pkg) NULL,
    setup_cmdstanr = function(...) invisible(NULL),
    setup_rstan = function(...) invisible(NULL),
    .package = "stanflow"
  )

  invisible(
    setup_interface(
      interface = c("cmdstanr", "rstan"),
      force = TRUE,
      reinstall = TRUE,
      cores = 2,
      quiet = TRUE
    )
  )

  expect_equal(installed, c("cmdstanr", "rstan"))
})

test_that("setup_interface installs missing packages when not installed", {
  skip_on_covr()
  installed <- character()

  local_mocked_bindings(
    is_installed = function(pkg) FALSE,
    install_backend_package = function(pkg, ...) {
      installed <<- c(installed, pkg)
    },
    same_library = function(pkg) NULL,
    setup_brms = function(...) invisible(NULL),
    .package = "stanflow"
  )

  invisible(
    setup_interface(
      interface = c("brms"),
      brms_backend = "rstan",
      force = FALSE,
      cores = 2,
      quiet = TRUE
    )
  )

  expect_equal(installed, "brms")
})

test_that("setup_interface runs backend setup helpers", {
  calls <- character()

  local_mocked_bindings(
    is_installed = function(pkg) TRUE,
    same_library = function(pkg) {
      calls <<- c(calls, paste0("library:", pkg))
      invisible(NULL)
    },
    setup_cmdstanr = function(...) {
      calls <<- c(calls, "setup_cmdstanr")
      invisible(NULL)
    },
    setup_rstan = function(...) {
      calls <<- c(calls, "setup_rstan")
      invisible(NULL)
    },
    setup_brms = function(...) {
      calls <<- c(calls, "setup_brms")
      invisible(NULL)
    },
    setup_rstanarm = function(...) {
      calls <<- c(calls, "setup_rstanarm")
      invisible(NULL)
    },
    .package = "stanflow"
  )

  invisible(
    setup_interface(
      interface = c("cmdstanr", "rstan", "brms", "rstanarm"),
      brms_backend = "cmdstanr",
      cores = 2,
      quiet = TRUE,
      force = FALSE
    )
  )

  expect_equal(
    calls,
    c(
      "library:cmdstanr",
      "setup_cmdstanr",
      "library:rstan",
      "setup_rstan",
      "library:brms",
      "setup_brms",
      "library:rstanarm",
      "setup_rstanarm"
    )
  )
})

test_that("install_backend_package installs from Stan universe when dev = TRUE", {
  withr::local_options(list(
    repos = c(CRAN = "https://cloud.r-project.org")
  ))
  called <- list()

  local_mocked_bindings(
    install.packages = function(pkg, repos, quiet) {
      called <<- list(pkg = pkg, repos = repos, quiet = quiet)
    },
    .package = "utils"
  )

  install_backend_package(
    "cmdstanr",
    dev = TRUE,
    quiet = TRUE,
    force = TRUE,
    reinstall = FALSE
  )

  expect_equal(called$pkg, "cmdstanr")
  expect_equal(unname(called$repos[1]), "https://stan-dev.r-universe.dev")
})

test_that("install_backend_package installs from multiverse when dev = FALSE", {
  withr::local_options(list(
    repos = c(CRAN = "https://cloud.r-project.org")
  ))
  called <- list()

  local_mocked_bindings(
    install.packages = function(pkg, repos, quiet) {
      called <<- list(pkg = pkg, repos = repos, quiet = quiet)
    },
    .package = "utils"
  )

  install_backend_package(
    "cmdstanr",
    dev = FALSE,
    quiet = TRUE,
    force = TRUE,
    reinstall = FALSE
  )

  expect_equal(called$pkg, "cmdstanr")
  expect_equal(unname(called$repos[1]), "https://community.r-multiverse.org")
  expect_equal(unname(called$repos[2]), unname(getOption("repos")))
})

test_that("install_backend_package aborts when user declines interactive install", {
  skip_on_covr()
  skip_if_not(interactive())
  local_mocked_bindings(
    menu = function(...) 2,
    install.packages = function(...) stop("should not reach install"),
    .package = "utils"
  )
  expect_error(
    install_backend_package(
      "cmdstanr",
      dev = FALSE,
      quiet = TRUE,
      force = FALSE,
      reinstall = FALSE
    ),
    "Installation of"
  )
})

test_that("install_backend_package installs when user accepts interactive prompt", {
  skip_on_covr()
  called <- list()
  skip_if_not(interactive())
  local_mocked_bindings(
    menu = function(...) 1,
    install.packages = function(pkg, repos, quiet) {
      called <<- list(pkg = pkg, repos = repos, quiet = quiet)
    },
    .package = "utils"
  )
  invisible(
    install_backend_package(
      "cmdstanr",
      dev = FALSE,
      quiet = TRUE,
      force = FALSE,
      reinstall = FALSE
    )
  )
  expect_equal(called$pkg, "cmdstanr")
})

test_that("setup_brms configures brms backend", {
  withr::local_options(list(mc.cores = NULL, brms.backend = NULL))

  setup_brms(quiet = TRUE, brms_backend = "cmdstanr", cores = 8)

  expect_equal(getOption("mc.cores"), 8)
  expect_equal(getOption("brms.backend"), "cmdstanr")
})

test_that("setup_brms emits configuration message when quiet = FALSE", {
  withr::local_options(list(mc.cores = NULL, brms.backend = NULL))
  msg <- NULL

  local_mocked_bindings(
    cli_alert_info = function(...) msg <<- paste0(...),
    .package = "cli"
  )

  setup_brms(quiet = FALSE, brms_backend = "rstan", cores = 4)

  expect_match(msg, "Configured")
  expect_match(msg, "brms")
})

test_that("setup_rstan configures parallel cores and rstan options", {
  withr::local_options(list(mc.cores = NULL))
  rstan_args <- NULL
  skip_if_not_installed("rstan")

  local_mocked_bindings(
    rstan_options = function(...) rstan_args <<- list(...),
    .package = "rstan"
  )

  setup_rstan(quiet = TRUE, cores = 6)

  expect_equal(getOption("mc.cores"), 6)
  expect_true(rstan_args$auto_write)
})

test_that("setup_rstan emits configuration message when quiet = FALSE", {
  withr::local_options(list(mc.cores = NULL))
  skip_if_not_installed("rstan")
  msg <- NULL

  local_mocked_bindings(
    rstan_options = function(...) NULL,
    .package = "rstan"
  )
  local_mocked_bindings(
    cli_alert_info = function(...) msg <<- paste0(...),
    .package = "cli"
  )

  setup_rstan(quiet = FALSE, cores = 3)

  expect_match(msg, "Configured")
  expect_match(msg, "rstan")
})

test_that("setup_rstanarm emits configuration message when quiet = FALSE", {
  withr::local_options(list(mc.cores = NULL))
  msg <- NULL

  local_mocked_bindings(
    cli_alert_info = function(...) msg <<- paste0(...),
    .package = "cli"
  )

  setup_rstanarm(quiet = FALSE, cores = 5)

  expect_match(msg, "Configured")
  expect_match(msg, "rstanarm")
})


test_that("setup_cmdstanr aborts when toolchain check fails", {
  skip_on_cran()
  skip_if_not_installed("cmdstanr")
  expect_error(
    {
      local_mocked_bindings(
        check_cmdstan_toolchain = function(...) stop("broken"),
        .package = "cmdstanr"
      )
      setup_cmdstanr(quiet = TRUE, force = FALSE, cores = 2)
    },
    "CmdStan toolchain check failed"
  )
})

test_that("setup_cmdstanr installs CmdStan when not ready and force = TRUE", {
  skip_on_cran()
  skip_if_not_installed("cmdstanr")
  installed <- FALSE

  local_mocked_bindings(
    check_cmdstan_toolchain = function(...) TRUE,
    cmdstan_path = function() stop("missing"),
    cmdstan_version = function() stop("missing"),
    install_cmdstan = function(...) installed <<- TRUE,
    .package = "cmdstanr"
  )

  invisible(setup_cmdstanr(quiet = TRUE, force = TRUE, cores = 2))

  expect_true(installed)
})

test_that("setup_cmdstanr returns invisibly when up to date", {
  skip_on_cran()
  skip_if_not_installed("cmdstanr")
  local_mocked_bindings(
    check_cmdstan_toolchain = function(...) TRUE,
    cmdstan_path = function() "/tmp",
    cmdstan_version = function() "2.31.0",
    install_cmdstan = function(...) stop("should not install"),
    .package = "cmdstanr"
  )
  local_mocked_bindings(
    readLines = function(...) stop("no network"),
    .package = "base"
  )
  result <- setup_cmdstanr(
    quiet = TRUE,
    force = FALSE,
    check_updates = FALSE,
    cores = 2
  )

  expect_identical(result, TRUE)
})

test_that("setup_interface aborts when install_backend_package fails", {
  local_mocked_bindings(
    is_installed = function(pkg) FALSE,
    install_backend_package = function(pkg, ...) {
      cli::cli_abort("Boom")
    },
    setup_cmdstanr = function(...) invisible(NULL),
    .package = "stanflow"
  )
  expect_error(
    setup_interface(interface = "cmdstanr", quiet = TRUE, cores = 2),
    "Boom"
  )
})

test_that("setup_interface handles same_library errors gracefully", {
  local_mocked_bindings(
    is_installed = function(pkg) TRUE,
    same_library = function(pkg) stop("library error"),
    setup_cmdstanr = function(...) invisible(NULL),
    .package = "stanflow"
  )
  expect_error(
    setup_interface(interface = "cmdstanr", quiet = TRUE, cores = 2),
    "library error"
  )
})

test_that("setup_interface warns when brms_backend adds cmdstanr", {
  local_mocked_bindings(
    is_installed = function(pkg) TRUE,
    same_library = function(pkg) NULL,
    install_backend_package = function(...) stop("install should not run"),
    setup_cmdstanr = function(...) invisible(NULL),
    setup_brms = function(...) invisible(NULL),
    .package = "stanflow"
  )
  expect_snapshot_output(
    setup_interface(
      interface = c("brms"),
      brms_backend = "cmdstanr",
      cores = 2,
      quiet = FALSE
    )
  )
})
