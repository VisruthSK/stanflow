test_that("setup_interface adds cmdstanr when brms_backend = cmdstanr", {
  libraries <- character()
  local_mocked_bindings(
    is_installed = function(pkg) TRUE,
    install_backend_package = function(...) {
      stop("install_backend_package should not run")
    },
    same_library = function(pkg) libraries <<- c(libraries, pkg),
    .package = "stanflow"
  )

  setup_interface(
    interface = "brms",
    brms_backend = "cmdstanr",
    skip_setup = TRUE,
    quiet = FALSE
  )

  expect_equal(libraries, c("brms", "cmdstanr"))
})

test_that("setup_interface installs backends when force = TRUE", {
  installed <- character()

  local_mocked_bindings(
    is_installed = function(pkg) TRUE,
    install_backend_package = function(pkg, ...) {
      installed <<- c(installed, pkg)
    },
    same_library = function(pkg) NULL,
    .package = "stanflow"
  )

  invisible(
    setup_interface(
      interface = c("cmdstanr", "rstan"),
      force = TRUE,
      skip_setup = TRUE,
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
    .package = "stanflow"
  )

  invisible(
    setup_interface(
      interface = c("brms"),
      brms_backend = "rstan",
      force = FALSE,
      skip_setup = TRUE,
      quiet = TRUE
    )
  )

  expect_equal(installed, "brms")
})

test_that("setup_interface runs backend setup helpers when skip_setup = FALSE", {
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
      quiet = TRUE,
      skip_setup = FALSE
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

  install_backend_package("cmdstanr", dev = TRUE, quiet = TRUE, force = TRUE)

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

  install_backend_package("cmdstanr", dev = FALSE, quiet = TRUE, force = TRUE)

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
      force = FALSE
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
      force = FALSE
    )
  )
  expect_equal(called$pkg, "cmdstanr")
})

test_that("setup_brms configures brms backend", {
  withr::local_options(list(mc.cores = NULL, brms.backend = NULL))

  local_mocked_bindings(
    detectCores = function() 8,
    .package = "parallel"
  )

  setup_brms(quiet = TRUE, brms_backend = "cmdstanr")

  expect_equal(getOption("mc.cores"), 8)
  expect_equal(getOption("brms.backend"), "cmdstanr")
})

test_that("setup_rstan configures parallel cores and rstan options", {
  withr::local_options(list(mc.cores = NULL))
  rstan_args <- NULL
  skip_if_not_installed("rstan")

  local_mocked_bindings(
    detectCores = function() 6,
    .package = "parallel"
  )
  local_mocked_bindings(
    rstan_options = function(...) rstan_args <<- list(...),
    .package = "rstan"
  )

  setup_rstan(quiet = TRUE)

  expect_equal(getOption("mc.cores"), 6)
  expect_true(rstan_args$auto_write)
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
      setup_cmdstanr(quiet = TRUE, force = FALSE)
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

  invisible(setup_cmdstanr(quiet = TRUE, force = TRUE))

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
  result <- setup_cmdstanr(quiet = TRUE, force = FALSE)

  expect_identical(result, TRUE)
})

test_that("setup_interface aborts when install_backend_package fails", {
  local_mocked_bindings(
    is_installed = function(pkg) FALSE,
    install_backend_package = function(pkg, ...) {
      cli::cli_abort("Boom")
    },
    .package = "stanflow"
  )
  expect_error(
    setup_interface(interface = "cmdstanr", quiet = TRUE, skip_setup = TRUE),
    "Boom"
  )
})

test_that("setup_interface handles same_library errors gracefully", {
  local_mocked_bindings(
    is_installed = function(pkg) TRUE,
    same_library = function(pkg) stop("library error"),
    .package = "stanflow"
  )
  expect_error(
    setup_interface(interface = "cmdstanr", quiet = TRUE, skip_setup = TRUE),
    "library error"
  )
})

test_that("setup_interface warns when brms_backend adds cmdstanr", {
  local_mocked_bindings(
    is_installed = function(pkg) TRUE,
    same_library = function(pkg) NULL,
    install_backend_package = function(...) stop("install should not run"),
    .package = "stanflow"
  )
  expect_snapshot_output(
    setup_interface(
      interface = c("brms"),
      brms_backend = "cmdstanr",
      skip_setup = TRUE,
      quiet = FALSE
    )
  )
})
