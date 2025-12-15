test_that("setup_interface adds cmdstanr when prefer_cmdstanr = TRUE", {
  libraries <- character()

  with_mocked_bindings(
    is_installed = function(pkg) TRUE,
    install_backend_package = function(...) {
      stop("install_backend_package should not run")
    },
    same_library = function(pkg) libraries <<- c(libraries, pkg),
    setup_interface(
      interface = "brms",
      prefer_cmdstanr = TRUE,
      skip_setup = TRUE,
      quiet = FALSE
    )
  )

  expect_equal(libraries, c("brms", "cmdstanr"))
})

test_that("setup_interface installs backends when force = TRUE", {
  installed <- character()

  invisible(with_mocked_bindings(
    is_installed = function(pkg) TRUE,
    install_backend_package = function(pkg, ...) {
      installed <<- c(installed, pkg)
    },
    same_library = function(pkg) NULL,
    setup_interface(
      interface = c("cmdstanr", "rstan"),
      force = TRUE,
      skip_setup = TRUE,
      quiet = TRUE
    )
  ))

  expect_equal(installed, c("cmdstanr", "rstan"))
})

test_that("setup_interface runs backend setup helpers when skip_setup = FALSE", {
  calls <- character()

  invisible(with_mocked_bindings(
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
    setup_interface(
      interface = c("cmdstanr", "rstan", "brms", "rstanarm"),
      prefer_cmdstanr = TRUE,
      quiet = TRUE,
      skip_setup = FALSE
    )
  ))

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
