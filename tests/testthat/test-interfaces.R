interfaces <- c("brms", "cmdstanr", "rstan", "rstanarm")

grepl_fixed <- function(pattern, x) {
  specials <- c(
    ".",
    "^",
    "$",
    "*",
    "+",
    "?",
    "(",
    ")",
    "[",
    "]",
    "{",
    "}",
    "|",
    "\\"
  )
  has_regex <- any(strsplit(pattern, "", fixed = TRUE)[[1]] %in% specials)
  if (has_regex) grepl(pattern, x) else grepl(pattern, x, fixed = TRUE)
}

capture_dry_run <- function(...) {
  utils::capture.output(setup_interface(...))
}

test_that("setup_interface adds cmdstanr when prefer_cmdstanr = TRUE", {
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
    prefer_cmdstanr = TRUE,
    configure = FALSE,
    quiet = FALSE
  )

  expect_equal(libraries, c("brms", "cmdstanr"))
})

test_that("setup_interface aborts when interface is missing", {
  expect_error(
    setup_interface(),
    "interface"
  )
})

test_that("setup_interface dry_run reports planned actions", {
  local_mocked_bindings(
    is_installed = function(pkg) pkg == "brms",
    install_backend_package = function(...) stop("should not install"),
    same_library = function(...) stop("should not attach"),
    .package = "stanflow"
  )
  local_mocked_bindings(
    packageVersion = function(pkg) package_version("1.2.3"),
    .package = "utils"
  )

  output <- utils::capture.output(
    result <- setup_interface(
      interface = c("brms", "cmdstanr"),
      configure = TRUE,
      dry_run = TRUE,
      force = TRUE,
      quiet = TRUE
    )
  )

  expect_equal(
    result$installed_packages$action,
    c("unchanged", "would_install")
  )
  expect_equal(result$configuration_actions$brms$mode, "planned")
  expect_true(any(grepl("mc.cores", result$configuration_actions$brms$actions)))
  expect_equal(result$cmdstan$status, "pending")
  expect_true(any(grepl_fixed("Planned commands:", output)))
  expect_true(any(grepl("utils::install.packages\\(\"cmdstanr\"", output)))
  expect_true(any(grepl("library\\(\"brms\"", output)))
  expect_true(any(grepl("setup_brms\\(", output)))
  expect_true(any(grepl("setup_cmdstanr\\(", output)))
})

test_that("setup_interface dry_run prints planned commands for prefer_cmdstanr", {
  local_mocked_bindings(
    is_installed = function(pkg) TRUE,
    .package = "stanflow"
  )
  local_mocked_bindings(
    packageVersion = function(pkg) package_version("1.2.3"),
    .package = "utils"
  )

  output <- capture_dry_run(
    interface = "brms",
    prefer_cmdstanr = TRUE,
    configure = TRUE,
    dry_run = TRUE,
    quiet = TRUE
  )

  expect_equal(
    output,
    c(
      "Planned commands:",
      paste0(
        "library(\"brms\", lib.loc = if (\"brms\" %in% loadedNamespaces()) ",
        "dirname(getNamespaceInfo(\"brms\", \"path\")) else NULL, character.only = TRUE, ",
        "warn.conflicts = FALSE)"
      ),
      "setup_brms(quiet = TRUE, prefer_cmdstanr = TRUE)",
      paste0(
        "library(\"cmdstanr\", lib.loc = if (\"cmdstanr\" %in% loadedNamespaces()) ",
        "dirname(getNamespaceInfo(\"cmdstanr\", \"path\")) else NULL, character.only = TRUE, ",
        "warn.conflicts = FALSE)"
      ),
      "setup_cmdstanr(quiet = TRUE, force = FALSE, reinstall = FALSE)"
    )
  )
})

test_that("setup_interface dry_run prints planned commands for each interface", {
  setup_calls <- list(
    brms = "setup_brms",
    cmdstanr = "setup_cmdstanr",
    rstan = "setup_rstan",
    rstanarm = "setup_rstanarm"
  )

  local_mocked_bindings(
    is_installed = function(pkg) TRUE,
    .package = "stanflow"
  )
  local_mocked_bindings(
    packageVersion = function(pkg) package_version("1.2.3"),
    .package = "utils"
  )

  for (pkg in backends) {
    output <- capture_dry_run(
      interface = pkg,
      configure = TRUE,
      dry_run = TRUE,
      quiet = TRUE
    )

    expect_true(any(grepl_fixed(sprintf("library\\(\"%s\"", pkg), output)))
    expect_true(any(grepl_fixed(setup_calls[[pkg]], output)))
  }
})

test_that("setup_interface dry_run prints only attach commands when configure = FALSE", {
  local_mocked_bindings(
    is_installed = function(pkg) TRUE,
    .package = "stanflow"
  )
  local_mocked_bindings(
    packageVersion = function(pkg) package_version("1.2.3"),
    .package = "utils"
  )

  for (pkg in backends) {
    output <- capture_dry_run(
      interface = pkg,
      configure = FALSE,
      dry_run = TRUE,
      force = TRUE,
      quiet = TRUE
    )

    expect_true(any(grepl_fixed(sprintf("library\\(\"%s\"", pkg), output)))
    expect_false(any(grepl("setup_(brms|cmdstanr|rstan|rstanarm)", output)))
  }
})

test_that("setup_interface dry_run prints install commands for missing packages", {
  local_mocked_bindings(
    is_installed = function(pkg) FALSE,
    .package = "stanflow"
  )

  for (pkg in backends) {
    output <- capture_dry_run(
      interface = pkg,
      configure = FALSE,
      dry_run = TRUE,
      force = TRUE,
      quiet = TRUE
    )

    expect_true(
      any(grepl_fixed(sprintf("utils::install.packages\\(\"%s\"", pkg), output))
    )
  }
})

test_that("setup_interface dry_run uses dev repos when dev = TRUE", {
  local_mocked_bindings(
    is_installed = function(pkg) FALSE,
    .package = "stanflow"
  )

  for (pkg in backends) {
    output <- capture_dry_run(
      interface = pkg,
      dev = TRUE,
      configure = FALSE,
      dry_run = TRUE,
      force = TRUE,
      quiet = TRUE
    )

    expect_true(any(grepl_fixed("https://stan-dev.r-universe.dev", output)))
  }
})

test_that("setup_interface dry_run prints install commands for reinstall", {
  local_mocked_bindings(
    is_installed = function(pkg) TRUE,
    .package = "stanflow"
  )

  for (pkg in backends) {
    output <- capture_dry_run(
      interface = pkg,
      reinstall = TRUE,
      configure = FALSE,
      dry_run = TRUE,
      force = TRUE,
      quiet = TRUE
    )

    expect_true(
      any(grepl_fixed(sprintf("utils::install.packages\\(\"%s\"", pkg), output))
    )
  }
})

test_that("setup_interface reinstalls backends when reinstall = TRUE", {
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
      reinstall = TRUE,
      configure = FALSE,
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
      force = FALSE,
      configure = FALSE,
      quiet = TRUE
    )
  )

  expect_equal(installed, "brms")
})

test_that("setup_interface runs backend setup helpers when configure = TRUE", {
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
      prefer_cmdstanr = TRUE,
      quiet = TRUE,
      configure = TRUE
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
  old_repos <- options("repos")
  on.exit(options(old_repos), add = TRUE)
  options(repos = c(CRAN = "https://cloud.r-project.org"))
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
  old_repos <- options("repos")
  on.exit(options(old_repos), add = TRUE)
  options(repos = c(CRAN = "https://cloud.r-project.org"))
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

test_that("setup_brms configures cmdstanr backend when preferred", {
  old_opts <- options(mc.cores = NULL, brms.backend = NULL)
  on.exit(options(old_opts), add = TRUE)

  local_mocked_bindings(
    detectCores = function() 8,
    .package = "parallel"
  )

  setup_brms(quiet = TRUE, prefer_cmdstanr = TRUE)

  expect_equal(getOption("mc.cores"), 8)
  expect_equal(getOption("brms.backend"), "cmdstanr")
})

test_that("setup_rstan configures parallel cores and rstan options", {
  old_opts <- options(mc.cores = NULL)
  on.exit(options(old_opts), add = TRUE)
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

test_that("setup_rstanarm sets mc.cores regardless of prefer flag", {
  old_opts <- options(mc.cores = NULL)
  on.exit(options(old_opts), add = TRUE)

  local_mocked_bindings(
    detectCores = function() 4,
    .package = "parallel"
  )

  setup_rstanarm(quiet = TRUE, prefer_cmdstanr = TRUE)

  expect_equal(getOption("mc.cores"), 4)
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

test_that("setup_cmdstanr quiet suppresses output", {
  skip_on_cran()
  skip_if_not_installed("cmdstanr")

  local_mocked_bindings(
    check_cmdstan_toolchain = function(...) TRUE,
    cmdstan_path = function() stop("missing"),
    cmdstan_version = function() stop("missing"),
    install_cmdstan = function(...) {
      cat("noisy\n")
      message("noisy")
      invisible(NULL)
    },
    .package = "cmdstanr"
  )

  expect_silent(setup_cmdstanr(quiet = TRUE, force = TRUE))
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

  expect_equal(result$status, "up_to_date")
  expect_equal(result$path, "/tmp")
  expect_equal(result$version, "2.31.0")
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
    setup_interface(interface = "cmdstanr", quiet = TRUE, configure = FALSE),
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
    setup_interface(interface = "cmdstanr", quiet = TRUE, configure = FALSE),
    "library error"
  )
})

test_that("setup_interface warns when prefer_cmdstanr adds cmdstanr", {
  local_mocked_bindings(
    is_installed = function(pkg) TRUE,
    same_library = function(pkg) NULL,
    install_backend_package = function(...) stop("install should not run"),
    .package = "stanflow"
  )
  expect_snapshot_output(
    setup_interface(
      interface = c("brms"),
      prefer_cmdstanr = TRUE,
      configure = FALSE,
      quiet = FALSE
    )
  )
})
