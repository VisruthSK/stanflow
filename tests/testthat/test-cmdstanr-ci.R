testthat::skip_if_not(Sys.getenv("STANFLOW_CMDSTANR_CI") == "true")
testthat::skip_on_cran()

as_logical_env <- function(name, default = FALSE) {
  value <- tolower(Sys.getenv(name, ""))
  if (value == "") {
    return(default)
  }
  value %in% c("1", "true", "yes")
}

test_that("setup_cmdstanr handles missing and existing CmdStan", {
  withr::local_options(list(
    mc.cores = 2,
    stanflow.force_interactive = TRUE
  ))

  if (!requireNamespace("cmdstanr", quietly = TRUE)) {
    stanflow:::install_backend_package(
      "cmdstanr",
      dev = FALSE,
      quiet = TRUE,
      force = TRUE,
      reinstall = FALSE
    )
  }

  check_updates <- as_logical_env("STANFLOW_CMDSTANR_CHECK_UPDATES", TRUE)
  reinstall <- as_logical_env("STANFLOW_CMDSTANR_REINSTALL", FALSE)

  missing_path <- file.path(tempdir(), "cmdstan-missing")
  if (dir.exists(missing_path)) {
    unlink(missing_path, recursive = TRUE)
  }

  withr::local_envvar(c(CMDSTAN = missing_path))

  setup_cmdstanr(
    quiet = TRUE,
    force = TRUE,
    reinstall = reinstall,
    check_updates = check_updates,
    cores = 2
  )

  expect_true(dir.exists(cmdstanr::cmdstan_path()))

  result <- setup_cmdstanr(
    quiet = TRUE,
    force = FALSE,
    reinstall = FALSE,
    check_updates = FALSE,
    cores = 2
  )

  expect_true(isTRUE(result))
})
