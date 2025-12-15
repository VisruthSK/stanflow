test_that("core_attach_message reflects real core packages", {
  testthat::local_reproducible_output(width = 80)
  pinned_versions <- c(
    bayesplot = "1.15.0",
    loo = "2.8.0.9000",
    posterior = "1.6.1",
    projpred = "2.10.0",
    shinystan = "2.7.0"
  )

  expect_snapshot_output(
    with_mocked_bindings(
      find_unloaded = function(pkgs) pkgs,
      same_library = function(...) NULL,
      package_version_h = function(pkg) {
        if (pkg == "stanflow") {
          "0.0.0.9000"
        } else if (pkg %in% names(pinned_versions)) {
          pinned_versions[[pkg]]
        } else {
          ""
        }
      },
      cat(core_attach_message(), "\n")
    )
  )
})

test_that("backends_attach_message shows installed vs missing", {
  testthat::local_reproducible_output(width = 80)
  pinned_versions <- c(
    brms = "2.22.0",
    cmdstanr = "0.9.0.9000",
    rstan = "2.36.0.9000"
  )

  expect_snapshot_output(
    with_mocked_bindings(
      is_installed = function(pkg) pkg != "rstanarm",
      is_attached = function(pkg) FALSE,
      package_version_h = function(pkg) {
        if (pkg %in% names(pinned_versions)) pinned_versions[[pkg]] else ""
      },
      cat(backends_attach_message(), "\n")
    )
  )
})
