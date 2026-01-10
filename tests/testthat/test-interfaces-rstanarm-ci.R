test_that("setup_interface configures rstanarm", {
  skip_if_not(Sys.getenv("STANFLOW_INTERFACES_CI") == "true")
  skip_on_cran()
  withr::local_options(list(
    mc.cores = 2,
    stanflow.force_interactive = TRUE
  ))

  run_interface_setup("rstanarm")
})
