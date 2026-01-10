test_that("setup_interface configures brms with rstan and cmdstanr", {
  skip_if_not(Sys.getenv("STANFLOW_INTERFACES_CI") == "true")
  skip_on_cran()
  withr::local_options(list(
    mc.cores = 2,
    stanflow.force_interactive = TRUE,
    brms.backend = NULL
  ))

  run_interface_setup("brms", brms_backend = "rstan")
  run_interface_setup("brms", brms_backend = "cmdstanr")
})
