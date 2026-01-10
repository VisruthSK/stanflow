run_interface_setup <- function(interface, brms_backend = NULL) {
  setup_interface(
    interface,
    cores = 2,
    force = TRUE,
    reinstall = TRUE,
    quiet = TRUE,
    brms_backend = brms_backend
  )

  expect_true(
    requireNamespace(interface, quietly = TRUE),
    info = paste("Missing package:", interface)
  )
  expect_false(is.null(getOption("mc.cores")))
  expect_true(paste0("package:", interface) %in% search())
}
