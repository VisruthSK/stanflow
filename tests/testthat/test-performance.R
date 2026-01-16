test_that("scan_usage performance on faux_proj is within budget", {
  skip_if_not_installed("knitr")

  if (!isTRUE(getOption("stanflow.run_perf", FALSE))) {
    skip("Enable with options(stanflow.run_perf = TRUE).")
  }

  budget <- getOption("stanflow.perf_budget_ms", 15000)
  if (!is.numeric(budget) || length(budget) != 1L || !is.finite(budget)) {
    testthat::fail("stanflow.perf_budget_ms must be a finite number.")
  }

  faux_path <- testthat::test_path("faux_proj")

  timings <- replicate(10, {
    start <- proc.time()[["elapsed"]]
    suppressWarnings(suppressMessages(scan_usage(faux_path)))
    (proc.time()[["elapsed"]] - start) * 1000
  })

  median_ms <- stats::median(timings)
  message(sprintf("scan_usage median: %.1f ms", median_ms))
  expect_true(is.finite(median_ms))
  if (is.na(median_ms) || median_ms >= budget) {
    testthat::fail(
      sprintf("median %.1f ms (budget %.1f ms)", median_ms, budget)
    )
  }
})
