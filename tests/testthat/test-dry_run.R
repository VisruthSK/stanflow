test_that("dry_runner reports without evaluating expressions", {
  ran <- FALSE
  run_side_effect <- dry_runner(TRUE)

  out <- capture_messages(
    run_side_effect("set a flag", {
      ran <- TRUE
    })
  )

  expect_false(ran)
  expect_match(out, "Would set a flag")
})

test_that("dry_runner includes debug code when provided", {
  run_side_effect <- dry_runner(TRUE)

  out <- capture_messages(
    run_side_effect(
      "set a flag",
      {
        stop("should not run")
      },
      code = "base::identity(TRUE)"
    )
  )

  expect_match(out, "Would set a flag: base::identity\\(TRUE\\)")
})

test_that("dry_runner evaluates expressions when dry_run is FALSE", {
  ran <- FALSE
  run_side_effect <- dry_runner(FALSE)

  result <- run_side_effect("set a flag", {
    ran <- TRUE
    "value"
  })

  expect_true(ran)
  expect_null(result)
})
