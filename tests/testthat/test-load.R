test_that("message_packages formats two columns when even", {
  header <- "Header Line"
  pkgs <- c("pkgA 1.0.0", "pkgB 2.0.0", "pkgC 3.0.0", "pkgD 4.0.0")

  expect_equal(
    message_packages(pkgs, header),
    paste(
      header,
      "pkgA 1.0.0     pkgC 3.0.0",
      "pkgB 2.0.0     pkgD 4.0.0",
      sep = "\n"
    )
  )
})

test_that("message_packages pads uneven package lists", {
  header <- "Header Line"
  pkgs <- c("pkgA 1.0.0", "pkgB 2.0.0", "pkgC 3.0.0")

  expect_equal(
    message_packages(pkgs, header),
    paste(
      header,
      "pkgA 1.0.0     pkgC 3.0.0",
      "pkgB 2.0.0     ",
      sep = "\n"
    )
  )
})
