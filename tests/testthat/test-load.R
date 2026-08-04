test_that("core_attach_message reflects real core packages", {
  pinned_versions <- c(
    bayesplot = "1.15.0",
    loo = "2.8.0.9000",
    posterior = "1.6.1",
    projpred = "2.10.0",
    shinystan = "2.7.0"
  )

  expect_snapshot_output(
    with_mocked_bindings(
      .find_unloaded = function(pkgs) pkgs,
      .same_library = function(...) NULL,
      .package_version_h = function(pkg) {
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
  pinned_versions <- c(
    brms = "2.22.0",
    cmdstanr = "0.9.0.9000",
    rstan = "2.36.0.9000"
  )

  expect_snapshot_output(
    with_mocked_bindings(
      is_installed = function(pkg) pkg != "rstanarm",
      is_attached = function(pkg) FALSE,
      .package_version_h = function(pkg) {
        if (pkg %in% names(pinned_versions)) pinned_versions[[pkg]] else ""
      },
      cat(backends_attach_message(), "\n")
    )
  )
})

test_that("core_attach_message returns NULL when nothing to show", {
  local_mocked_bindings(
    .find_unloaded = function(pkgs) character(),
    .same_library = function(...) NULL,
    .package = "stanflow"
  )

  expect_null(core_attach_message(show_all = FALSE))
})

test_that("core packages attach in declared order", {
  core <- getFromNamespace("core", "stanflow")
  calls <- character()

  with_mocked_bindings(
    .find_unloaded = function(pkgs) pkgs,
    .same_library = function(pkg) {
      calls <<- c(calls, pkg)
      NULL
    },
    .package = "stanflow",
    {
      core_attach_message(show_all = TRUE)
    }
  )

  expect_identical(calls, core)
})

test_that("backends_attach_message shows attached packages", {
  pinned_versions <- c(
    brms = "2.22.0",
    cmdstanr = "0.9.0.9000",
    rstan = "2.36.0.9000",
    rstanarm = "2.32.1"
  )

  expect_snapshot_output(
    with_mocked_bindings(
      is_installed = function(pkg) TRUE,
      is_attached = function(pkg) pkg %in% c("cmdstanr", "rstanarm"),
      .package_version_h = function(pkg) pinned_versions[[pkg]],
      {
        output <- backends_attach_message()
        output <- cli::ansi_strip(output)
        output <- gsub("\r\n", "\n", output, fixed = TRUE)
        output <- gsub("[ \t]+(?=\\r?\\n|$)", "", output, perl = TRUE)
        cat(output, "\n")
      }
    )
  )
})

test_that("flow_check prints and returns messages", {
  local_mocked_bindings(
    core_attach_message = function(...) "core-msg",
    backends_attach_message = function(...) "backend-msg",
    stanflow_conflicts = function(...) "conflicts",
    stanflow_conflict_message = function(...) "conflict-msg",
    .package = "stanflow"
  )

  expect_snapshot_output({
    result <- flow_check()
    expect_identical(result, c("core-msg", "backend-msg", "conflict-msg"))
  })
})

test_that("flow_check includes update status when requested (no updates)", {
  local_mocked_bindings(
    core_attach_message = function(...) "core-msg",
    backends_attach_message = function(...) "backend-msg",
    stanflow_conflicts = function(...) "conflicts",
    stanflow_conflict_message = function(...) "conflict-msg",
    update_check_message = function(...) "up-to-date-msg",
    .package = "stanflow"
  )

  expect_snapshot_output({
    result <- flow_check(check_updates = TRUE)
    expect_identical(
      result,
      c("core-msg", "backend-msg", "conflict-msg", "up-to-date-msg")
    )
  })
})

test_that("flow_check includes update list when packages are behind", {
  local_mocked_bindings(
    core_attach_message = function(...) "core-msg",
    backends_attach_message = function(...) "backend-msg",
    stanflow_conflicts = function(...) "conflicts",
    stanflow_conflict_message = function(...) "conflict-msg",
    update_check_message = function(...) "updates-msg",
    .package = "stanflow"
  )

  expect_snapshot_output({
    result <- flow_check(check_updates = TRUE)
    expect_identical(
      result,
      c("core-msg", "backend-msg", "conflict-msg", "updates-msg")
    )
  })
})

test_that("update_check_message prints up-to-date status", {
  expect_snapshot_output(
    with_mocked_bindings(
      stanflow_deps = function(...) {
        data.frame(
          package = c("cmdstanr", "posterior"),
          remote = c("1.2.0", "1.6.0"),
          local = c("1.2.0", "1.6.0"),
          behind = c(FALSE, FALSE),
          stringsAsFactors = FALSE
        )
      },
      cat(update_check_message(), "\n"),
      .package = "stanflow"
    )
  )
})

test_that("update_check_message prints update list", {
  expect_snapshot_output(
    with_mocked_bindings(
      stanflow_deps = function(...) {
        data.frame(
          package = c("cmdstanr", "posterior"),
          remote = c("1.2.0", "1.6.0"),
          local = c("1.1.0", "1.5.0"),
          behind = c(TRUE, TRUE),
          stringsAsFactors = FALSE
        )
      },
      cat(update_check_message(), "\n"),
      .package = "stanflow"
    )
  )
})

test_that("message_packages balances odd package counts", {
  output <- message_packages(c("pkgA", "pkgB", "pkgC"), "Header")
  expect_snapshot(cat(output))
})

test_that("package_version_h highlights development versions", {
  col_version <- with_mocked_bindings(
    packageVersion = function(pkg) base::package_version("1.2.9000"),
    .package = "utils",
    .package_version_h("dummy")
  )
  expect_match(col_version, "9000")
})
