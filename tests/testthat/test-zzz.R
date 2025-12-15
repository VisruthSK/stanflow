test_that(".onAttach prints conflicts", {
  expect_snapshot_output(
    with_mocked_bindings(
      wrapped_startup = function(msg) {
        if (!is.null(msg)) cat(msg, "\n", sep = "")
      },
      core_attach_message = function() "core message",
      backends_attach_message = function() "backend message",
      stanflow_conflicts = function() list(conflict = TRUE),
      stanflow_conflict_message = function(x) "conflict message",
      .package = "stanflow",
      stanflow:::.onAttach()
    )
  )
})
