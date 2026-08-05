# wrapped_startup emits startup messages when enabled

    Code
      wrapped_startup("hello from stanflow")
    Message
      hello from stanflow

# local_cli_quiet suppresses cli messages within caller

    Code
      capture(FALSE)
    Output
      [1] "i hello from cli\n"

# local_cli_quiet restores cli output after caller exits

    Code
      cli::cli_alert_info("audible")
    Message
      i audible

