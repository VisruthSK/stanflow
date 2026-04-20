# setup_interface warns when brms_backend adds cmdstanr

    Code
      setup_interface(interface = c("brms"), brms_backend = "cmdstanr", cores = 2,
      quiet = FALSE)
    Message
      i Adding cmdstanr to setup because `brms_backend = 'cmdstanr'`
      i Attaching brms...
      i Attaching cmdstanr...
      v Setup complete. brms, cmdstanr are attached; you do not need to run `library()`.

