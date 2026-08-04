# dry_runner reports without evaluating expressions

    Code
      run_side_effect("set a flag", {
        ran <- TRUE
      })
    Message
      i Would set a flag.

# dry_runner includes debug code when provided

    Code
      run_side_effect("set a flag", {
        stop("should not run")
      }, code = "base::identity(TRUE)")
    Message
      i Would set a flag: base::identity(TRUE).

