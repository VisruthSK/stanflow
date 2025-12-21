#' Setup and Load Stan Interfaces
#'
#' This function ensures specific Stan interfaces are installed, configured,
#' and loaded. It handles package installation (from R-multiverse/CRAN (stable) or Stan
#' universe (dev)) and performs necessary one-time setup (like installing CmdStan).
#'
#' @param interface A character vector. Select at least one of: "brms", "cmdstanr", "rstan", "rstanarm".
#' @param dev Logical. If `FALSE` (default), installs stable releases from
#'   R-multiverse or CRAN. If `TRUE`, installs development versions from Stan R-universe.
#' @param prefer_cmdstanr Logical. If `TRUE`, configures `brms`
#'   to use the `cmdstanr` backend instead of the default `rstan`.
#' @param quiet Logical. If `TRUE`, suppresses status messages.
#' @param force Logical. If `TRUE`, forces re-installation/setup. Required
#'   for installation in non-interactive sessions.
#' @param skip_setup Logical. If `TRUE`, packages are only attached and no
#'   backend-specific configuration is run.
#' @return Returns `NULL` invisibly.
#' @export
setup_interface <- function(
  interface = c("brms", "cmdstanr", "rstan", "rstanarm"),
  dev = FALSE,
  prefer_cmdstanr = FALSE,
  quiet = FALSE,
  force = FALSE,
  skip_setup = FALSE
) {
  interface <- match.arg(interface, several.ok = TRUE)

  if (prefer_cmdstanr && !"cmdstanr" %in% interface) {
    if (!quiet) {
      cli::cli_alert_info(
        "Adding {.pkg cmdstanr} to setup because {.arg prefer_cmdstanr = TRUE}"
      )
    }
    interface <- c(interface, "cmdstanr")
  }

  for (pkg in interface) {
    if (!is_installed(pkg) || force) {
      install_backend_package(pkg, dev, quiet, force)
    }

    if (!quiet) {
      cli::cli_alert_info("Attaching {.pkg {pkg}}...")
    }

    suppressPackageStartupMessages(same_library(pkg))

    if (!skip_setup) {
      switch(
        pkg,
        "cmdstanr" = setup_cmdstanr(quiet, force),
        "rstan" = setup_rstan(quiet),
        "brms" = setup_brms(quiet, prefer_cmdstanr),
        "rstanarm" = setup_rstanarm(quiet, prefer_cmdstanr)
      )
    }
  }

  if (!quiet) {
    attached_pkgs <- paste0("{.pkg ", interface, "}", collapse = ", ")
    cli::cli_alert_success(
      cli::format_inline(
        "Setup complete. {attached_pkgs} are attached; you do not need to run {.code library()}."
      )
    )
  }

  invisible(NULL)
}

# nocov start
install_backend_package <- function(pkg, dev, quiet, force) {
  if (!quiet) {
    if (force) {
      cli::cli_alert_warning(
        "Reinstalling {.pkg {pkg}} because {.code force = TRUE}."
      )
    } else {
      cli::cli_alert_warning("Package {.pkg {pkg}} is not installed.")
    }
  }

  if (!interactive() && !force) {
    cli::cli_abort(c(
      "Package {.pkg {pkg}} is missing.",
      "x" = "Cannot naively install automatically in a non-interactive session.",
      "i" = "Run interactively or set {.code force = TRUE} to allow automated installation."
    ))
  }

  if (interactive() && !force) {
    title <- if (dev) {
      "Install from Stan Universe (Dev)?"
    } else {
      "Install from R-multiverse (Stable)?"
    }
    do_it <- utils::menu(c("Yes", "No"), title = title)
    if (do_it != 1) {
      cli::cli_abort("Installation of {.pkg {pkg}} aborted by user.")
    }
  }

  if (!quiet) {
    cli::cli_progress_step("Installing {.pkg {pkg}}...")
  }
  utils::install.packages(pkg, repos = stan_repos(dev), quiet = TRUE)
  if (!quiet) cli::cli_progress_done()
}

setup_cmdstanr <- function(quiet, force) {
  toolchain_ok <- tryCatch(
    {
      cmdstanr::check_cmdstan_toolchain(fix = TRUE, quiet = quiet)
      TRUE
    },
    error = function(e) {
      if (!quiet) {
        cli::cli_alert_danger("C++ toolchain broken: {e$message}")
      }
      FALSE
    }
  )

  if (!toolchain_ok) {
    cli::cli_abort(c(
      "CmdStan toolchain check failed.",
      "i" = "You need a C++ compiler (RTools on Windows, Xcode on Mac) to run Stan."
    ))
  }

  cmdstan_ready <- FALSE
  local_ver <- NULL
  tryCatch(
    {
      path <- cmdstanr::cmdstan_path()
      local_ver <- cmdstanr::cmdstan_version() |> numeric_version()
      if (!quiet) {
        cli::cli_alert_info("Found CmdStan v{local_ver} at {.path {path}}")
      }
      cmdstan_ready <- TRUE
    },
    error = function(e) {}
  )

  latest_ver <- NULL
  if (cmdstan_ready) {
    tryCatch(
      {
        raw_json <- suppressWarnings(readLines(
          "https://api.github.com/repos/stan-dev/cmdstan/releases/latest",
          warn = FALSE
        ))
        tag_line <- grep('"tag_name":', raw_json, value = TRUE)[1]
        if (!is.na(tag_line)) {
          latest_ver <- numeric_version(
            sub(
              '.*"tag_name":\\s*"v?([^"]+)".*',
              "\\1",
              tag_line
            )
          )
        }
      },
      error = function(e) {
        if (!quiet) {
          cli::cli_alert_warning(
            "Could not check for CmdStan updates (network/parsing error)."
          )
        }
      }
    )
  }

  needs_install <- !cmdstan_ready || force
  needs_update <- !is.null(latest_ver) &&
    !is.null(local_ver) &&
    (latest_ver > local_ver)

  if (needs_install) {
    action_msg <- "CmdStan binaries are missing or force-reinstall requested."
  } else if (needs_update) {
    action_msg <- sprintf("Update available: v%s -> v%s", local_ver, latest_ver)
  } else {
    return(invisible(TRUE))
  }

  if (!quiet) {
    cli::cli_alert_warning(action_msg)
  }

  if (!interactive() && !force) {
    if (needs_update && !needs_install) {
      if (!quiet) {
        cli::cli_alert_info(
          "Skipping update in non-interactive mode (set {.code force = TRUE} to upgrade)."
        )
      }
      return(invisible(TRUE))
    }
    cli::cli_abort(c(
      "CmdStan setup required.",
      "x" = "Cannot install in non-interactive session.",
      "i" = "Run interactively or set {.code force = TRUE}."
    ))
  }

  if (interactive() && !force) {
    title <- if (needs_install) {
      "Download and compile CmdStan now?"
    } else {
      "Upgrade CmdStan now?"
    }
    do_it <- utils::menu(c("Yes", "No"), title = title)
    if (do_it != 1) {
      if (needs_install) {
        cli::cli_abort("CmdStan setup aborted.")
      }
      if (needs_update) return(invisible(TRUE))
    }
  }

  if (!quiet) {
    cli::cli_process_start("Installing CmdStan (this takes time)...")
  }

  cmdstanr::install_cmdstan(quiet = quiet, overwrite = TRUE, cores = 2)

  if (!quiet) cli::cli_process_done()
}
# nocov end

setup_rstan <- function(quiet) {
  options(mc.cores = parallel::detectCores())
  rstan::rstan_options(auto_write = TRUE)

  if (!quiet) {
    cli::cli_alert_info(
      "Configured {.pkg rstan}: set {.code options(mc.cores = parallel::detectCores())} and {.code rstan::rstan_options(auto_write = TRUE)}"
    )
  }
}

setup_brms <- function(quiet, prefer_cmdstanr) {
  options(mc.cores = parallel::detectCores())

  msg <- "Configured {.pkg brms}: set {.code options(mc.cores = parallel::detectCores())}"

  if (prefer_cmdstanr) {
    options(brms.backend = "cmdstanr")
    msg <- paste0(msg, " and {.code options(brms.backend = 'cmdstanr')}")
  }

  if (!quiet) {
    cli::cli_alert_info(msg)
  }
}

setup_rstanarm <- function(quiet, prefer_cmdstanr) {
  options(mc.cores = parallel::detectCores())

  msg <- "Configured {.pkg rstanarm}: set {.code options(mc.cores = parallel::detectCores())}"

  if (prefer_cmdstanr) {
    msg <- paste0(msg, " (Note: {.arg prefer_cmdstanr} ignored)")
  }

  if (!quiet) {
    cli::cli_alert_info(msg)
  }
}
