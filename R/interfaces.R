#' Setup and Load Stan Interfaces
#'
#' This function ensures specific Stan interfaces are installed, configured,
#' and loaded. It handles package installation (from R-multiverse/CRAN (stable) or Stan
#' universe (dev)) and performs necessary one-time setup (like installing CmdStan).
#'
#' @param interface A character vector. Must include at least one of: "brms", "cmdstanr", "rstan", "rstanarm".
#' @param dev Logical. If `FALSE` (default), installs stable releases from
#'   R-multiverse or CRAN. If `TRUE`, installs development versions from Stan R-universe.
#' @param prefer_cmdstanr Logical. If `TRUE`, configures `brms`
#'   to use the `cmdstanr` backend instead of the default `rstan`.
#' @param quiet Logical. If `TRUE`, suppresses status messages.
#' @param force Logical. If `TRUE`, runs non-interactively with no prompts.
#'   Required to allow installation in non-interactive sessions.
#' @param reinstall Logical. If `TRUE`, reinstalls interface packages and CmdStan
#'   when already present.
#' @param configure Logical. If `TRUE` (default), run backend-specific
#'   configuration; if `FALSE`, packages are only attached.
#' @param dry_run Logical. If `TRUE`, report planned actions (including the
#'   commands that would run) without installing, attaching, or configuring anything.
#' @return A list (returned invisibly) with elements:
#' \describe{
#'   \item{installed_packages}{Data frame of package versions before/after and action taken.}
#'   \item{configuration_actions}{Named list with \code{mode} ("applied" or "planned") and action strings.}
#'   \item{cmdstan}{CmdStan path/version status and any action taken.}
#'   \item{skipped}{Data frame of skipped items and reasons.}
#' }
#' @export
setup_interface <- function(
  interface = c("brms", "cmdstanr", "rstan", "rstanarm"),
  dev = FALSE,
  prefer_cmdstanr = FALSE,
  quiet = TRUE,
  force = FALSE,
  reinstall = FALSE,
  configure = TRUE,
  dry_run = FALSE
) {
  if (missing(interface)) {
    cli::cli_abort(
      "{.arg interface} must include at least one of brms, cmdstanr, rstan, rstanarm."
    )
  }
  interface <- interface |> match.arg(choices = backends, several.ok = TRUE)

  installed_packages <- data.frame(
    package = character(),
    before = character(),
    after = character(),
    action = character(),
    stringsAsFactors = FALSE
  )
  configuration_actions <- list()
  skipped <- data.frame(
    item = character(),
    reason = character(),
    stringsAsFactors = FALSE
  )
  cmdstan_status <- list(
    path = NA_character_,
    version = NA_character_,
    status = "not_requested",
    action = NA_character_,
    skipped_reason = NA_character_
  )
  planned_commands <- character()

  if (prefer_cmdstanr && !"cmdstanr" %in% interface) {
    if (!quiet) {
      cli::cli_alert_info(
        "Adding {.pkg cmdstanr} to setup because {.arg prefer_cmdstanr = TRUE}"
      )
    }
    interface <- c(interface, "cmdstanr")
  }

  for (pkg in interface) {
    before_version <- if (is_installed(pkg)) {
      as.character(utils::packageVersion(pkg))
    } else {
      NA_character_
    }
    pkg_installed <- !is.na(before_version)
    action <- "unchanged"
    needs_install <- !pkg_installed || reinstall

    if (needs_install && dry_run) {
      planned_commands <- c(
        planned_commands,
        sprintf(
          "install_backend_package(%s, dev = %s, quiet = %s, force = %s, reinstall = %s)",
          deparse(pkg),
          deparse(dev),
          deparse(quiet),
          deparse(force),
          deparse(reinstall)
        )
      )
      action <- if (!interactive() && !force) {
        "would_abort"
      } else if (interactive() && !force) {
        "would_prompt"
      } else if (pkg_installed) {
        "would_reinstall"
      } else {
        "would_install"
      }

      if (!quiet) {
        if (action == "would_prompt") {
          cli::cli_alert_info(
            "Dry run: would prompt to install {.pkg {pkg}}."
          )
        } else if (action == "would_abort") {
          cli::cli_alert_warning(
            "Dry run: would abort installing {.pkg {pkg}} (non-interactive and {.code force = FALSE})."
          )
        } else {
          verb <- if (pkg_installed) "reinstall" else "install"
          cli::cli_alert_info("Dry run: would {verb} {.pkg {pkg}}.")
        }
      }
    }

    if (needs_install && !dry_run) {
      install_backend_package(pkg, dev, quiet, force, reinstall)
      action <- if (pkg_installed) "reinstalled" else "installed"
    }

    after_version <- if (is_installed(pkg)) {
      as.character(utils::packageVersion(pkg))
    } else {
      NA_character_
    }

    installed_packages <- rbind(
      installed_packages,
      data.frame(
        package = pkg,
        before = before_version,
        after = after_version,
        action = action,
        stringsAsFactors = FALSE
      )
    )

    if (!quiet) {
      if (dry_run) {
        cli::cli_alert_info("Dry run: would attach {.pkg {pkg}}.")
      } else {
        cli::cli_alert_info("Attaching {.pkg {pkg}}...")
      }
    }

    if (dry_run) {
      planned_commands <- c(
        planned_commands,
        sprintf("same_library(%s)", deparse(pkg))
      )
    }

    if (!dry_run) {
      suppressPackageStartupMessages(same_library(pkg))
    }

    if (!configure) {
      skipped <- rbind(
        skipped,
        data.frame(
          item = paste0(pkg, " setup"),
          reason = "configure = FALSE",
          stringsAsFactors = FALSE
        )
      )
      if (pkg == "cmdstanr") {
        cmdstan_status <- list(
          path = NA_character_,
          version = NA_character_,
          status = "skipped",
          action = NA_character_,
          skipped_reason = "configure = FALSE"
        )
      }
      next
    }

    config_action <- switch(
      pkg,
      "cmdstanr" = {
        if (dry_run) {
          planned_commands <- c(
            planned_commands,
            sprintf(
              "setup_cmdstanr(quiet = %s, force = %s, reinstall = %s)",
              deparse(quiet),
              deparse(force),
              deparse(reinstall)
            )
          )
        }
        if (dry_run && !is_installed("cmdstanr")) {
          cmdstan_status <- list(
            path = NA_character_,
            version = NA_character_,
            status = "pending",
            action = "would_install_cmdstanr",
            skipped_reason = "cmdstanr not installed (dry run)"
          )
        } else {
          cmdstan_status <- setup_cmdstanr(quiet, force, reinstall, dry_run)
        }
        if (
          is.list(cmdstan_status) &&
            !is.null(cmdstan_status$skipped_reason) &&
            !is.na(cmdstan_status$skipped_reason)
        ) {
          skipped <- rbind(
            skipped,
            data.frame(
              item = "cmdstan update",
              reason = cmdstan_status$skipped_reason,
              stringsAsFactors = FALSE
            )
          )
        }
        NULL
      },
      "rstan" = {
        if (dry_run) {
          planned_commands <- c(
            planned_commands,
            sprintf("setup_rstan(quiet = %s)", deparse(quiet))
          )
        }
        setup_rstan(quiet, dry_run = dry_run)
      },
      "brms" = {
        if (dry_run) {
          planned_commands <- c(
            planned_commands,
            sprintf(
              "setup_brms(quiet = %s, prefer_cmdstanr = %s)",
              deparse(quiet),
              deparse(prefer_cmdstanr)
            )
          )
        }
        setup_brms(quiet, prefer_cmdstanr, dry_run = dry_run)
      },
      "rstanarm" = {
        if (dry_run) {
          planned_commands <- c(
            planned_commands,
            sprintf(
              "setup_rstanarm(quiet = %s, prefer_cmdstanr = %s)",
              deparse(quiet),
              deparse(prefer_cmdstanr)
            )
          )
        }
        setup_rstanarm(quiet, prefer_cmdstanr, dry_run = dry_run)
      }
    )

    if (!is.null(config_action)) {
      configuration_actions[[pkg]] <- list(
        mode = if (dry_run) "planned" else "applied",
        actions = config_action
      )
    }
  }

  if (!quiet) {
    attached_pkgs <- paste0("{.pkg ", interface, "}", collapse = ", ")
    message <- if (dry_run) {
      "Dry run complete. Would attach {attached_pkgs}; no changes were made."
    } else {
      "Setup complete. {attached_pkgs} are attached; you do not need to run {.code library()}."
    }
    cli::cli_alert_success(cli::format_inline(message))
  }

  if (dry_run) {
    cli::cat_line("Planned commands:")
    cli::cat_line(planned_commands)
  }

  invisible(
    list(
      installed_packages = installed_packages,
      configuration_actions = configuration_actions,
      cmdstan = cmdstan_status,
      skipped = skipped
    )
  )
}

# nocov start
install_backend_package <- function(pkg, dev, quiet, force, reinstall = FALSE) {
  was_installed <- is_installed(pkg)
  if (!quiet) {
    if (reinstall && was_installed) {
      cli::cli_alert_warning(
        "Reinstalling {.pkg {pkg}} because {.code reinstall = TRUE}."
      )
    } else {
      cli::cli_alert_warning("Package {.pkg {pkg}} is not installed.")
    }
  }

  if (!interactive() && !force) {
    main <- if (was_installed) {
      "Reinstall of {.pkg {pkg}} requested."
    } else {
      "Package {.pkg {pkg}} is missing."
    }
    cli::cli_abort(c(
      main,
      "x" = "Cannot prompt in a non-interactive session.",
      "i" = "Run interactively or set {.code force = TRUE} to proceed without prompts."
    ))
  }

  if (interactive() && !force) {
    verb <- if (reinstall && was_installed) "Reinstall" else "Install"
    title <- if (dev) {
      sprintf("%s from Stan Universe (Dev)?", verb)
    } else {
      sprintf("%s from R-multiverse (Stable)?", verb)
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

setup_cmdstanr <- function(quiet, force, reinstall = FALSE, dry_run = FALSE) {
  result <- list(
    path = NA_character_,
    version = NA_character_,
    status = "unknown",
    action = NA_character_,
    skipped_reason = NA_character_
  )

  run_quietly <- function(expr) {
    if (!quiet) {
      return(force(expr))
    }
    output_file <- tempfile()
    message_file <- tempfile()
    output_con <- file(output_file, open = "wt")
    message_con <- file(message_file, open = "wt")
    sink(output_con)
    sink(message_con, type = "message")
    on.exit(
      {
        sink(type = "message")
        sink()
        close(output_con)
        close(message_con)
        unlink(c(output_file, message_file))
      },
      add = TRUE
    )
    suppressWarnings(suppressMessages(force(expr)))
  }

  toolchain_ok <- tryCatch(
    {
      cmdstanr::check_cmdstan_toolchain(fix = !dry_run, quiet = quiet)
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
    if (dry_run) {
      result$status <- "toolchain_failed"
      result$action <- "would_abort"
      result$skipped_reason <- "toolchain check failed"
      return(invisible(result))
    }
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
      result$path <- path
      result$version <- as.character(local_ver)
      if (!quiet) {
        cli::cli_alert_info("Found CmdStan v{local_ver} at {.path {path}}")
      }
      cmdstan_ready <- TRUE
    },
    error = function(e) {}
  )

  latest_ver <- NULL
  if (cmdstan_ready && !reinstall) {
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

  needs_install <- !cmdstan_ready || reinstall
  needs_update <- !is.null(latest_ver) &&
    !is.null(local_ver) &&
    (latest_ver > local_ver)

  if (needs_install) {
    if (!cmdstan_ready) {
      action_msg <- "CmdStan binaries are missing."
    } else {
      action_msg <- "Reinstall requested."
    }
  } else if (needs_update) {
    action_msg <- sprintf("Update available: v%s -> v%s", local_ver, latest_ver)
  } else {
    result$status <- "up_to_date"
    return(invisible(result))
  }

  if (!quiet) {
    if (dry_run) {
      cli::cli_alert_info("Dry run: {action_msg}")
    } else {
      cli::cli_alert_warning(action_msg)
    }
  }

  if (dry_run) {
    if (!interactive() && !force && needs_install) {
      result$status <- "would_abort"
      result$action <- "would_abort"
      result$skipped_reason <- "non-interactive and force = FALSE"
      return(invisible(result))
    }
    if (!interactive() && !force && needs_update) {
      result$status <- "would_skip_update"
      result$action <- "would_skip_update"
      result$skipped_reason <- "non-interactive and force = FALSE"
      return(invisible(result))
    }
    if (interactive() && !force) {
      result$status <- "would_prompt"
      result$action <- "would_prompt"
      result$skipped_reason <- "confirmation required"
      return(invisible(result))
    }
    if (needs_install) {
      result$status <- if (reinstall && cmdstan_ready) {
        "would_reinstall"
      } else {
        "would_install"
      }
      result$action <- result$status
      return(invisible(result))
    }
    result$status <- "would_update"
    result$action <- "would_update"
    return(invisible(result))
  }

  if (!interactive() && !force) {
    if (needs_update && !needs_install) {
      if (!quiet) {
        cli::cli_alert_info(
          "Skipping update in non-interactive mode (set {.code force = TRUE} to upgrade)."
        )
      }
      result$status <- "update_skipped"
      result$skipped_reason <- "non-interactive and force = FALSE"
      return(invisible(result))
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
      if (needs_update) {
        result$status <- "update_skipped"
        result$skipped_reason <- "user declined"
        return(invisible(result))
      }
    }
  }

  if (!quiet) {
    cli::cli_process_start("Installing CmdStan (this takes time)...")
  }

  run_quietly(cmdstanr::install_cmdstan(
    quiet = quiet,
    overwrite = needs_install || needs_update,
    cores = 2
  ))

  tryCatch(
    {
      path <- cmdstanr::cmdstan_path()
      local_ver <- cmdstanr::cmdstan_version() |> numeric_version()
      result$path <- path
      result$version <- as.character(local_ver)
    },
    error = function(e) {}
  )

  if (!quiet) {
    cli::cli_process_done()
  }

  if (needs_install) {
    result$status <- if (reinstall && cmdstan_ready) {
      "reinstalled"
    } else {
      "installed"
    }
    result$action <- result$status
  } else {
    result$status <- "updated"
    result$action <- "updated"
  }

  invisible(result)
}
# nocov end

setup_rstan <- function(quiet, dry_run = FALSE) {
  if (!dry_run) {
    options(mc.cores = parallel::detectCores())
    rstan::rstan_options(auto_write = TRUE)
  }

  actions <- c(
    "options(mc.cores = parallel::detectCores())",
    "rstan::rstan_options(auto_write = TRUE)"
  )

  if (!quiet) {
    verb <- if (dry_run) "Would configure" else "Configured"
    cli::cli_alert_info(
      "{verb} {.pkg rstan}: set {.code options(mc.cores = parallel::detectCores())} and {.code rstan::rstan_options(auto_write = TRUE)}"
    )
  }

  invisible(actions)
}

setup_brms <- function(quiet, prefer_cmdstanr, dry_run = FALSE) {
  if (!dry_run) {
    options(mc.cores = parallel::detectCores())
  }

  actions <- "options(mc.cores = parallel::detectCores())"
  msg <- "Configured {.pkg brms}: set {.code options(mc.cores = parallel::detectCores())}"

  if (prefer_cmdstanr) {
    if (!dry_run) {
      options(brms.backend = "cmdstanr")
    }
    actions <- c(actions, "options(brms.backend = \"cmdstanr\")")
    msg <- paste0(msg, " and {.code options(brms.backend = 'cmdstanr')}")
  }

  if (!quiet) {
    if (dry_run) {
      msg <- sub("^Configured", "Would configure", msg)
    }
    cli::cli_alert_info(msg)
  }

  invisible(actions)
}

setup_rstanarm <- function(quiet, prefer_cmdstanr, dry_run = FALSE) {
  if (!dry_run) {
    options(mc.cores = parallel::detectCores())
  }

  actions <- "options(mc.cores = parallel::detectCores())"
  msg <- "Configured {.pkg rstanarm}: set {.code options(mc.cores = parallel::detectCores())}"

  if (prefer_cmdstanr) {
    msg <- paste0(msg, " (Note: {.arg prefer_cmdstanr} ignored)")
  }

  if (!quiet) {
    if (dry_run) {
      msg <- sub("^Configured", "Would configure", msg)
    }
    cli::cli_alert_info(msg)
  }

  invisible(actions)
}
