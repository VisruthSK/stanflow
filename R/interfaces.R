planned_install_steps <- function(pkg, dev, force, reinstall, pkg_installed) {
  pkg_label <- deparse(pkg)
  install_verb <- if (reinstall && pkg_installed) "Reinstall" else "Install"
  install_title <- if (dev) {
    sprintf("%s from Stan Universe (Dev)?", install_verb)
  } else {
    sprintf("%s from R-multiverse (Stable)?", install_verb)
  }
  repos_expr <- if (dev) {
    "c(\"https://stan-dev.r-universe.dev\", getOption(\"repos\"))"
  } else {
    "c(\"https://community.r-multiverse.org\", getOption(\"repos\"))"
  }
  action <- if (!interactive() && !force) {
    "would_abort"
  } else if (interactive() && !force) {
    # nocov
    "would_prompt" # nocov
  } else if (pkg_installed) {
    "would_reinstall"
  } else {
    "would_install"
  }
  planned <- character()
  if (action == "would_abort") {
    abort_main <- if (pkg_installed) {
      sprintf("Reinstall of %s requested.", pkg_label)
    } else {
      sprintf("Package %s is missing.", pkg_label)
    }
    planned <- add_planned(
      planned,
      sprintf("cli::cli_abort(%s)", deparse(abort_main))
    )
  } else {
    if (action == "would_prompt") {
      # nocov start
      planned <- add_planned(
        planned,
        sprintf(
          "utils::menu(c(\"Yes\", \"No\"), title = \"%s\")",
          install_title
        )
      )
    } # nocov end
    planned <- add_planned(
      planned,
      sprintf(
        "utils::install.packages(%s, repos = %s, quiet = TRUE)",
        pkg_label,
        repos_expr
      )
    )
  }
  list(action = action, planned = planned)
}

planned_attach_call <- function(pkg) {
  pkg_label <- deparse(pkg)
  sprintf(
    paste0(
      "library(%s, lib.loc = if (%s %%in%% loadedNamespaces()) ",
      "dirname(getNamespaceInfo(%s, \"path\")) else NULL, ",
      "character.only = TRUE, warn.conflicts = FALSE)"
    ),
    pkg_label,
    pkg_label,
    pkg_label
  )
}

planned_setup_call <- function(pkg, quiet, prefer_cmdstanr, force, reinstall) {
  switch(
    pkg,
    "cmdstanr" = sprintf(
      "setup_cmdstanr(quiet = %s, force = %s, reinstall = %s)",
      deparse(quiet),
      deparse(force),
      deparse(reinstall)
    ),
    "rstan" = sprintf("setup_rstan(quiet = %s)", deparse(quiet)),
    "brms" = sprintf(
      "setup_brms(quiet = %s, prefer_cmdstanr = %s)",
      deparse(quiet),
      deparse(prefer_cmdstanr)
    ),
    "rstanarm" = sprintf(
      "setup_rstanarm(quiet = %s, prefer_cmdstanr = %s)",
      deparse(quiet),
      deparse(prefer_cmdstanr)
    )
  )
}

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
    before_version <- pkg_version(pkg)
    pkg_installed <- !is.na(before_version)
    pkg_label <- deparse(pkg)
    action <- "unchanged"
    needs_install <- !pkg_installed || reinstall

    if (needs_install && dry_run) {
      plan <- planned_install_steps(pkg, dev, force, reinstall, pkg_installed)
      action <- plan$action
      planned_commands <- add_planned(planned_commands, plan$planned)

      if (!quiet) {
        if (action == "would_prompt") {
          # nocov start
          cli::cli_alert_info(
            "Dry run: would prompt to install {.pkg {pkg}}."
          )
          # nocov end
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

    after_version <- pkg_version(pkg)

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
      planned_commands <- add_planned(
        planned_commands,
        planned_attach_call(pkg)
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
          planned_commands <- add_planned(
            planned_commands,
            planned_setup_call(pkg, quiet, prefer_cmdstanr, force, reinstall)
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
          planned_commands <- add_planned(
            planned_commands,
            planned_setup_call(pkg, quiet, prefer_cmdstanr, force, reinstall)
          )
        }
        setup_rstan(quiet, dry_run = dry_run)
      },
      "brms" = {
        if (dry_run) {
          planned_commands <- add_planned(
            planned_commands,
            planned_setup_call(pkg, quiet, prefer_cmdstanr, force, reinstall)
          )
        }
        setup_brms(quiet, prefer_cmdstanr, dry_run = dry_run)
      },
      "rstanarm" = {
        if (dry_run) {
          planned_commands <- add_planned(
            planned_commands,
            planned_setup_call(pkg, quiet, prefer_cmdstanr, force, reinstall)
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

#' Configure CmdStan via cmdstanr
#'
#' Checks the toolchain and installs or updates CmdStan as needed.
#'
#' @param quiet Logical. If `TRUE`, suppresses status messages.
#' @param force Logical. If `TRUE`, runs non-interactively with no prompts.
#' @param reinstall Logical. If `TRUE`, reinstalls CmdStan when already present.
#' @param dry_run Logical. If `TRUE`, report planned actions without installing.
#' @return A list describing CmdStan status and any action taken.
#' @export
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

#' Configure rstan defaults
#'
#' @param quiet Logical. If `TRUE`, suppresses status messages.
#' @param dry_run Logical. If `TRUE`, report planned actions without side effects.
#' @return Character vector of configuration actions.
#' @export
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

#' Configure brms defaults
#'
#' @param quiet Logical. If `TRUE`, suppresses status messages.
#' @param prefer_cmdstanr Logical. If `TRUE`, configure the cmdstanr backend.
#' @param dry_run Logical. If `TRUE`, report planned actions without side effects.
#' @return Character vector of configuration actions.
#' @export
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

#' Configure rstanarm defaults
#'
#' @param quiet Logical. If `TRUE`, suppresses status messages.
#' @param prefer_cmdstanr Logical. Ignored for rstanarm.
#' @param dry_run Logical. If `TRUE`, report planned actions without side effects.
#' @return Character vector of configuration actions.
#' @export
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
