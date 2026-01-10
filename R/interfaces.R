#' Setup and Load Stan Interfaces
#'
#' This function ensures specific Stan interfaces are installed, configured,
#' and loaded. It handles package installation (from R-multiverse/CRAN (stable) or Stan
#' universe (dev)) and performs necessary one-time setup (like installing CmdStan).
#'
#' @param interface A character vector. Select at least one of: "brms", "cmdstanr", "rstan", "rstanarm".
#' @param dev Logical. If `FALSE` (default), installs stable releases from
#'   R-multiverse or CRAN. If `TRUE`, installs development versions from Stan R-universe.
#' @param brms_backend Character. The `brms` backend to use Defaults to
#'   `getOption("brms.backend", "cmdstanr")` and must be one of
#'   `c("cmdstanr", "rstan")`.
#' @param cores Integer. Number of cores to use. Defaults to
#'   `getOption("mc.cores")`. You must set `options(mc.cores = ...)` or pass
#'   `cores` explicitly.
#' @param quiet Logical. If `TRUE`, suppresses status messages.
#' @param force Logical. If `TRUE`, allows installation in non-interactive sessions.
#' @param reinstall Logical. If `TRUE`, forces re-installation.
#' @param check_updates Logical. If `TRUE`, checks for CmdStan updates.
#' @return Returns `NULL` invisibly.
#' @export
setup_interface <- function(
  interface = c("brms", "cmdstanr", "rstan", "rstanarm"),
  cores = getOption("mc.cores"),
  quiet = TRUE,
  force = FALSE,
  reinstall = FALSE,
  check_updates = FALSE,
  dev = FALSE,
  brms_backend = c("cmdstanr", "rstan")
) {
  local_cli_quiet(quiet)

  interface <- match.arg(interface, several.ok = TRUE)

  if (missing(brms_backend)) {
    brms_backend <- getOption("brms.backend", "cmdstanr")
  }
  brms_backend <- match.arg(brms_backend, c("cmdstanr", "rstan"))

  if (is.null(cores)) {
    cli::cli_abort(
      c(
        "{.arg cores} must be provided when setup is enabled.",
        "x" = "No default {._opt mc.cores} option is set.",
        "i" = "Set {.code options(mc.cores = ...)} or pass {.arg cores}."
      )
    )
  }

  if (brms_backend == "cmdstanr" && !"cmdstanr" %in% interface) {
    cli::cli_alert_info(
      "Adding {.pkg cmdstanr} to setup because {.arg brms_backend = 'cmdstanr'}"
    )
    interface <- c(interface, "cmdstanr")
  }

  for (pkg in interface) {
    if (!is_installed(pkg) || reinstall) {
      install_backend_package(pkg, dev, quiet, force, reinstall)
    }

    cli::cli_alert_info("Attaching {.pkg {pkg}}...")

    suppressPackageStartupMessages(same_library(pkg))

    switch(
      pkg,
      "cmdstanr" = setup_cmdstanr(
        quiet,
        force,
        reinstall,
        check_updates,
        cores
      ),
      "rstan" = setup_rstan(quiet, cores),
      "brms" = setup_brms(quiet, brms_backend, cores),
      "rstanarm" = setup_rstanarm(quiet, cores)
    )
  }

  attached_pkgs <- paste0("{.pkg ", interface, "}", collapse = ", ")
  cli::cli_alert_success(
    cli::format_inline(
      "Setup complete. {attached_pkgs} are attached; you do not need to run {.code library()}."
    )
  )

  invisible(NULL)
}

# nocov start
install_backend_package <- function(pkg, dev, quiet, force, reinstall) {
  local_cli_quiet(quiet)

  if (reinstall) {
    cli::cli_alert_warning(
      "Reinstalling {.pkg {pkg}} because {.code reinstall = TRUE}."
    )
  } else {
    cli::cli_alert_warning("Package {.pkg {pkg}} is not installed.")
  }

  if (!is_interactive_session() && !force) {
    cli::cli_abort(
      c(
        "Package {.pkg {pkg}} is missing.",
        "x" = "Cannot naively install automatically in a non-interactive session.",
        "i" = "Run interactively or set {.code force = TRUE} to allow automated installation."
      )
    )
  }

  if (is_interactive_session() && !force) {
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

  cli::cli_progress_step("Installing {.pkg {pkg}}...")
  utils::install.packages(pkg, repos = stan_repos(dev), quiet = TRUE)
  cli::cli_progress_done()
}

#' Setup cmdstanr and CmdStan
#'
#' Checks the C++ toolchain, locates CmdStan, and installs or upgrades
#' CmdStan if needed. Prefer `setup_interface()` for user-facing setup since
#' it performs argument validation and defaults; `setup_cmdstanr()` assumes
#' inputs are already checked.
#'
#' @param quiet Logical. If `TRUE`, suppresses status messages.
#' @param force Logical. If `TRUE`, forces installation or upgrade in
#'   non-interactive sessions.
#' @param reinstall Logical. If `TRUE`, forces re-installation.
#' @param check_updates Logical. If `FALSE`, skips checking for CmdStan updates.
#' @param cores Integer. Number of cores to use when building CmdStan.
#' @return Returns `TRUE` invisibly when no install/upgrade is needed.
#'   Otherwise, returns `NULL` invisibly after installation.
#' @export
setup_cmdstanr <- function(
  quiet,
  force,
  reinstall = FALSE,
  check_updates = TRUE,
  cores
) {
  local_cli_quiet(quiet)

  toolchain_ok <- tryCatch(
    {
      cmdstanr::check_cmdstan_toolchain(fix = TRUE, quiet = quiet)
      TRUE
    },
    error = function(e) {
      cli::cli_alert_danger("C++ toolchain broken: {e$message}")
      FALSE
    }
  )

  if (!toolchain_ok) {
    cli::cli_abort(
      c(
        "CmdStan toolchain check failed.",
        "i" = "You need a C++ compiler (RTools on Windows, Xcode on Mac) to run Stan.",
        "i" = "Re-run {.code cmdstanr::check_cmdstan_toolchain(fix = TRUE, quiet = FALSE)} for detailed diagnostics."
      )
    )
  }

  cmdstan_ready <- FALSE
  local_ver <- NULL
  tryCatch(
    {
      path <- cmdstanr::cmdstan_path()
      local_ver <- cmdstanr::cmdstan_version() |> numeric_version()
      cli::cli_alert_info("Found CmdStan v{local_ver} at {.path {path}}")
      cmdstan_ready <- TRUE
    },
    error = function(e) {}
  )

  latest_ver <- NULL
  if (cmdstan_ready && check_updates) {
    tryCatch(
      {
        raw_json <- suppressWarnings(
          readLines(
            "https://api.github.com/repos/stan-dev/cmdstan/releases/latest",
            warn = FALSE
          )
        )
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
        cli::cli_abort(
          c(
            "Could not check for CmdStan updates.",
            "x" = "Network or parsing error while reaching GitHub.",
            "i" = "Set {.code check_updates = FALSE} to skip update checks."
          )
        )
      }
    )
  }

  needs_install <- !cmdstan_ready || reinstall
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

  cli::cli_alert_warning(action_msg)

  if (!is_interactive_session() && !force) {
    if (needs_update && !needs_install) {
      cli::cli_alert_info(
        "Skipping update in non-interactive mode (set {.code force = TRUE} to upgrade)."
      )
      return(invisible(TRUE))
    }
    cli::cli_abort(
      c(
        "CmdStan setup required.",
        "x" = "Cannot install in non-interactive session.",
        "i" = "Run interactively or set {.code force = TRUE}."
      )
    )
  }

  if (is_interactive_session() && !force) {
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

  cli::cli_process_start("Installing CmdStan (this takes time)...")

  cmdstanr::install_cmdstan(quiet = quiet, overwrite = TRUE, cores = cores)

  cli::cli_process_done()
}
# nocov end

#' Setup rstan
#'
#' Configures `rstan` to use available cores and write compiled models to disk.
#' Prefer `setup_interface()` for user-facing setup since it performs argument
#' validation and defaults; `setup_rstan()` assumes inputs are already checked.
#'
#' @param quiet Logical. If `TRUE`, suppresses status messages.
#' @param cores Integer. Number of cores to use.
#' @return Returns `NULL` invisibly.
#' @export
setup_rstan <- function(quiet, cores) {
  local_cli_quiet(quiet)
  options(mc.cores = cores)
  rstan::rstan_options(auto_write = TRUE)

  cli::cli_alert_info(
    "Configured {.pkg rstan}: set {.code options(mc.cores = {cores})} and {.code rstan::rstan_options(auto_write = TRUE)}"
  )
}

#' Setup brms
#'
#' Configures `brms` to use available cores and sets the backend.
#' Prefer `setup_interface()` for user-facing setup since it performs argument
#' validation and defaults; `setup_brms()` assumes inputs are already checked.
#'
#' @param quiet Logical. If `TRUE`, suppresses status messages.
#' @param brms_backend Character. The `brms` backend to configure. Must be one
#'   of `c("cmdstanr", "rstan")`.
#' @param cores Integer. Number of cores to use.
#' @return Returns `NULL` invisibly.
#' @export
setup_brms <- function(quiet, brms_backend, cores) {
  local_cli_quiet(quiet)
  options(mc.cores = cores)

  msg <- "Configured {.pkg brms}: set {.code options(mc.cores = {cores})}"
  brms_backend <- match.arg(brms_backend, c("cmdstanr", "rstan"))
  options(brms.backend = brms_backend)
  msg <- paste0(
    msg,
    " and {.code options(brms.backend = '",
    brms_backend,
    "')}"
  )

  cli::cli_alert_info(msg)
}

#' Setup rstanarm
#'
#' Configures `rstanarm` to use available cores.
#' Prefer `setup_interface()` for user-facing setup since it performs argument
#' validation and defaults; `setup_rstanarm()` assumes inputs are already checked.
#'
#' @param quiet Logical. If `TRUE`, suppresses status messages.
#' @param cores Integer. Number of cores to use.
#' @return Returns `NULL` invisibly.
#' @export
setup_rstanarm <- function(quiet, cores) {
  local_cli_quiet(quiet)
  options(mc.cores = cores)

  msg <- "Configured {.pkg rstanarm}: set {.code options(mc.cores = {cores})}"

  cli::cli_alert_info(msg)
}
