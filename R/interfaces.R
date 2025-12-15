#' Setup and Load Stan Interfaces
#'
#' This function ensures specific Stan interfaces are installed, configured,
#' and loaded. It handles package installation (from the R-multiverse or Stan
#' universe) and performs necessary one-time setup (like installing CmdStan).
#'
#' @param interface A character vector. Options: "brms", "cmdstanr", "rstan", "rstanarm".
#' @param dev Logical. If \code{FALSE} (default), installs stable releases from
#'   R-multiverse. If \code{TRUE}, installs development versions from Stan R-universe.
#' @param prefer_cmdstanr Logical. If \code{TRUE}, configures \code{brms} and
#'   \code{rstanarm} to use the 'cmdstanr' backend instead of the default 'rstan'.
#' @param quiet Logical. If \code{TRUE}, suppresses status messages.
#' @param force Logical. If \code{TRUE}, forces re-installation/setup. Required
#'   for installation in non-interactive sessions.
#' @return Invisible list of attached environments.
#' @export
setup_interface <- function(
  interface = c("brms", "cmdstanr", "rstan", "rstanarm"),
  dev = FALSE,
  prefer_cmdstanr = FALSE,
  quiet = FALSE,
  force = FALSE
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

  envs <- lapply(interface, function(pkg) {
    if (!is_installed(pkg) || force) {
      install_backend_package(pkg, dev, quiet, force)
    }

    if (!quiet) {
      cli::cli_alert_info("Loading {.pkg {pkg}}...")
    }
    suppressPackageStartupMessages(same_library(pkg))

    switch(
      pkg,
      "cmdstanr" = setup_cmdstanr(quiet, force),
      "rstan" = setup_rstan(quiet),
      "brms" = setup_brms(quiet, prefer_cmdstanr),
      "rstanarm" = setup_rstanarm(quiet, prefer_cmdstanr)
    )

    as.environment(paste0("package:", pkg))
  })

  if (!quiet) {
    cli::cli_alert_success("Setup complete.")
  }
  invisible(envs)
}

# ------------------------------------------------------------------------------
# Internal Setup Logic
# ------------------------------------------------------------------------------

install_backend_package <- function(pkg, dev, quiet, force) {
  if (!quiet) {
    cli::cli_alert_warning("Package {.pkg {pkg}} is not installed.")
  }

  # GUARD: Non-interactive Bail Out
  if (!interactive() && !force) {
    cli::cli_abort(c(
      "Package {.pkg {pkg}} is missing.",
      "x" = "Cannot install automatically in a non-interactive session.",
      "i" = "Run interactively or set {.code force = TRUE} to allow automated installation."
    ))
  }

  # GUARD: Interactive Permission
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
  cmdstan_ready <- FALSE
  tryCatch(
    {
      path <- cmdstanr::cmdstan_path()
      version <- cmdstanr::cmdstan_version()
      if (!quiet) {
        cli::cli_alert_info("Found CmdStan v{version} at {.path {path}}")
      }
      cmdstan_ready <- TRUE
    },
    error = function(e) {}
  )

  if (!cmdstan_ready || force) {
    if (!quiet) {
      cli::cli_alert_warning(
        "CmdStan binaries are missing or requested to be reinstalled."
      )
    }

    if (!interactive() && !force) {
      cli::cli_abort(c(
        "CmdStan binaries are missing.",
        "x" = "Cannot download and compile CmdStan in a non-interactive session.",
        "i" = "Run interactively or set {.code force = TRUE}."
      ))
    }

    if (interactive() && !force) {
      do_it <- utils::menu(
        c("Yes", "No"),
        title = "Download and compile CmdStan now? (Required for cmdstanr)"
      )
      if (do_it != 1) {
        cli::cli_abort("CmdStan setup aborted.")
      }
    }

    if (!quiet) {
      cli::cli_process_start("Installing CmdStan (this takes time)...")
    }
    cmdstanr::install_cmdstan(quiet = quiet, overwrite = force)
    if (!quiet) cli::cli_process_done()
  }
}

setup_rstan <- function(quiet) {
  cores <- parallel::detectCores()
  options(mc.cores = cores)
  rstan::rstan_options(auto_write = TRUE)

  if (!quiet) {
    cli::cli_alert_info(
      "Configured {.pkg rstan}: set {.code options(mc.cores = {cores})} and {.code rstan::rstan_options(auto_write = TRUE)}"
    )
  }
}

setup_brms <- function(quiet, prefer_cmdstanr) {
  cores <- parallel::detectCores()
  options(mc.cores = cores)

  backend_msg <- ""
  if (prefer_cmdstanr) {
    options(brms.backend = "cmdstanr")
    backend_msg <- " and {.code options(brms.backend = 'cmdstanr')}"
  }

  if (!quiet) {
    cli::cli_alert_info(
      "Configured {.pkg brms}: set {.code options(mc.cores = {cores})}{backend_msg}"
    )
  }
}

setup_rstanarm <- function(quiet, prefer_cmdstanr) {
  cores <- parallel::detectCores()
  options(mc.cores = cores)

  msg <- glue::glue(
    "Configured {.pkg rstanarm}: set {.code options(mc.cores = {cores})}"
  )

  if (prefer_cmdstanr) {
    msg <- paste0(msg, " (Note: {.arg prefer_cmdstanr} ignored)")
  }

  if (!quiet) {
    cli::cli_alert_info(msg)
  }
}
