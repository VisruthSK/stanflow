#' Setup and Load Stan Interfaces
#'
#' This function ensures specific Stan interfaces are installed, configured,
#' and loaded. It handles package installation (from R-multiverse/CRAN (stable) or Stan
#' universe (dev)) and performs necessary one-time setup (like installing CmdStan).
#'
#' The setup functions are exported (e.g., `setup_brms()`) for transparency.
#' Each function has some side effects, mainly setting `mc.cores`, see the function
#' for specifics.
#'
#' @param interface A character vector. Select at least one of: "brms", "cmdstanr", "rstan", "rstanarm".
#' @param dev Logical. If `FALSE` (default), installs stable releases from
#'   R-multiverse or CRAN. If `TRUE`, installs development versions from Stan R-universe.
#' @param brms_backend Character. The `brms` backend to use. Defaults to
#'   `getOption("brms.backend", "cmdstanr")` and must be one of
#'   `c("cmdstanr", "rstan")`.
#' @param cores Integer. Number of cores to use. Defaults to
#'   `getOption("mc.cores")`. You must set `options(mc.cores = ...)` or pass
#'   `cores` explicitly.
#' @param quiet Logical. If `TRUE`, suppresses status messages. This cannot suppress cmdstan messages.
#' @param force Logical. If `TRUE`, allows installation in non-interactive sessions.
#' @param reinstall Logical. If `TRUE`, forces re-installation.
#' @param check_updates Logical. If `TRUE`, checks for CmdStan updates.
#' @param rstan_auto_write Logical. If `TRUE` (default), sets `rstan::rstan_options(auto_write = TRUE)`
#' @param dry_run Logical. If `TRUE`, previews mutating setup actions without
#'   installing, attaching, changing options, or prompting. Dry-run output is
#'   shown even when `quiet = TRUE`.
#' @return Returns attached package names invisibly. With `dry_run = TRUE`, returns the package names that would be attached.
#' @export
#' @examples
#' \dontrun{
#' options(mc.cores = 2)
#' setup_interface("cmdstanr", quiet = TRUE)
#' setup_interface(
#'   c("brms", "cmdstanr"),
#'   brms_backend = "cmdstanr",
#'   quiet = TRUE
#' )
#' }
setup_interface <- function(
  interface = c("brms", "cmdstanr", "rstan", "rstanarm"),
  cores = getOption("mc.cores"),
  quiet = getOption("stanflow.quiet", FALSE),
  force = FALSE,
  reinstall = FALSE,
  check_updates = FALSE,
  dev = FALSE,
  brms_backend = c("cmdstanr", "rstan"),
  rstan_auto_write = TRUE,
  dry_run = FALSE
) {
  local_cli_quiet(quiet && !dry_run)

  if (missing(interface)) {
    cli::cli_abort(
      c(
        "{.arg interface} must be provided.",
        "x" = "No interface selection was provided.",
        "i" = "Set {.arg interface} to one or more of {.val brms}, {.val cmdstanr}, {.val rstan}, {.val rstanarm}."
      )
    )
  }
  interface <- match.arg(interface, several.ok = TRUE)

  if (missing(brms_backend)) {
    brms_backend <- getOption("brms.backend", "cmdstanr")
  }
  brms_backend <- match.arg(brms_backend, c("cmdstanr", "rstan"))

  if (is.null(cores)) {
    cli::cli_abort(
      c(
        "{.arg cores} must be provided.",
        "x" = "No default {._opt mc.cores} option is set.",
        "i" = "Set {.code options(mc.cores = ...)} or pass {.arg cores}."
      )
    )
  }

  if (
    brms_backend == "cmdstanr" &&
      "brms" %in% interface &&
      !"cmdstanr" %in% interface
  ) {
    cli::cli_alert_info(
      "Adding {.pkg cmdstanr} to setup because {.arg brms_backend = 'cmdstanr'}"
    )
    interface <- c(interface, "cmdstanr")
  }

  for (pkg in interface) {
    if (!is_installed(pkg) || reinstall) {
      install_backend_package(
        pkg,
        dev,
        quiet,
        force,
        reinstall,
        dry_run = dry_run
      )
    }

    switch(
      pkg,
      "cmdstanr" = setup_cmdstanr(
        quiet,
        force,
        reinstall,
        check_updates,
        cores,
        dry_run = dry_run
      ),
      "rstan" = setup_rstan(quiet, cores, rstan_auto_write, dry_run),
      "brms" = setup_brms(quiet, brms_backend, cores, dry_run),
      "rstanarm" = setup_rstanarm(quiet, cores, dry_run)
    )

    attach_backend_package(pkg, dry_run)
  }

  attached_pkgs <- unique(interface)
  if (dry_run) {
    return(invisible(attached_pkgs))
  }

  attached_pkgs_cli <- paste0("{.pkg ", attached_pkgs, "}", collapse = ", ")
  pkg_count <- cli::qty(length(attached_pkgs))
  pkg_phrase <- cli::pluralize("{pkg_count}{?is/are}")
  cli::cli_alert_success(
    cli::format_inline(
      "Setup complete. {attached_pkgs_cli} {pkg_phrase} attached; you do not need to run {.code library()}."
    )
  )

  invisible(attached_pkgs)
}

# nocov start
install_backend_package <- function(
  pkg,
  dev,
  quiet,
  force,
  reinstall,
  dry_run = FALSE
) {
  local_cli_quiet(quiet && !dry_run)
  run_side_effect <- dry_runner(dry_run)

  if (reinstall) {
    cli::cli_alert_warning(
      "Reinstalling {.pkg {pkg}} because {.code reinstall = TRUE}."
    )
  } else {
    cli::cli_alert_warning("Package {.pkg {pkg}} is not installed.")
  }

  if (!dry_run && !is_interactive_session() && !force) {
    cli::cli_abort(
      c(
        "Package {.pkg {pkg}} is missing.",
        "x" = "Cannot naively install automatically in a non-interactive session.",
        "i" = "Run interactively or set {.code force = TRUE} to allow automated installation."
      )
    )
  }

  if (!dry_run && is_interactive_session() && !force) {
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

  run_side_effect(
    "install {.pkg {pkg}}",
    {
      cli::cli_progress_step("Installing {.pkg {pkg}}...")
      utils::install.packages(pkg, repos = stan_repos(dev), quiet = quiet)
      cli::cli_progress_done()
    },
    code = dry_code_install_package(pkg, dev, quiet)
  )
}

attach_backend_package <- function(pkg, dry_run = FALSE) {
  run_side_effect <- dry_runner(dry_run)

  run_side_effect(
    "attach {.pkg {pkg}}",
    {
      cli::cli_alert_info("Attaching {.pkg {pkg}}...")
      suppressPackageStartupMessages(.same_library(pkg))
    },
    code = dry_code_attach(pkg)
  )
}

#' Setup cmdstanr and CmdStan
#'
#' Checks the C++ toolchain, locates CmdStan, and installs or upgrades
#' CmdStan if needed. Prefer `setup_interface()` for user-facing setup since
#' it performs argument validation and defaults; `setup_cmdstanr()` assumes
#' inputs are already checked.
#'
#' @inheritParams setup_interface
#' @return Returns `TRUE` invisibly when no install/upgrade is needed.
#'   Otherwise, returns `NULL` invisibly after installation.
#' @export
#' @examples
#' \dontrun{
#' setup_cmdstanr(
#'   quiet = TRUE,
#'   force = TRUE,
#'   reinstall = FALSE,
#'   check_updates = FALSE,
#'   cores = 2
#' )
#' }
setup_cmdstanr <- function(
  quiet,
  force,
  reinstall = FALSE,
  check_updates = FALSE,
  cores,
  dry_run = FALSE
) {
  local_cli_quiet(quiet && !dry_run)
  run_side_effect <- dry_runner(dry_run)

  toolchain_ok <- tryCatch(
    {
      run_side_effect(
        "check the CmdStan toolchain",
        {
          cmdstanr::check_cmdstan_toolchain(quiet = quiet)
        },
        code = sprintf(
          "cmdstanr::check_cmdstan_toolchain(quiet = %s)",
          deparse1(quiet)
        )
      )
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
        "i" = "Re-run {.code cmdstanr::check_cmdstan_toolchain(quiet = FALSE)} for detailed diagnostics."
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
    error = \(e) NULL
  )

  latest_ver <- NULL
  if (cmdstan_ready && check_updates) {
    if (dry_run) {
      cli::cli_alert_info(
        "Would install or upgrade CmdStan if a newer release is found."
      )
    } else {
      latest_ver <- try_fetch_latest_cmdstan_version()
      if (is.null(latest_ver)) {
        cli::cli_alert_warning(
          "Could not check for CmdStan updates; using installed CmdStan v{local_ver}."
        )
      }
    }
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
    set_mc_cores(run_side_effect, cores, "cmdstanr")
    return(invisible(TRUE))
  }

  cli::cli_alert_warning(action_msg)

  if (!dry_run && !is_interactive_session() && !force) {
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

  if (!dry_run && is_interactive_session() && !force) {
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

  run_side_effect(
    "install or upgrade CmdStan",
    {
      cli::cli_process_start("Installing CmdStan (this can take some time)...")
      cmdstanr::install_cmdstan(quiet = quiet, overwrite = TRUE, cores = cores)
      cli::cli_process_done()
    },
    code = sprintf(
      "cmdstanr::install_cmdstan(quiet = %s, overwrite = TRUE, cores = %s)",
      deparse1(quiet),
      deparse1(cores)
    )
  )

  set_mc_cores(run_side_effect, cores, "cmdstanr")
  invisible(NULL)
}
# nocov end

try_fetch_latest_cmdstan_version <- function() {
  tryCatch(
    {
      raw_json <- suppressWarnings(
        readLines(
          "https://api.github.com/repos/stan-dev/cmdstan/releases/latest",
          warn = FALSE
        )
      )
      tag_line <- grep('"tag_name"\\s*:', raw_json, value = TRUE)
      if (!length(tag_line)) {
        return(NULL)
      }
      numeric_version(
        sub(
          '.*"tag_name":\\s*"v?([^"]+)".*',
          "\\1",
          tag_line
        )
      )
    },
    error = \(e) NULL
  )
}

#' Setup rstan
#'
#' Configures `rstan` to use available cores and write compiled models to disk.
#' Prefer `setup_interface()` for user-facing setup since it performs argument
#' validation and defaults; `setup_rstan()` assumes inputs are already checked.
#'
#' @inheritParams setup_interface
#' @return Returns `NULL` invisibly.
#' @export
#' @examples
#' \dontrun{
#' setup_rstan(quiet = TRUE, cores = 2, rstan_auto_write = TRUE)
#' }
setup_rstan <- function(
  quiet,
  cores,
  rstan_auto_write,
  dry_run = FALSE
) {
  local_cli_quiet(quiet && !dry_run)
  run_side_effect <- dry_runner(dry_run)

  run_side_effect(
    "configure {.pkg rstan}: set {.code options(mc.cores = {cores})} and {.code rstan::rstan_options(auto_write = {rstan_auto_write})}",
    {
      options(mc.cores = cores)
      rstan::rstan_options(auto_write = rstan_auto_write)
      cli::format_inline(
        "Configured {.pkg rstan}: set {.code options(mc.cores = {cores})} and {.code rstan::rstan_options(auto_write = {rstan_auto_write})}"
      ) |>
        cli::cli_alert_info()
    },
    code = dry_code_rstan(cores, rstan_auto_write)
  )
  invisible(NULL)
}

#' Setup brms
#'
#' Configures `brms` to use available cores and sets the backend.
#' Prefer `setup_interface()` for user-facing setup since it performs argument
#' validation and defaults; `setup_brms()` assumes inputs are already checked.
#'
#' @inheritParams setup_interface
#' @return Returns `NULL` invisibly.
#' @export
#' @examples
#' \dontrun{
#' setup_brms(quiet = TRUE, brms_backend = "cmdstanr", cores = 2)
#' }
setup_brms <- function(
  quiet,
  brms_backend,
  cores,
  dry_run = FALSE
) {
  local_cli_quiet(quiet && !dry_run)
  run_side_effect <- dry_runner(dry_run)
  brms_backend <- match.arg(brms_backend, c("cmdstanr", "rstan"))

  run_side_effect(
    "configure {.pkg brms}: set {.code options(mc.cores = {cores})} and {.code options(brms.backend = '{brms_backend}')}",
    {
      options(mc.cores = cores)
      options(brms.backend = brms_backend)
      cli::cli_alert_info(
        "Configured {.pkg brms}: set {.code options(mc.cores = {cores})} and {.code options(brms.backend = '{brms_backend}')}"
      )
    },
    code = dry_code_brms(cores, brms_backend)
  )
  invisible(NULL)
}

#' Setup rstanarm
#'
#' Configures `rstanarm` to use available cores.
#' Prefer `setup_interface()` for user-facing setup since it performs argument
#' validation and defaults; `setup_rstanarm()` assumes inputs are already checked.
#'
#' @inheritParams setup_interface
#' @return Returns `NULL` invisibly.
#' @export
#' @examples
#' \dontrun{
#' setup_rstanarm(quiet = TRUE, cores = 2)
#' }
setup_rstanarm <- function(
  quiet,
  cores,
  dry_run = FALSE
) {
  local_cli_quiet(quiet && !dry_run)
  run_side_effect <- dry_runner(dry_run)

  set_mc_cores(run_side_effect, cores, "rstanarm")
  invisible(NULL)
}

set_mc_cores <- function(run_side_effect, cores, pkg) {
  run_side_effect(
    "configure {.pkg {pkg}}: set {.code options(mc.cores = {cores})}",
    {
      options(mc.cores = cores)
      cli::format_inline(
        "Configured {.pkg {pkg}}: set {.code options(mc.cores = {cores})}"
      ) |>
        cli::cli_alert_info()
    },
    code = dry_code_mc_cores(cores)
  )
}
