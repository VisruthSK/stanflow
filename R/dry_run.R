dry_runner <- function(dry_run = FALSE) {
  force(dry_run)

  function(msg, expr, code = NULL) {
    if (!dry_run) {
      force(expr)
      return(invisible(NULL))
    }

    msg <- cli::format_inline(msg, .envir = parent.frame())
    if (!is.null(code)) {
      msg <- paste0(msg, ": ", code)
    }
    cli::cli_alert_info("Would {msg}.")
    invisible(NULL)
  }
}

dry_code_attach <- function(pkg) {
  sprintf(
    paste0(
      "library(%s, lib.loc = if (%s %%in%% loadedNamespaces()) ",
      "dirname(getNamespaceInfo(%s, \"path\")), character.only = TRUE, ",
      "warn.conflicts = FALSE)"
    ),
    deparse1(pkg),
    deparse1(pkg),
    deparse1(pkg)
  )
}
dry_code_mc_cores <- \(cores) sprintf("options(mc.cores = %s)", deparse1(cores))
dry_code_install_package <- function(pkg, dev, quiet) {
  sprintf(
    "utils::install.packages(%s, repos = stanflow::stan_repos(%s), quiet = %s)",
    deparse1(pkg),
    deparse1(dev),
    deparse1(quiet)
  )
}
dry_code_rstan <- function(cores, rstan_auto_write) {
  sprintf(
    "%s; rstan::rstan_options(auto_write = %s)",
    dry_code_mc_cores(cores),
    deparse1(rstan_auto_write)
  )
}

dry_code_brms <- function(cores, brms_backend) {
  sprintf(
    "%s; options(brms.backend = %s)",
    dry_code_mc_cores(cores),
    deparse1(brms_backend)
  )
}
