dry_runner <- function(dry_run = FALSE) {
  force(dry_run)

  function(msg, expr, code = NULL) {
    env <- parent.frame()
    if (!dry_run) {
      eval(substitute(expr), envir = env)
      return(invisible(NULL))
    }

    msg <- cli::format_inline(msg, .envir = env)
    if (!is.null(code)) {
      code <- cli::format_inline(code, .envir = env)
      msg <- paste0(msg, ": ", code)
    }
    cli::cli_alert_info("Would {msg}.")
    invisible(NULL)
  }
}

dry_code_attach <- function(pkg) {
  cli::format_inline('stanflow:::.same_library("{pkg}")')
}

dry_code_install_package <- function(pkg) {
  cli::format_inline(
    'utils::install.packages("{pkg}", repos = stanflow::stan_repos(dev), quiet = TRUE)'
  )
}

dry_code_mc_cores <- function(cores) {
  cli::format_inline("options(mc.cores = {cores})")
}

dry_code_rstan <- function(cores, rstan_auto_write) {
  cli::format_inline(
    "options(mc.cores = {cores}); rstan::rstan_options(auto_write = {rstan_auto_write})"
  )
}

dry_code_brms <- function(cores, brms_backend) {
  cli::format_inline(
    'options(mc.cores = {cores}); options(brms.backend = "{brms_backend}")'
  )
}

dry_code_package_vector <- function(pkgs) {
  paste0("c(", paste(vapply(pkgs, deparse, character(1)), collapse = ", "), ")")
}
