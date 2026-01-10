# Setup and Load Stan Interfaces

This function ensures specific Stan interfaces are installed,
configured, and loaded. It handles package installation (from
R-multiverse/CRAN (stable) or Stan universe (dev)) and performs
necessary one-time setup (like installing CmdStan).

## Usage

``` r
setup_interface(
  interface = c("brms", "cmdstanr", "rstan", "rstanarm"),
  cores = getOption("mc.cores"),
  quiet = TRUE,
  force = FALSE,
  reinstall = FALSE,
  check_updates = FALSE,
  dev = FALSE,
  brms_backend = c("cmdstanr", "rstan")
)
```

## Arguments

- interface:

  A character vector. Select at least one of: "brms", "cmdstanr",
  "rstan", "rstanarm".

- cores:

  Integer. Number of cores to use. Defaults to `getOption("mc.cores")`.
  You must set `options(mc.cores = ...)` or pass `cores` explicitly.

- quiet:

  Logical. If `TRUE`, suppresses status messages.

- force:

  Logical. If `TRUE`, allows installation in non-interactive sessions.

- reinstall:

  Logical. If `TRUE`, forces re-installation.

- check_updates:

  Logical. If `TRUE`, checks for CmdStan updates.

- dev:

  Logical. If `FALSE` (default), installs stable releases from
  R-multiverse or CRAN. If `TRUE`, installs development versions from
  Stan R-universe.

- brms_backend:

  Character. The `brms` backend to use Defaults to
  `getOption("brms.backend", "cmdstanr")` and must be one of
  `c("cmdstanr", "rstan")`.

## Value

Returns `NULL` invisibly.
