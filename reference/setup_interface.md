# Setup and Load Stan Interfaces

This function ensures specific Stan interfaces are installed,
configured, and loaded. It handles package installation (from
R-multiverse/CRAN (stable) or Stan universe (dev)) and performs
necessary one-time setup (like installing CmdStan).

## Usage

``` r
setup_interface(
  interface = c("brms", "cmdstanr", "rstan", "rstanarm"),
  dev = FALSE,
  prefer_cmdstanr = FALSE,
  quiet = FALSE,
  force = FALSE
)
```

## Arguments

- interface:

  A character vector. Select at least one of: "brms", "cmdstanr",
  "rstan", "rstanarm".

- dev:

  Logical. If `FALSE` (default), installs stable releases from
  R-multiverse or CRAN. If `TRUE`, installs development versions from
  Stan R-universe.

- prefer_cmdstanr:

  Logical. If `TRUE`, configures `brms` to use the `cmdstanr` backend
  instead of the default `rstan`.

- quiet:

  Logical. If `TRUE`, suppresses status messages.

- force:

  Logical. If `TRUE`, forces re-installation/setup. Required for
  installation in non-interactive sessions.

## Value

Returns `NULL` invisibly.
