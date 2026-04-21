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
  quiet = getOption("stanflow.quiet", FALSE),
  force = FALSE,
  reinstall = FALSE,
  check_updates = FALSE,
  dev = FALSE,
  brms_backend = c("cmdstanr", "rstan"),
  rstan_auto_write = TRUE
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

  Logical. If `TRUE`, suppresses status messages. This cannot suppress
  cmdstan messages.

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

  Character. The `brms` backend to use. Defaults to
  `getOption("brms.backend", "cmdstanr")` and must be one of
  `c("cmdstanr", "rstan")`.

- rstan_auto_write:

  Logical. If `TRUE` (default), sets
  `rstan::rstan_options(auto_write = TRUE)`

## Value

Returns attached package names invisibly.

## Details

The setup functions are exported (e.g.,
[`setup_brms()`](https://visruthsk.github.io/stanflow/dev/reference/setup_brms.md))
for transparency. Each function has some side effects, mainly setting
`mc.cores`, see the function for specifics.

## Examples

``` r
if (FALSE) { # \dontrun{
options(mc.cores = 2)
setup_interface("cmdstanr", quiet = TRUE)
setup_interface(
  c("brms", "cmdstanr"),
  brms_backend = "cmdstanr",
  quiet = TRUE
)
} # }
```
