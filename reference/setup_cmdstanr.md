# Setup cmdstanr and CmdStan

Checks the C++ toolchain, locates CmdStan, and installs or upgrades
CmdStan if needed. Prefer
[`setup_interface()`](https://visruthsk.github.io/stanflow/reference/setup_interface.md)
for user-facing setup since it performs argument validation and
defaults; `setup_cmdstanr()` assumes inputs are already checked.

## Usage

``` r
setup_cmdstanr(quiet, force, reinstall = FALSE, check_updates = TRUE, cores)
```

## Arguments

- quiet:

  Logical. If `TRUE`, suppresses status messages. This cannot suppress
  cmdstan messages.

- force:

  Logical. If `TRUE`, allows installation in non-interactive sessions.

- reinstall:

  Logical. If `TRUE`, forces re-installation.

- check_updates:

  Logical. If `TRUE`, checks for CmdStan updates.

- cores:

  Integer. Number of cores to use. Defaults to `getOption("mc.cores")`.
  You must set `options(mc.cores = ...)` or pass `cores` explicitly.

## Value

Returns `TRUE` invisibly when no install/upgrade is needed. Otherwise,
returns `NULL` invisibly after installation.
