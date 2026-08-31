# Setup cmdstanr and CmdStan

Checks the C++ toolchain, locates CmdStan, and installs or upgrades
CmdStan if needed. Prefer
[`setup_interface()`](https://stanflow.visruth.com/reference/setup_interface.md)
for user-facing setup since it performs argument validation and
defaults; `setup_cmdstanr()` assumes inputs are already checked.

## Usage

``` r
setup_cmdstanr(
  quiet,
  force,
  reinstall = FALSE,
  check_updates = FALSE,
  cores,
  dry_run = FALSE
)
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

- dry_run:

  Logical. If `TRUE`, previews mutating setup actions without
  installing, attaching, changing options, or prompting. Dry-run output
  is shown even when `quiet = TRUE`.

## Value

Returns `TRUE` invisibly when no install/upgrade is needed. Otherwise,
returns `NULL` invisibly after installation.

## Examples

``` r
if (FALSE) { # \dontrun{
setup_cmdstanr(
  quiet = TRUE,
  force = TRUE,
  reinstall = FALSE,
  check_updates = FALSE,
  cores = 2
)
} # }
```
