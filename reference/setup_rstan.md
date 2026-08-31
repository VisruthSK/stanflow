# Setup rstan

Configures `rstan` to use available cores and write compiled models to
disk. Prefer
[`setup_interface()`](https://stanflow.visruth.com/reference/setup_interface.md)
for user-facing setup since it performs argument validation and
defaults; `setup_rstan()` assumes inputs are already checked.

## Usage

``` r
setup_rstan(quiet, cores, rstan_auto_write, dry_run = FALSE)
```

## Arguments

- quiet:

  Logical. If `TRUE`, suppresses status messages. This cannot suppress
  cmdstan messages.

- cores:

  Integer. Number of cores to use. Defaults to `getOption("mc.cores")`.
  You must set `options(mc.cores = ...)` or pass `cores` explicitly.

- rstan_auto_write:

  Logical. If `TRUE` (default), sets
  `rstan::rstan_options(auto_write = TRUE)`

- dry_run:

  Logical. If `TRUE`, previews mutating setup actions without
  installing, attaching, changing options, or prompting. Dry-run output
  is shown even when `quiet = TRUE`.

## Value

Returns `NULL` invisibly.

## Examples

``` r
if (FALSE) { # \dontrun{
setup_rstan(quiet = TRUE, cores = 2, rstan_auto_write = TRUE)
} # }
```
