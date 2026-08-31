# Setup rstanarm

Configures `rstanarm` to use available cores. Prefer
[`setup_interface()`](https://stanflow.visruth.com/reference/setup_interface.md)
for user-facing setup since it performs argument validation and
defaults; `setup_rstanarm()` assumes inputs are already checked.

## Usage

``` r
setup_rstanarm(quiet, cores, dry_run = FALSE)
```

## Arguments

- quiet:

  Logical. If `TRUE`, suppresses status messages. This cannot suppress
  cmdstan messages.

- cores:

  Integer. Number of cores to use. Defaults to `getOption("mc.cores")`.
  You must set `options(mc.cores = ...)` or pass `cores` explicitly.

- dry_run:

  Logical. If `TRUE`, previews mutating setup actions without
  installing, attaching, changing options, or prompting. Dry-run output
  is shown even when `quiet = TRUE`.

## Value

Returns `NULL` invisibly.

## Examples

``` r
if (FALSE) { # \dontrun{
setup_rstanarm(quiet = TRUE, cores = 2)
} # }
```
