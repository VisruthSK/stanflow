# Setup rstanarm

Configures `rstanarm` to use available cores. Prefer
[`setup_interface()`](https://visruthsk.github.io/stanflow/dev/reference/setup_interface.md)
for user-facing setup since it performs argument validation and
defaults; `setup_rstanarm()` assumes inputs are already checked.

## Usage

``` r
setup_rstanarm(quiet, cores)
```

## Arguments

- quiet:

  Logical. If `TRUE`, suppresses status messages. This cannot suppress
  cmdstan messages.

- cores:

  Integer. Number of cores to use. Defaults to `getOption("mc.cores")`.
  You must set `options(mc.cores = ...)` or pass `cores` explicitly.

## Value

Returns `NULL` invisibly.
