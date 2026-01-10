# Setup rstanarm

Configures `rstanarm` to use available cores. Prefer
[`setup_interface()`](https://visruthsk.github.io/stanflow/reference/setup_interface.md)
for user-facing setup since it performs argument validation and
defaults; `setup_rstanarm()` assumes inputs are already checked.

## Usage

``` r
setup_rstanarm(quiet, cores)
```

## Arguments

- quiet:

  Logical. If `TRUE`, suppresses status messages.

- cores:

  Integer. Number of cores to use.

## Value

Returns `NULL` invisibly.
