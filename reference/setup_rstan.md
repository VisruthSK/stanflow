# Setup rstan

Configures `rstan` to use available cores and write compiled models to
disk. Prefer
[`setup_interface()`](https://visruthsk.github.io/stanflow/reference/setup_interface.md)
for user-facing setup since it performs argument validation and
defaults; `setup_rstan()` assumes inputs are already checked.

## Usage

``` r
setup_rstan(quiet, cores)
```

## Arguments

- quiet:

  Logical. If `TRUE`, suppresses status messages.

- cores:

  Integer. Number of cores to use.

## Value

Returns `NULL` invisibly.
