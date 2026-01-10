# Setup brms

Configures `brms` to use available cores and sets the backend. Prefer
[`setup_interface()`](https://visruthsk.github.io/stanflow/reference/setup_interface.md)
for user-facing setup since it performs argument validation and
defaults; `setup_brms()` assumes inputs are already checked.

## Usage

``` r
setup_brms(quiet, brms_backend, cores)
```

## Arguments

- quiet:

  Logical. If `TRUE`, suppresses status messages.

- brms_backend:

  Character. The `brms` backend to configure. Must be one of
  `c("cmdstanr", "rstan")`.

- cores:

  Integer. Number of cores to use.

## Value

Returns `NULL` invisibly.
