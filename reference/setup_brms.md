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

  Character. The `brms` backend to use. Defaults to
  `getOption("brms.backend", "cmdstanr")` and must be one of
  `c("cmdstanr", "rstan")`.

- cores:

  Integer. Number of cores to use. Defaults to `getOption("mc.cores")`.
  You must set `options(mc.cores = ...)` or pass `cores` explicitly.

## Value

Returns `NULL` invisibly.
