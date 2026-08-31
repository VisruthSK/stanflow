# Setup brms

Configures `brms` to use available cores and sets the backend. Prefer
[`setup_interface()`](https://stanflow.visruth.com/reference/setup_interface.md)
for user-facing setup since it performs argument validation and
defaults; `setup_brms()` assumes inputs are already checked.

## Usage

``` r
setup_brms(quiet, brms_backend, cores, dry_run = FALSE)
```

## Arguments

- quiet:

  Logical. If `TRUE`, suppresses status messages. This cannot suppress
  cmdstan messages.

- brms_backend:

  Character. The `brms` backend to use. Defaults to
  `getOption("brms.backend", "cmdstanr")` and must be one of
  `c("cmdstanr", "rstan")`.

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
setup_brms(quiet = TRUE, brms_backend = "cmdstanr", cores = 2)
} # }
```
