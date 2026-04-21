# Conflicts between stanflow and other packages

List conflicts between stanflow packages and other attached packages.

## Usage

``` r
stanflow_conflicts(only = NULL)

# S3 method for class 'stanflow_conflicts'
print(x, ...)
```

## Arguments

- only:

  Defaults to `NULL`. Set this to a character vector to restrict to
  conflicts only between the provided packages and loaded stanflow
  packages.

- x:

  A `stanflow_conflicts` object, usually from `stanflow_conflicts()`.

- ...:

  Unused. Included for consistency with
  [`base::print()`](https://rdrr.io/r/base/print.html).

## Value

Invisibly returns `x`.

## Details

There are several conflicts that are deliberately ignored: `diag`,
`drop`, `match`, `\%in\%`, `mad`, `sd`, and `var` from posterior.

Adapted from `tidyverse::tidyverse_conflicts()` for stanflow's package
set.

## Examples

``` r
stanflow_conflicts()
#> ── Conflicts ─────────────────────────────────────────── stanflow_conflicts() ──
#> ✖ posterior::gpdfit() masks loo::gpdfit()
#> ✖ posterior::rhat()   masks bayesplot::rhat()
#> ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
stanflow_conflicts(c("base"))
#> ── Conflicts ─────────────────────────────────────────── stanflow_conflicts() ──
#> ✖ posterior::gpdfit() masks loo::gpdfit()
#> ✖ posterior::rhat()   masks bayesplot::rhat()
#> ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
conflicts <- stanflow_conflicts()
print(conflicts)
#> ── Conflicts ─────────────────────────────────────────── stanflow_conflicts() ──
#> ✖ posterior::gpdfit() masks loo::gpdfit()
#> ✖ posterior::rhat()   masks bayesplot::rhat()
#> ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
```
