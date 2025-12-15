# Conflicts between stanflow and other packages

This function lists all the conflicts between packages in stanflow and
other loaded packages.

## Usage

``` r
stanflow_conflicts(only = NULL)
```

## Arguments

- only:

  Set this to a character vector to restrict to conflicts only with
  these packages.

## Details

There are several conflicts that are deliberately ignored: `diag`,
`drop`, `match`, and `%in%` from posterior.

## Examples

``` r
stanflow_conflicts()
#> ── Conflicts ─────────────────────────────────────────── stanflow_conflicts() ──
#> ✖ posterior::mad()  masks stats::mad()
#> ✖ posterior::rhat() masks bayesplot::rhat()
#> ✖ posterior::sd()   masks stats::sd()
#> ✖ posterior::var()  masks stats::var()
#> ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
```
