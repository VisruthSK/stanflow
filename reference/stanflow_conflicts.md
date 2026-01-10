# Conflicts between stanflow and other packages

This function lists all the conflicts between packages in stanflow and
other loaded packages.

## Usage

``` r
stanflow_conflicts(only = NULL)
```

## Arguments

- only:

  Set this to a character vector to restrict to conflicts only between
  the provided packages and loaded stanflow packages.

## Details

There are several conflicts that are deliberately ignored: `diag`,
`drop`, `match`, `%in%`, `mad`, `sd`, and `var` from posterior.

## Examples

``` r
stanflow_conflicts()
#> ── Conflicts ─────────────────────────────────────────── stanflow_conflicts() ──
#> ✖ posterior::rhat() masks bayesplot::rhat()
#> ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
```
