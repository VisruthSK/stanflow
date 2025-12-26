# List all stanflow dependencies

List all stanflow dependencies

## Usage

``` r
stanflow_deps(recursive = FALSE, dev = FALSE)
```

## Arguments

- recursive:

  If `TRUE`, will also list all dependencies.

- dev:

  If `FALSE` (default), checks for updates in the R-multiverse or CRAN
  (stable releases). If `TRUE`, checks the Stan R-universe (dev
  versions). This is only cogent for Stan packages, and cannot compare
  two dev versions.
