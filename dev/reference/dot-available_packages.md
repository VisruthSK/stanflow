# Fetch repository package metadata for update checks

Wraps
[`utils::available.packages()`](https://rdrr.io/r/utils/available.packages.html)
for `stanflow` update checks and converts repository access failures
into a package-specific error message.

## Usage

``` r
.available_packages(dev)
```

## Arguments

- dev:

  Logical. Whether to query the Stan development repository via
  [`stan_repos()`](https://visruthsk.github.io/stanflow/dev/reference/stan_repos.md).

## Value

A package database matrix from
[`utils::available.packages()`](https://rdrr.io/r/utils/available.packages.html).
