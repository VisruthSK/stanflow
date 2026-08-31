# Parse stanflow dependencies from DESCRIPTION

Reads the `stanflow` package metadata and returns dependency names from
`Depends`, `Imports`, and `Suggests`, dropping version constraints and
the `R` dependency.

When `recursive = TRUE`, transitive dependencies are resolved with
[`tools::package_dependencies()`](https://rdrr.io/r/tools/package_dependencies.html)
using the supplied package database.

## Usage

``` r
.description_deps(recursive, db)
```

## Arguments

- recursive:

  Logical. Whether to include transitive dependencies.

- db:

  A package database suitable for
  [`tools::package_dependencies()`](https://rdrr.io/r/tools/package_dependencies.html),
  usually from
  [`utils::available.packages()`](https://rdrr.io/r/utils/available.packages.html)
  or
  [`utils::installed.packages()`](https://rdrr.io/r/utils/installed.packages.html).

## Value

A character vector of package names.
