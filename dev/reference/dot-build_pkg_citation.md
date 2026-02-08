# Cite Stan Packages

Build the appropriate citation for R packages, including papers needed
to cite the package. Equivalent to
[`.pkg_cite()`](https://visruthsk.github.io/stanflow/dev/reference/dot-pkg_cite.md)
for most packages.

## Usage

``` r
.build_pkg_citation(pkg)
```

## Arguments

- pkg:

  Stan package name as a character scalar.

## Value

Vector of bibentries to properly cite the provided Stan package

## Details

Bayesplot and Posterior have papers in addition to their "typical"
software citation that should be cited when using the package, which is
why this exists.
