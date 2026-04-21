# Build Stan package bibentry citations

Helper function to build standardized package citations. This mostly
matches how each Stan R package wants to be cited. Some Stan packages
have additional paper citations generated in `data-raw/sysdata.R` and
stored in `.stan_citation_pkg_extras`.

## Usage

``` r
.pkg_cite(pkg)
```

## Arguments

- pkg:

  Stan package name as a character scalar.

## Value

Vector of bibentries for citing that package.
