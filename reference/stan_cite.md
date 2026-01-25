# Collect citations

Unqualified function calls are only attributed when a Stan package is
attached via [`library()`](https://rdrr.io/r/base/library.html) or
[`require()`](https://rdrr.io/r/base/library.html) in the same file.
Known reexports are remapped to their origin packages; missing mappings
fall back to the resolved package.

## Usage

``` r
stan_cite(
  path = ".",
  ignore_unqualified_functions = .stdlib_funs,
  strict = TRUE,
  skip_dirs = .scan_skip_dirs,
  format = c("bibtex", "bibentry")
)
```

## Arguments

- path:

  A single project directory (searched recursively) or a vector of files
  (.R/.Rmd/.Qmd).

- ignore_unqualified_functions:

  Character vector of function names to ignore when attributing
  (unqualified) calls to Stan packages. Defaults to exports from base R
  packages listed in
  [`stdlib_funs()`](https://visruthsk.github.io/stanflow/reference/stdlib_funs.md).
  Calls like `rstan::plot()` will NOT be ignored even if `plot` is in
  `ignore_unqualified_functions`.

- strict:

  If `TRUE`, only count unqualified function calls that resolve to a
  single Stan package.

- skip_dirs:

  Character vector of directory names to skip when scanning a directory.

- format:

  One of "bibtex" or "bibentry".

## Value

A BibTeX character vector or a bibentry object.
