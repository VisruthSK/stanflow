# Cite Stan packages in a project/files

`stan_cite()` generates the correct citations for Stan packages in a
directory or set of files. The `{knitr}` package is required to parse
Quarto (.qmd) or RMarkdown (.Rmd) documents. `stan_cite()` uses some
simple heuristics to guess which packages export functions, and also
attempts to map re-exports to their origin package. Calls to
[`library()`](https://rdrr.io/r/base/library.html),
[`require()`](https://rdrr.io/r/base/library.html),
[`requireNamespace()`](https://rdrr.io/r/base/ns-load.html), or
[`use()`](https://rdrr.io/r/base/use.html) are all recognized as
attaching a package.

## Usage

``` r
stan_cite(
  path = ".",
  strict = TRUE,
  format = c("bibtex", "bibentry"),
  skip_dirs = .scan_skip_dirs,
  ignore_unqualified_functions = .stdlib_funs,
  quiet = getOption("stanflow.quiet", FALSE)
)
```

## Arguments

- path:

  A single project directory (searched recursively) or a vector of files
  (.R/.Rmd/.qmd).

- strict:

  If `TRUE` (default), only count unqualified function calls that
  resolve to a single Stan package.

- format:

  One of "bibtex" or "bibentry", specifying the return format.

- skip_dirs:

  Defaults to directories listed in `scan_skip_dirs`. Character vector
  of directory names to skip when scanning a directory.

- ignore_unqualified_functions:

  Defaults to exports from base R packages listed in
  [`stdlib_funs()`](https://visruthsk.github.io/stanflow/reference/internal_data.md).
  Character vector of function names to ignore when attributing
  (unqualified) calls to Stan packages. Calls like `rstan::plot()` will
  NOT be ignored even if `plot` is in `ignore_unqualified_functions`,
  since they are namespaced.

- quiet:

  Logical. If `TRUE`, suppresses status messages.

## Value

A BibTeX character vector or a bibentry object.

## Details

The parsing is handled by
[`scan_usage()`](https://visruthsk.github.io/stanflow/reference/scan_usage.md);
`stan_cite()` owns the citation lookups.
