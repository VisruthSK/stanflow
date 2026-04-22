# Find used functions and packages

This function is primarily exported for developers. The scanner itself
is generic and requires an explicit package universe;
[`stan_cite()`](https://visruthsk.github.io/stanflow/dev/reference/stan_cite.md)
is the Stan-configured entry point. The scanning is wholly static (AST
parsing), so there are a number of restrictions on what calls are
recognized: calls to [`library()`](https://rdrr.io/r/base/library.html),
[`require()`](https://rdrr.io/r/base/library.html),
[`requireNamespace()`](https://rdrr.io/r/base/ns-load.html), or
[`use()`](https://rdrr.io/r/base/use.html) are all recognized as
attaching a package.

## Usage

``` r
scan_usage(
  path = ".",
  allowed_packages,
  export_index,
  origin_map,
  ignore_unqualified_functions = .stdlib_funs,
  strict = FALSE,
  skip_dirs = .scan_skip_dirs,
  metapackages = NULL,
  use_knitr = FALSE,
  quiet = getOption("stanflow.quiet", FALSE)
)
```

## Arguments

- path:

  A single project directory (searched recursively) or a vector of files
  (.R/.Rmd/.qmd).

- allowed_packages:

  Character vector of package namespaces to attribute calls to.

- export_index:

  Named list mapping function names to packages.

- origin_map:

  Named character vector mapping `pkg::fun` keys to the origin package.

- ignore_unqualified_functions:

  Defaults to exports from base R packages listed in
  [`stdlib_funs()`](https://visruthsk.github.io/stanflow/dev/reference/internal_data.md).
  Character vector of function names to ignore when attributing
  (unqualified) calls to Stan packages. Calls like `rstan::plot()` will
  NOT be ignored even if `plot` is in `ignore_unqualified_functions`,
  since they are namespaced.

- strict:

  If `TRUE` (default), only count unqualified function calls whose
  origin can be determined exactly from the static scan, including
  attachment-order tie-breaks when the winner is unambiguous from the
  file. Unresolved calls are warned about and omitted.

- skip_dirs:

  Character vector of directory names to skip when scanning a directory.
  Defaults to `.scan_skip_dirs`.

- metapackages:

  Named list mapping attached package names to additional packages that
  should be treated as co-attached for unqualified resolution. Defaults
  to `NULL`.

- use_knitr:

  Logical. If `TRUE`, parse `.Rmd` and `.qmd` files with
  [`knitr::purl()`](https://rdrr.io/pkg/knitr/man/knit.html). This is
  more accurate for knitr/quarto chunk extraction but much slower than
  the default in-house parser. Defaults to `FALSE`.

- quiet:

  Logical. If `TRUE`, suppresses status messages.

## Value

A list of packages, resolved functions, and ambiguous function calls.

## Details

Explicit package references from
[`library()`](https://rdrr.io/r/base/library.html),
[`require()`](https://rdrr.io/r/base/library.html),
[`requireNamespace()`](https://rdrr.io/r/base/ns-load.html),
[`use()`](https://rdrr.io/r/base/use.html), and `pkg::fun` are only
recorded when their package is included in `allowed_packages`.
Unqualified function calls are only attributed when a package is
attached via [`library()`](https://rdrr.io/r/base/library.html) or
[`require()`](https://rdrr.io/r/base/library.html) in the same file and
`allowed_packages`, `export_index`, and `origin_map` describe how to
resolve them. Attaching a metapackage can also be treated as attaching
additional packages when `metapackages` is supplied. When multiple
attached packages export the same unqualified function, attachment order
is respected: the most recently attached matching package whose
attachment appears before the call is treated as the winner. Known
reexports are remapped to their origin packages; missing mappings fall
back to the resolved package.

## Examples

``` r
path <- tempfile(fileext = ".R")
writeLines(
  c(
    "# one messy analysis file",
    "library(stats)",
    "requireNamespace(\"utils\")",
    "filter(1:10, rep(1, 3))",
    "utils::head(letters)"
  ),
  path
)
scan_usage(
  path,
  allowed_packages = c("stats", "utils"),
  export_index = list(filter = "stats"),
  origin_map = c("stats::filter" = "stats"),
  ignore_unqualified_functions = character(),
  quiet = TRUE
)
#> $packages
#> [1] "stats" "utils"
#> 
#> $functions
#> [1] "stats::filter" "utils::head"  
#> 
#> $ambiguous
#> character(0)
#> 
#> attr(,"class")
#> [1] "scan_usage"
unlink(path)
```
