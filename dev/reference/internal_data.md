# Ignored functions/directories used by scanner

Vector of functions to be ignored when parsing. Generated in
`data-raw/sysdata.R` from exports of base R packages.

Vector of directories skipped when recursively searching a project.
Generated in `data-raw/sysdata.R`.

## Usage

``` r
stdlib_funs(quiet = getOption("stanflow.quiet", FALSE))

scan_skip_dirs(quiet = getOption("stanflow.quiet", FALSE))
```
