# Ignored functions/directories used by scanner

Vector of functions to be ignored when parsing. Generated in
`data-raw/sysdata.R` from exports of base R packages.

Vector of directories skipped when recursively searching a project.
Generated in `data-raw/sysdata.R`.

## Usage

``` r
stdlib_funs()

scan_skip_dirs()
```

## Examples

``` r
head(stdlib_funs())
#> [1] "-"         "-.Date"    "-.POSIXt"  "!"         "!.hexmode" "!.octmode"
scan_skip_dirs()
#>  [1] "renv"            "packrat"         "rv"              ".Rcheck"        
#>  [5] "revdep"          "_site"           "_book"           "_bookdown_files"
#>  [9] "_freeze"         ".quarto"         ".quarto_cache"   ".knitr_cache"   
#> [13] "_cache"          ".cache"         
```
