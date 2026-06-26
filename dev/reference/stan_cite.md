# Cite Stan packages in a project/files

`stan_cite()` generates the correct citations for Stan packages in a
directory or set of files. Quarto (.qmd) and R Markdown (.Rmd) documents
are scanned by extracting R code chunks directly from the source text by
default. Setting `use_knitr = TRUE` switches to
[`knitr::purl()`](https://rdrr.io/pkg/knitr/man/knit.html), which is
more accurate for knitr/quarto chunk handling but much slower.
`stan_cite()` uses some simple heuristics to guess which packages export
functions, and also attempts to map re-exports to their origin package.
Calls to [`library()`](https://rdrr.io/r/base/library.html),
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
  use_knitr = FALSE,
  quiet = getOption("stanflow.quiet", FALSE)
)
```

## Arguments

- path:

  A single project directory (searched recursively) or a vector of files
  (.R/.Rmd/.qmd).

- strict:

  If `TRUE` (default), only count unqualified function calls whose
  origin can be determined exactly from the static scan, including
  attachment-order tie-breaks when the winner is unambiguous from the
  file. Unresolved calls are warned about and omitted.

- format:

  One of "bibtex" or "bibentry", specifying the return format.

- skip_dirs:

  Defaults to directories listed in `scan_skip_dirs`. Character vector
  of directory names to skip when scanning a directory.

- ignore_unqualified_functions:

  Defaults to exports from base R packages listed in
  [`stdlib_funs()`](https://visruthsk.github.io/stanflow/dev/reference/internal_data.md).
  Character vector of function names to ignore when attributing
  (unqualified) calls to Stan packages. Calls like `rstan::plot()` will
  NOT be ignored even if `plot` is in `ignore_unqualified_functions`,
  since they are namespaced.

- use_knitr:

  Logical. If `TRUE`, parse `.Rmd` and `.qmd` files with
  [`knitr::purl()`](https://rdrr.io/pkg/knitr/man/knit.html). This is
  more accurate for knitr/quarto chunk extraction but much slower than
  the default in-house parser. Defaults to `FALSE`.

- quiet:

  Logical. If `TRUE`, suppresses status messages.

## Value

A BibTeX character vector or a bibentry object.

## Details

The parsing is handled by
[`scan_usage()`](https://visruthsk.github.io/stanflow/dev/reference/scan_usage.md);
`stan_cite()` owns the citation lookups.

## Examples

``` r
path <- tempfile(fileext = ".R")
writeLines(
  c(
    "# one messy analysis file",
    "library(posterior)",
    "requireNamespace(\"loo\")",
    "draws <- as_draws(list(mu = rnorm(10)))",
    "posterior::rhat(draws)",
    "loo::loo(matrix(1))"
  ),
  path
)

stan_cite(path, quiet = TRUE)
#> @Manual{loo,
#>   title = {Efficient Leave-One-Out Cross-Validation and WAIC for Bayesian
#> Models},
#>   author = {Aki Vehtari and Jonah Gabry and Måns Magnusson and Yuling Yao and Paul-Christian Bürkner and Topi Paananen and Andrew Gelman},
#>   year = {2025},
#>   note = {R package version 2.9.0, https://discourse.mc-stan.org},
#>   url = {https://mc-stan.org/loo/},
#> }
#> 
#> @Manual{posterior,
#>   title = {Tools for Working with Posterior Distributions},
#>   author = {Paul-Christian Bürkner and Jonah Gabry and Matthew Kay and Aki Vehtari},
#>   year = {2026},
#>   note = {R package version 1.7.0, https://discourse.mc-stan.org},
#>   url = {https://mc-stan.org/posterior/},
#> }
#> 
#> @Article{burkner-2026-posterior,
#>   title = {posterior: Tools for Working with Posterior Distributions in R},
#>   author = {Paul-Christian B\u00fcrkner and Jonah Gabry and Matthew Kay and Aki Vehtari},
#>   journal = {Journal of Open Source Software},
#>   year = {2026},
#>   volume = {11},
#>   number = {122},
#>   pages = {10526},
#>   doi = {10.21105/joss.10526},
#>   url = {https://doi.org/10.21105/joss.10526},
#>   publisher = {The Open Journal},
#>   encoding = {UTF-8},
#> }
#> 
#> @Article{vehtari-2021-rhat,
#>   title = {Rank-normalization, folding, and localization: An improved R-hat for assessing convergence of MCMC (with discussion)},
#>   author = {Aki Vehtari and Andrew Gelman and Daniel Simpson and Bob Carpenter and Paul-Christian B\u00fcrkner},
#>   journal = {Bayesian Analysis},
#>   year = {2021},
#>   volume = {16},
#>   number = {2},
#>   pages = {667--718},
#>   doi = {10.1214/20-BA1221},
#> }
#> 
#> @Manual{stanflow,
#>   title = {A Mildly Opinionated Stan Bayesian Workflow},
#>   author = {Visruth {Srimath Kandali}},
#>   year = {2026},
#>   note = {R package version 0.1.0.9000, https://discourse.mc-stan.org},
#>   url = {https://mc-stan.org/stanflow/},
#> }
#> 
#> @Article{vehtari-2017-loo,
#>   title = {Practical Bayesian model evaluation using leave-one-out cross-validation and WAIC},
#>   author = {Aki Vehtari and Andrew Gelman and Jonah Gabry},
#>   journal = {Statistics and Computing},
#>   year = {2017},
#>   volume = {27},
#>   number = {5},
#>   pages = {1413--1432},
#>   doi = {10.1007/s11222-016-9696-4},
#>   note = {arXiv preprint: https://arxiv.org/abs/1507.04544},
#> }
#> 
#> @Article{vehtari-2024-psis,
#>   title = {Pareto smoothed importance sampling},
#>   author = {Aki Vehtari and Daniel Simpson and Andrew Gelman and Yuling Yao and Jonah Gabry},
#>   journal = {Journal of Machine Learning Research},
#>   year = {2024},
#>   volume = {25},
#>   number = {72},
#>   pages = {1--58},
#>   url = {https://jmlr.org/papers/v25/19-556.html},
#> }
#> 
#> @Manual{,
#>   title = {R: A Language and Environment for Statistical Computing},
#>   author = {{R Core Team}},
#>   organization = {R Foundation for Statistical Computing},
#>   address = {Vienna, Austria},
#>   year = {2026},
#>   doi = {10.32614/R.manuals},
#>   url = {https://www.R-project.org/},
#> }
stan_cite(path, format = "bibentry", quiet = TRUE)
#> Vehtari A, Gabry J, Magnusson M, Yao Y, Bürkner P, Paananen T, Gelman A
#> (2025). _Efficient Leave-One-Out Cross-Validation and WAIC for Bayesian
#> Models_. R package version 2.9.0, https://discourse.mc-stan.org,
#> <https://mc-stan.org/loo/>.
#> 
#> Bürkner P, Gabry J, Kay M, Vehtari A (2026). _Tools for Working with
#> Posterior Distributions_. R package version 1.7.0,
#> https://discourse.mc-stan.org, <https://mc-stan.org/posterior/>.
#> 
#> B\u00fcrkner P, Gabry J, Kay M, Vehtari A (2026). “posterior: Tools for
#> Working with Posterior Distributions in R.” _Journal of Open Source
#> Software_, *11*(122), 10526. doi:10.21105/joss.10526
#> <https://doi.org/10.21105/joss.10526>.
#> <https://doi.org/10.21105/joss.10526>.
#> 
#> Vehtari A, Gelman A, Simpson D, Carpenter B, B\u00fcrkner P (2021).
#> “Rank-normalization, folding, and localization: An improved R-hat for
#> assessing convergence of MCMC (with discussion).” _Bayesian Analysis_,
#> *16*(2), 667-718. doi:10.1214/20-BA1221
#> <https://doi.org/10.1214/20-BA1221>.
#> 
#> Srimath Kandali V (2026). _A Mildly Opinionated Stan Bayesian
#> Workflow_. R package version 0.1.0.9000, https://discourse.mc-stan.org,
#> <https://mc-stan.org/stanflow/>.
#> 
#> Vehtari A, Gelman A, Gabry J (2017). “Practical Bayesian model
#> evaluation using leave-one-out cross-validation and WAIC.” _Statistics
#> and Computing_, *27*(5), 1413-1432. doi:10.1007/s11222-016-9696-4
#> <https://doi.org/10.1007/s11222-016-9696-4>. arXiv preprint:
#> https://arxiv.org/abs/1507.04544.
#> 
#> Vehtari A, Simpson D, Gelman A, Yao Y, Gabry J (2024). “Pareto smoothed
#> importance sampling.” _Journal of Machine Learning Research_, *25*(72),
#> 1-58. <https://jmlr.org/papers/v25/19-556.html>.
#> 
#> R Core Team (2026). _R: A Language and Environment for Statistical
#> Computing_. R Foundation for Statistical Computing, Vienna, Austria.
#> doi:10.32614/R.manuals <https://doi.org/10.32614/R.manuals>.
#> <https://www.R-project.org/>.
unlink(path)
```
