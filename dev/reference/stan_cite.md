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
    "requireNamespace(\"brms\")",
    "use(\"cmdstanr\", c(\"cmdstan_model\", \"write_stan_json\"))",
    "draws <- as_draws(list(mu = rnorm(10)))",
    "posterior::rhat(draws)",
    "brms::mixture(0.4)",
    "cmdstanr::write_stan_json(list(N = 3), \"data.json\")"
  ),
  path
)

stan_cite(path, quiet = TRUE)
#> @Article{,
#>   title = {{brms}: An {R} Package for {Bayesian} Multilevel Models Using {Stan}},
#>   author = {Paul-Christian B\u00fcrkner},
#>   journal = {Journal of Statistical Software},
#>   year = {2017},
#>   volume = {80},
#>   number = {1},
#>   pages = {1--28},
#>   doi = {10.18637/jss.v080.i01},
#>   encoding = {UTF-8},
#> }
#> 
#> @Article{,
#>   title = {Advanced {Bayesian} Multilevel Modeling with the {R} Package {brms}},
#>   author = {Paul-Christian B\u00fcrkner},
#>   journal = {The R Journal},
#>   year = {2018},
#>   volume = {10},
#>   number = {1},
#>   pages = {395--411},
#>   doi = {10.32614/RJ-2018-017},
#>   encoding = {UTF-8},
#> }
#> 
#> @Article{,
#>   title = {Bayesian Item Response Modeling in {R} with {brms} and {Stan}},
#>   author = {Paul-Christian B\u00fcrkner},
#>   journal = {Journal of Statistical Software},
#>   year = {2021},
#>   volume = {100},
#>   number = {5},
#>   pages = {1--54},
#>   doi = {10.18637/jss.v100.i05},
#>   encoding = {UTF-8},
#> }
#> 
#> @Manual{cmdstanr,
#>   title = {R Interface to 'CmdStan'},
#>   author = {Jonah Gabry and Rok Češnovar and Andrew Johnson and Steve Bronder},
#>   year = {2025},
#>   note = {R package version 0.9.0, https://discourse.mc-stan.org},
#>   url = {https://mc-stan.org/cmdstanr/},
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
#> @Manual{,
#>   title = {R: A Language and Environment for Statistical Computing},
#>   author = {{R Core Team}},
#>   organization = {R Foundation for Statistical Computing},
#>   address = {Vienna, Austria},
#>   year = {2026},
#>   url = {https://www.R-project.org/},
#> }
stan_cite(path, format = "bibentry", quiet = TRUE)
#> B\u00fcrkner P (2017). “brms: An R Package for Bayesian Multilevel
#> Models Using Stan.” _Journal of Statistical Software_, *80*(1), 1-28.
#> doi:10.18637/jss.v080.i01 <https://doi.org/10.18637/jss.v080.i01>.
#> 
#> B\u00fcrkner P (2018). “Advanced Bayesian Multilevel Modeling with the
#> R Package brms.” _The R Journal_, *10*(1), 395-411.
#> doi:10.32614/RJ-2018-017 <https://doi.org/10.32614/RJ-2018-017>.
#> 
#> B\u00fcrkner P (2021). “Bayesian Item Response Modeling in R with brms
#> and Stan.” _Journal of Statistical Software_, *100*(5), 1-54.
#> doi:10.18637/jss.v100.i05 <https://doi.org/10.18637/jss.v100.i05>.
#> 
#> Gabry J, Češnovar R, Johnson A, Bronder S (2025). _R Interface to
#> 'CmdStan'_. R package version 0.9.0, https://discourse.mc-stan.org,
#> <https://mc-stan.org/cmdstanr/>.
#> 
#> Bürkner P, Gabry J, Kay M, Vehtari A (2026). _Tools for Working with
#> Posterior Distributions_. R package version 1.7.0,
#> https://discourse.mc-stan.org, <https://mc-stan.org/posterior/>.
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
#> R Core Team (2026). _R: A Language and Environment for Statistical
#> Computing_. R Foundation for Statistical Computing, Vienna, Austria.
#> <https://www.R-project.org/>.
unlink(path)
```
