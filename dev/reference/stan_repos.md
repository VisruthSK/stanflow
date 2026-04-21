# Stan package repositories

Stan package repositories

## Usage

``` r
stan_repos(dev = FALSE)
```

## Arguments

- dev:

  Include the development r-universe repo–don't use this unless you need
  the latest commits.

## Value

Character vector of repository URLs.

## Examples

``` r
stan_repos()
#>                                                    Multiverse 
#>                          "https://community.r-multiverse.org" 
#>                                                          RSPM 
#> "https://packagemanager.posit.co/cran/__linux__/noble/latest" 
#>                                                          CRAN 
#>                                    "https://cran.rstudio.com" 
#>                                                               
#>                         "https://community.r-multiverse.org," 
#>                                                               
#>                             "https://stan-dev.r-universe.dev" 
stan_repos(dev = TRUE)
#>                                                 StanRUniverse 
#>                             "https://stan-dev.r-universe.dev" 
#>                                                          RSPM 
#> "https://packagemanager.posit.co/cran/__linux__/noble/latest" 
#>                                                          CRAN 
#>                                    "https://cran.rstudio.com" 
#>                                                               
#>                         "https://community.r-multiverse.org," 
#>                                                               
#>                             "https://stan-dev.r-universe.dev" 
```
