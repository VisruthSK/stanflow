# Print stanflow status and conflicts

Print a consolidated status report showing attached packages, available
interfaces, and any conflicts.

## Usage

``` r
flow_check(check_updates = FALSE, only = NULL)
```

## Arguments

- check_updates:

  If `TRUE`, checks for stable updates to stanflow packages.

- only:

  Set this to a character vector to restrict to conflicts only between
  the provided packages and loaded stanflow packages.

## Value

Invisibly returns the character vector that was printed.

## Examples

``` r
flow_check()
#> ── Attaching Stan processing packages ──────────────────────── stanflow 0.1.0 ──
#> ✔ bayesplot 1.15.0     ✔ projpred  2.10.0
#> ✔ loo       2.9.0      ✔ shinystan 2.7.0 
#> ✔ posterior 1.6.1      
#> ── Available Stan interfaces ────────────────────────────── setup_interface() ──
#> • brms     2.23.0     • rstan    2.32.7
#> • cmdstanr 0.9.0      • rstanarm 2.32.2
#> ── Conflicts ─────────────────────────────────────────── stanflow_conflicts() ──
#> ✖ posterior::rhat() masks bayesplot::rhat()
#> ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
```
