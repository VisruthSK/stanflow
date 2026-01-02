# stanflow: quick tour

`stanflow` is a lightweight metapackage that sets up a Stan-based
Bayesian workflow. It attaches a core toolkit for model analysis
(`posterior`, `loo`, `projpred`, `bayesplot`, `shinystan`) and helps you
install/configure Stan interfaces (`cmdstanr`, `rstan`, `brms`,
`rstanarm`).

## Attach `stanflow`

``` r
library(stanflow)
stan_logo()
#>            G08GLG80G           
#>         G80LLLLLLLLL08G        
#>     G08GLLLLLLLLLLLLLLLG80G    
#>  G80LLLLLLLLLLLLLLCG08@@@@@@8G 
#> 8LLLLLLLLLLLC8@@@@@@@@@@@@@@@@@
#> 8LLLLLLLLL8@@@@@@@@@@@@@@800GC8
#> 8LLLLLLLL8@@@@80GCLLLLLLLLLLLL8
#> 8LLLLLLLGtiiiitfLLLLLLLLLLLLLL8
#> 8LLLLLLLfiiiiiiiii1fLLLLLLLLLL8
#> 8LLLLLLLLLLLLftiiiiiifLLLLLLLL8
#> 8LLLLLLLLLLLLLLLLLLLGLLLLLLLLL8
#> 8LLLLLLLLLLLLLG0@@0CLLLLLLLLLL8
#> 8LLLLCG0880GCLLLLLLLLLLLLLLLLL8
#>  G80LLLLLLLLLLLLLLLLLLLLGCG08G 
#>     G08GLLLLLLLLLLLLCGCG80G    
#>         G80LLLLLLLCL08G        
#>            G08GLG80G
```

Startup messages list attached packages and any namespace clashes. You
can re-print that status later with
[`flow_check()`](https://visruthsk.github.io/stanflow/reference/flow_check.md),
or just the namespace conflicts with
[`stanflow_conflicts()`](https://visruthsk.github.io/stanflow/reference/stanflow_conflicts.md).

``` r
flow_check()
#> ── Attaching Stan processing packages ─────────────────── stanflow 0.0.0.9000 ──
#> ✔ bayesplot 1.15.0     ✔ projpred  2.10.0
#> ✔ loo       2.9.0      ✔ shinystan 2.7.0 
#> ✔ posterior 1.6.1      
#> ── Available Stan interfaces ────────────────────────────── setup_interface() ──
#> ✔ brms     2.23.0     • rstan    2.32.7
#> ✔ cmdstanr 0.9.0      • rstanarm 2.32.2
#> ── Conflicts ─────────────────────────────────────────── stanflow_conflicts() ──
#> ✖ brms::ar()      masks stats::ar()
#> ✖ brms::do_call() masks projpred::do_call()
#> ✖ brms::rhat()    masks posterior::rhat(), bayesplot::rhat()
#> ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
stanflow_conflicts()
#> ── Conflicts ─────────────────────────────────────────── stanflow_conflicts() ──
#> ✖ brms::ar()      masks stats::ar()
#> ✖ brms::do_call() masks projpred::do_call()
#> ✖ brms::rhat()    masks posterior::rhat(), bayesplot::rhat()
#> ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
```

## Choose interface backends

Use
[`setup_interface()`](https://visruthsk.github.io/stanflow/reference/setup_interface.md)
to install (if needed), attach, and configure interfaces. This example
prefers the `cmdstanr` backend for `brms`. Running this command will
attach both packages and ensure that `brms` calls rely on `cmdstanr` by
default.

``` r
setup_interface(
  interface = "brms",
  prefer_cmdstanr = TRUE,
  quiet = TRUE,
  force = TRUE # only required for non-interactive usage
)
#> * Latest CmdStan release is v2.37.0
#> * Installing CmdStan v2.37.0 in /home/runner/.cmdstan/cmdstan-2.37.0
#> * Downloading cmdstan-2.37.0.tar.gz from GitHub...
#> * Download complete
#> * Unpacking archive...
#> * Building CmdStan binaries...
#> ar: creating stan/lib/stan_math/lib/sundials_6.1.1/lib/libsundials_nvecserial.a
#> ar: creating stan/lib/stan_math/lib/sundials_6.1.1/lib/libsundials_cvodes.a
#> ar: creating stan/lib/stan_math/lib/sundials_6.1.1/lib/libsundials_idas.a
#> ar: creating stan/lib/stan_math/lib/sundials_6.1.1/lib/libsundials_kinsol.a
#> /home/runner/.cmdstan/cmdstan-2.37.0/stan/lib/stan_math/lib/tbb_2020.3/build/Makefile.tbb:28: CONFIG: cfg=release arch=intel64 compiler=gcc target=linux runtime=cc13.3.0_libc2.39_kernel6.11.0
#> In file included from ../tbb_2020.3/src/tbb/concurrent_hash_map.cpp:17:
#> ../tbb_2020.3/include/tbb/concurrent_hash_map.h:347:23: warning: ‘template<class _Category, class _Tp, class _Distance, class _Pointer, class _Reference> struct std::iterator’ is deprecated [-Wdeprecated-declarations]
#>   347 |         : public std::iterator<std::forward_iterator_tag,Value>
#>       |                       ^~~~~~~~
#> In file included from /usr/include/c++/13/bits/stl_construct.h:61,
#>                  from /usr/include/c++/13/bits/stl_tempbuf.h:61,
#>                  from /usr/include/c++/13/memory:66,
#>                  from ../tbb_2020.3/include/tbb/tbb_stddef.h:452,
#>                  from ../tbb_2020.3/include/tbb/concurrent_hash_map.h:23:
#> /usr/include/c++/13/bits/stl_iterator_base_types.h:127:34: note: declared here
#>   127 |     struct _GLIBCXX17_DEPRECATED iterator
#>       |                                  ^~~~~~~~
#> In file included from ../tbb_2020.3/src/tbb/concurrent_queue.cpp:22:
#> ../tbb_2020.3/include/tbb/internal/_concurrent_queue_impl.h:749:21: warning: ‘template<class _Category, class _Tp, class _Distance, class _Pointer, class _Reference> struct std::iterator’ is deprecated [-Wdeprecated-declarations]
#>   749 |         public std::iterator<std::forward_iterator_tag,Value> {
#>       |                     ^~~~~~~~
#> In file included from /usr/include/c++/13/bits/stl_construct.h:61,
#>                  from /usr/include/c++/13/bits/stl_tempbuf.h:61,
#>                  from /usr/include/c++/13/memory:66,
#>                  from ../tbb_2020.3/include/tbb/tbb_stddef.h:452,
#>                  from ../tbb_2020.3/src/tbb/concurrent_queue.cpp:17:
#> /usr/include/c++/13/bits/stl_iterator_base_types.h:127:34: note: declared here
#>   127 |     struct _GLIBCXX17_DEPRECATED iterator
#>       |                                  ^~~~~~~~
#> ../tbb_2020.3/include/tbb/internal/_concurrent_queue_impl.h:1013:21: warning: ‘template<class _Category, class _Tp, class _Distance, class _Pointer, class _Reference> struct std::iterator’ is deprecated [-Wdeprecated-declarations]
#>  1013 |         public std::iterator<std::forward_iterator_tag,Value> {
#>       |                     ^~~~~~~~
#> /usr/include/c++/13/bits/stl_iterator_base_types.h:127:34: note: declared here
#>   127 |     struct _GLIBCXX17_DEPRECATED iterator
#>       |                                  ^~~~~~~~
#> cc1plus: note: unrecognized command-line option ‘-Wno-unknown-warning-option’ may have been intended to silence earlier diagnostics
#> 
#> cc1plus: note: unrecognized command-line option ‘-Wno-unknown-warning-option’ may have been intended to silence earlier diagnostics
#> * Finished installing CmdStan to /home/runner/.cmdstan/cmdstan-2.37.0
#> CmdStan path set to: /home/runner/.cmdstan/cmdstan-2.37.0
flow_check()
#> ── Attaching Stan processing packages ─────────────────── stanflow 0.0.0.9000 ──
#> ✔ bayesplot 1.15.0     ✔ projpred  2.10.0
#> ✔ loo       2.9.0      ✔ shinystan 2.7.0 
#> ✔ posterior 1.6.1      
#> ── Available Stan interfaces ────────────────────────────── setup_interface() ──
#> ✔ brms     2.23.0     • rstan    2.32.7
#> ✔ cmdstanr 0.9.0      • rstanarm 2.32.2
#> ── Conflicts ─────────────────────────────────────────── stanflow_conflicts() ──
#> ✖ brms::ar()      masks stats::ar()
#> ✖ brms::do_call() masks projpred::do_call()
#> ✖ brms::rhat()    masks posterior::rhat(), bayesplot::rhat()
#> ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
```

If you prefer `RStan`, you could just load it alongside `brms`.

``` r
setup_interface(
  interface = c("rstan", "brms"),
  quiet = TRUE
)
```

## A tiny workflow

With the core packages attached, you can generate, summarise, and
visualise draws immediately. Here we create fake draws, compute a simple
`loo` object, and plot a quick histogram with `bayesplot`.

``` r
set.seed(0)
draws <- as_draws_df(
  matrix(rnorm(4000), ncol = 1, dimnames = list(NULL, "theta"))
)
summarise_draws(draws, mean, sd, rhat, ess_bulk)
#> # A tibble: 1 × 5
#>   variable    mean    sd  rhat ess_bulk
#>   <chr>      <dbl> <dbl> <dbl>    <dbl>
#> 1 theta    0.00750 0.990  1.00    4038.
mcmc_hist(draws, pars = "theta")
#> `stat_bin()` using `bins = 30`. Pick better value `binwidth`.
```

![](stanflow_files/figure-html/unnamed-chunk-6-1.png)

``` r

log_lik <- matrix(rnorm(4000 * 10, -1, 0.2), ncol = 10)
loo(log_lik)
#> 
#> Computed from 4000 by 10 log-likelihood matrix.
#> 
#>          Estimate  SE
#> elpd_loo    -10.2 0.0
#> p_loo         0.4 0.0
#> looic        20.4 0.0
#> ------
#> MCSE of elpd_loo is 0.0.
#> MCSE and ESS estimates assume independent draws (r_eff=1).
#> 
#> All Pareto k estimates are good (k < 0.7).
#> See help('pareto-k-diagnostic') for details.
```

## Keep the toolkit fresh

Check whether your Stan workflow packages are up to date (stable
releases by default, or dev builds with `dev = TRUE`). Set `recursive`
to check the full dependency closure.

``` r
stanflow_update(recursive = TRUE)
#> The following packages are out of date:
#> 
#> • cpp11 (0 -> 0.5.2)
#> • mgcv  (1.9.3 -> 1.9-4)
#> 
#> Start a clean R session then run:
#> install.packages(c("cpp11", "mgcv"), repos = c("https://community.r-multiverse.org", getOption("repos")))
```
