# .extract_code errors on unsupported extensions

    Unsupported file extension: "txt".
    i Supported extensions are '.R', '.Rmd', and '.qmd'.

# scan_usage strict aborts on ambiguous unqualified calls

    Cannot reliably detect which packages some functions are from.
    x Ambiguous functions: `ess_bulk()`, `rhat()`
    i Please namespace them (`pkg::function()`) and rerun or set `strict = FALSE`.

# scan_usage warns about multiple ambiguous calls in strict mode

    Cannot reliably detect which packages some functions are from.
    x Ambiguous functions: `ess_bulk()`, `rhat()`
    i Please namespace them (`pkg::function()`) and rerun or set `strict = FALSE`.

# scan_usage warns on ambiguous calls in non-strict mode

    Cannot reliably detect which packages some functions are from.
    x Ambiguous functions: `foo()`
    i Please namespace them (`pkg::function()`) and rerun or set `strict = FALSE`.

# print.scan_usage shows functions with no packages

    $packages
    character(0)
    
    $functions
    [1] "loo::loo"            "posterior::as_draws"
    
    attr(,"class")
    [1] "scan_usage"

# print.scan_usage shows many packages with no functions

    $packages
    [1] "bayesplot" "brms"      "cmdstanr"  "loo"       "posterior" "projpred" 
    [7] "rstan"     "shinystan"
    
    $functions
    character(0)
    
    attr(,"class")
    [1] "scan_usage"

# print.scan_usage shows many functions for one package

    $packages
    [1] "posterior"
    
    $functions
    [1] "posterior::summarise_draws" "posterior::as_draws_df"    
    [3] "posterior::rhat"            "posterior::ess_bulk"       
    [5] "posterior::as_draws"       
    
    attr(,"class")
    [1] "scan_usage"

# print.scan_usage shows many functions across packages

    $packages
    [1] "bayesplot" "loo"       "posterior" "rstan"    
    
    $functions
    [1] "rstan::rstan_options"       "bayesplot::mcmc_trace"     
    [3] "loo::loo"                   "posterior::as_draws"       
    [5] "bayesplot::pp_check"        "loo::loo_compare"          
    [7] "posterior::summarise_draws" "rstan::stan_model"         
    
    attr(,"class")
    [1] "scan_usage"

# scan_usage handles faux_proj directory tree

    {
      "type": "list",
      "attributes": {
        "names": {
          "type": "character",
          "attributes": {},
          "value": ["packages", "functions"]
        }
      },
      "value": [
        {
          "type": "character",
          "attributes": {},
          "value": ["bayesplot", "brms", "cmdstanr", "loo", "posterior", "projpred", "rstan", "rstanarm", "shinystan"]
        },
        {
          "type": "character",
          "attributes": {},
          "value": ["bayesplot::mcmc_acf", "bayesplot::mcmc_areas", "bayesplot::mcmc_intervals", "bayesplot::mcmc_rank_hist", "bayesplot::mcmc_trace", "bayesplot::pp_check", "bayesplot::ppc_bars", "bayesplot::ppc_error_hist", "brms::as_draws", "brms::bf", "brms::brm", "brms::conditional_effects", "brms::get_prior", "brms::mixture", "brms::set_prior", "cmdstanr::cmdstan_model", "cmdstanr::diagnostic_summary", "cmdstanr::draws", "cmdstanr::exe_file", "cmdstanr::pathfinder", "cmdstanr::print", "cmdstanr::read_cmdstan_csv", "cmdstanr::sample", "cmdstanr::summary", "cmdstanr::write_stan_json", "loo::loo", "loo::loo_compare", "posterior::as_draws", "posterior::as_draws_cmdstanr", "posterior::as_draws_df", "posterior::as_draws_matrix", "posterior::ess_bulk", "posterior::ess_tail", "posterior::mcse_mean", "posterior::rhat", "posterior::subset_draws", "posterior::summarise_draws", "projpred::cv_varsel", "rstan::extract", "rstan::stan_model", "rstanarm::logit", "shinystan::launch_shinystan"]
        }
      ]
    }

# scan_usage errors on multiple directories

    `path` must be a single directory or a vector of files.
    x Mixed directories and files or multiple directories are not supported.

# scan_usage errors when mixing directories and files

    `path` must be a single directory or a vector of files.
    x Mixed directories and files or multiple directories are not supported.

