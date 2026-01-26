# package citations match snapshots

    Code
      cat(paste(utils::toBibtex(.build_pkg_citation(pkg)), collapse = "\n"), "\n")
    Output
      @Manual{stanflow,
        title = {stanflow: Stan Bayesian Workflow},
        author = {Visruth {Srimath Kandali}},
        year = {2026},
        note = {R package version 0.0.0.9000},
        url = {https://visruthsk.github.io/stanflow/},
      } 

---

    Code
      cat(paste(utils::toBibtex(.build_pkg_citation(pkg)), collapse = "\n"), "\n")
    Output
      @Manual{,
        title = {R: A Language and Environment for Statistical Computing},
        author = {{R Core Team}},
        organization = {R Foundation for Statistical Computing},
        address = {Vienna, Austria},
        year = {2025},
        url = {https://www.R-project.org/},
      } 

---

    Code
      cat(paste(utils::toBibtex(.build_pkg_citation(pkg)), collapse = "\n"), "\n")
    Output
      @Misc{bayesplot,
        title = {bayesplot: Plotting for Bayesian Models},
        author = {Jonah Gabry and Tristan Mahr},
        year = {2025},
        note = {R package version 1.15.0},
        url = {https://mc-stan.org/bayesplot/},
      }
      
      @Article{bayesplot-2019,
        title = {Visualization in Bayesian workflow},
        author = {Jonah Gabry and Daniel Simpson and Aki Vehtari and Michael Betancourt and Andrew Gelman},
        year = {2019},
        journal = {J. R. Stat. Soc. A},
        volume = {182},
        issue = {2},
        pages = {389-402},
        doi = {10.1111/rssa.12378},
      } 

---

    Code
      cat(paste(utils::toBibtex(.build_pkg_citation(pkg)), collapse = "\n"), "\n")
    Output
      @Article{,
        title = {{brms}: An {R} Package for {Bayesian} Multilevel Models Using {Stan}},
        author = {Paul-Christian B\u00fcrkner},
        journal = {Journal of Statistical Software},
        year = {2017},
        volume = {80},
        number = {1},
        pages = {1--28},
        doi = {10.18637/jss.v080.i01},
        encoding = {UTF-8},
      }
      
      @Article{,
        title = {Advanced {Bayesian} Multilevel Modeling with the {R} Package {brms}},
        author = {Paul-Christian B\u00fcrkner},
        journal = {The R Journal},
        year = {2018},
        volume = {10},
        number = {1},
        pages = {395--411},
        doi = {10.32614/RJ-2018-017},
        encoding = {UTF-8},
      }
      
      @Article{,
        title = {Bayesian Item Response Modeling in {R} with {brms} and {Stan}},
        author = {Paul-Christian B\u00fcrkner},
        journal = {Journal of Statistical Software},
        year = {2021},
        volume = {100},
        number = {5},
        pages = {1--54},
        doi = {10.18637/jss.v100.i05},
        encoding = {UTF-8},
      } 

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

# scan_usage errors on multiple directories

    `path` must be a single directory or a vector of files.
    x Mixed directories and files or multiple directories are not supported.

# scan_usage alerts full paths for file vectors

    

# scan_usage alerts full paths for directories

    

# scan_usage errors when mixing directories and files

    `path` must be a single directory or a vector of files.
    x Mixed directories and files or multiple directories are not supported.

