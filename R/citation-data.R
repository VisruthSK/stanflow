.meta_year <- function(meta) sub("-.*", "", meta[["Date"]])
.meta_note <- function(meta) sprintf("R package version %s", meta[["Version"]])
.meta_authors <- function(meta) {
  meta[["Authors@R"]] |>
    str2expression() |>
    eval() |>
    Filter(\(person) any(person$role %in% c("aut", "cre")), x = _)
}

# Helper to create lazy-cached citation bindings.
.lazy_cite <- function(pkg, builder, env = .stan_citation_pkgs) {
  force(pkg)
  force(builder)
  force(env)
  assign(pkg, builder, envir = .stan_citation_builders)
  makeActiveBinding(
    pkg,
    local({
      cache_set <- FALSE
      cache <- NULL
      function() {
        if (!cache_set) {
          cache <<- builder()
          cache_set <<- TRUE
        }
        cache
      }
    }),
    env
  )
}


# TODO: add citations for stanflow.
# TODO: add function citations for specific functions (use pkg::function as key)

.stan_citation_pkgs$R <- utils::citation()

.lazy_cite("bayesplot", function() {
  packageDescription("bayesplot") |>
    (\(meta) {
      c(
        bibentry(
          bibtype = "Misc",
          key = "bayesplot",
          title = "bayesplot: Plotting for Bayesian Models",
          author = .meta_authors(meta),
          year = .meta_year(meta),
          note = .meta_note(meta),
          url = "https://mc-stan.org/bayesplot/"
        ),
        bibentry(
          bibtype = "Article",
          key = "bayesplot-2019",
          title = "Visualization in Bayesian workflow",
          author = c(
            person("Jonah", "Gabry"),
            person("Daniel", "Simpson"),
            person("Aki", "Vehtari"),
            person("Michael", "Betancourt"),
            person("Andrew", "Gelman")
          ),
          year = "2019",
          journal = "J. R. Stat. Soc. A",
          volume = 182,
          issue = 2,
          pages = "389-402",
          doi = "10.1111/rssa.12378"
        )
      )
    })()
})

.lazy_cite("cmdstanr", function() {
  if (!requireNamespace("cmdstanr", quietly = TRUE)) {
    return(NULL)
  }
  packageDescription("cmdstanr") |>
    (\(meta) {
      bibentry(
        bibtype = "Manual",
        key = "cmdstanr",
        title = "cmdstanr: R Interface to 'CmdStan'",
        author = .meta_authors(meta),
        year = .meta_year(meta),
        note = sprintf(
          "R package version %s, https://discourse.mc-stan.org",
          meta$Version
        ),
        url = "https://mc-stan.org/cmdstanr/"
      )
    })()
})

.lazy_cite("loo", function() {
  packageDescription("loo") |>
    (\(meta) {
      bibentry(
        bibtype = "Misc",
        key = "loo",
        title = "loo: Efficient leave-one-out cross-validation and WAIC for Bayesian models",
        author = .meta_authors(meta),
        note = .meta_note(meta),
        year = .meta_year(meta),
        url = "https://mc-stan.org/loo/"
      )
    })()
})

.lazy_cite("posterior", function() {
  packageDescription("posterior") |>
    (\(meta) {
      c(
        bibentry(
          bibtype = "Misc",
          key = "posterior",
          title = "posterior: Tools for Working with Posterior Distributions",
          author = .meta_authors(meta),
          year = .meta_year(meta),
          note = .meta_note(meta),
          url = "https://mc-stan.org/posterior/"
        ),
        bibentry(
          bibtype = "Article",
          key = "rhat-2021",
          title = "Rank-normalization, folding, and localization: An improved Rhat for assessing convergence of MCMC (with discussion)",
          author = c(
            person("Aki", "Vehtari"),
            person("Andrew", "Gelman"),
            person("Daniel", "Simpson"),
            person("Bob", "Carpenter"),
            person("Paul-Christian", "B\\\"urkner")
          ),
          journal = "Bayesian Analysis",
          year = "2021",
          volume = "16",
          number = "2",
          pages = "667-718"
        )
      )
    })()
})

.lazy_cite("projpred", function() {
  packageDescription("projpred") |>
    (\(meta) {
      bibentry(
        bibtype = "Misc",
        key = "projpred",
        title = "{{projpred}}: {{Projection}} Predictive Feature Selection",
        author = .meta_authors(meta),
        year = .meta_year(meta),
        note = .meta_note(meta),
        url = "https://mc-stan.org/projpred/"
      )
    })()
})

.lazy_cite("rstan", function() {
  if (!requireNamespace("rstan", quietly = TRUE)) {
    return(NULL)
  }
  packageDescription("rstan") |>
    (\(meta) {
      bibentry(
        bibtype = "Misc",
        key = "rstan",
        title = "{RStan}: the {R} interface to {Stan}",
        author = .meta_authors(meta),
        note = .meta_note(meta),
        url = "https://mc-stan.org/"
      )
    })()
})

.lazy_cite("rstanarm", function() {
  if (!requireNamespace("rstanarm", quietly = TRUE)) {
    return(NULL)
  }
  packageDescription("rstanarm") |>
    (\(meta) {
      bibentry(
        bibtype = "Misc",
        key = "rstanarm",
        title = "rstanarm: {Bayesian} applied regression modeling via {Stan}.",
        author = c(
          person("Ben", "Goodrich"),
          person("Jonah", "Gabry"),
          person("Imad", "Ali"),
          person("Sam", "Brilleman")
        ),
        note = .meta_note(meta),
        year = .meta_year(meta),
        url = "https://mc-stan.org/rstanarm/"
      )
    })()
})

.lazy_cite("shinystan", function() {
  packageDescription("shinystan") |>
    (\(meta) {
      bibentry(
        bibtype = "Manual",
        key = "shinystan",
        title = "shinystan: Interactive Visual and Numerical Diagnostics and Posterior Analysis for Bayesian Models",
        author = .meta_authors(meta),
        year = .meta_year(meta),
        note = .meta_note(meta),
        url = "https://mc-stan.org/shinystan/"
      )
    })()
})
