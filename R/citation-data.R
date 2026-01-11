.stan_pkgs <- c(stanflow_pkgs, "rstantools")
.stan_citation_pkgs <- new.env(parent = emptyenv())
.stan_citation_funs <- new.env(parent = .stan_citation_pkgs)
.meta_year <- function(meta) sub("-.*", "", meta[["Date"]])
.meta_note <- function(meta) sprintf("R package version %s", meta[["Version"]])
.meta_authors <- function(meta) {
  meta[["Authors@R"]] |>
    str2expression() |>
    eval() |>
    Filter(\(person) any(person$role %in% c("aut", "cre")), x = _)
}

# TODO: add citations for R and for stanflow.

# TODO: add more package citations--wrap brms and rstanarm in requireNamespace checks

# TODO: add function citations for specific functions (use pkg::function as key)

.stan_citation_pkgs$bayesplot <- packageDescription("bayesplot") |>
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

if (requireNamespace("cmdstanr", quietly = TRUE)) {
  .stan_citation_pkgs$cmdstanr <- packageDescription("cmdstanr") |>
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
}

.stan_citation_pkgs$loo <- packageDescription("loo") |>
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

.stan_citation_pkgs$posterior <- packageDescription("posterior") |>
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

.stan_citation_pkgs$projpred <- packageDescription("projpred") |>
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

if (requireNamespace("rstan", quietly = TRUE)) {
  .stan_citation_pkgs$rstan <- packageDescription("rstan") |>
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
}

if (requireNamespace("rstanarm", quietly = TRUE)) {
  .stan_citation_pkgs$rstanarm <- packageDescription("rstanarm") |>
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
}

.stan_citation_pkgs$shinystan <- packageDescription("shinystan") |>
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
