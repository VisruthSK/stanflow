.stan_pkgs <- c(
  "bayesplot",
  "brms",
  "cmdstanr",
  "loo",
  "posterior",
  "projpred",
  "rstan",
  "rstanarm",
  "rstantools",
  "shinystan",
  "stanflow"
)

message(
  "Make sure you use `stanflow_update()` to make sure your packages are up to date before generating the sysdata file."
)

missing <- .stan_pkgs[
  !vapply(.stan_pkgs, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing) > 0) {
  stop(
    sprintf(
      "Missing required package%s: %s",
      if (length(missing) == 1) "" else "s",
      paste(missing, collapse = ", ")
    ),
    call. = FALSE
  )
}

.stan_citation_pkgs <- new.env(parent = emptyenv())
.stan_citation_funs <- new.env(parent = emptyenv())
.stan_citation_pkg_extras <- new.env(parent = emptyenv())
.stan_citation_pkgs$brms <- c(
  bibentry(
    bibtype = "Article",
    title = "{brms}: An {R} Package for {Bayesian} Multilevel Models Using {Stan}",
    author = person(given = "Paul-Christian", family = "B\\u00fcrkner"),
    journal = "Journal of Statistical Software",
    year = "2017",
    volume = "80",
    number = "1",
    pages = "1--28",
    doi = "10.18637/jss.v080.i01",
    textVersion = paste(
      "Paul-Christian B\\u00fcrkner (2017).",
      "brms: An R Package for Bayesian Multilevel Models Using Stan.",
      "Journal of Statistical Software, 80(1), 1-28.",
      "doi:10.18637/jss.v080.i01"
    ),
    encoding = "UTF-8"
  ),
  bibentry(
    bibtype = "Article",
    title = "Advanced {Bayesian} Multilevel Modeling with the {R} Package {brms}",
    author = person(given = "Paul-Christian", family = "B\\u00fcrkner"),
    journal = "The R Journal",
    year = "2018",
    volume = "10",
    number = "1",
    pages = "395--411",
    doi = "10.32614/RJ-2018-017",
    textVersion = paste(
      "Paul-Christian B\\u00fcrkner (2018).",
      "Advanced Bayesian Multilevel Modeling with the R Package brms.",
      "The R Journal, 10(1), 395-411.",
      "doi:10.32614/RJ-2018-017"
    ),
    encoding = "UTF-8"
  ),
  bibentry(
    bibtype = "Article",
    title = "Bayesian Item Response Modeling in {R} with {brms} and {Stan}",
    author = person(given = "Paul-Christian", family = "B\\u00fcrkner"),
    journal = "Journal of Statistical Software",
    year = "2021",
    volume = "100",
    number = "5",
    pages = "1--54",
    doi = "10.18637/jss.v100.i05",
    textVersion = paste(
      "Paul-Christian B\\u00fcrkner (2021).",
      "Bayesian Item Response Modeling in R with brms and Stan.",
      "Journal of Statistical Software, 100(5), 1-54.",
      "doi:10.18637/jss.v100.i05"
    ),
    encoding = "UTF-8"
  )
)

assign_citation <- function(pkg, funs, entries) {
  for (fun in funs) {
    .stan_citation_funs[[paste0(pkg, "::", fun)]] <- entries
  }
}

source("data-raw/papers.R")
.stan_citation_pkg_extras$bayesplot <- gabry2019_vis
.stan_citation_pkg_extras$posterior <- c(
  posterior_joss,
  vehtari2021_rhat
)
source("data-raw/bayesplot-citations.R")
source("data-raw/brms-citations.R")
source("data-raw/cmdstanr-citations.R")
source("data-raw/loo-citations.R")
source("data-raw/posterior-citations.R")
source("data-raw/rstan-citations.R")
source("data-raw/rstanarm-citations.R")
source("data-raw/shinystan-citations.R")
source("data-raw/projpred-citations.R")
source("data-raw/rstantools-citations.R")

# Use ascribe to build scanner data for the Stan universe
ascribe::generate_universe_sysdata(
  packages = .stan_pkgs,
  prefix = "stan",
  extra_vars = list(
    .stan_citation_pkgs = .stan_citation_pkgs,
    .stan_citation_funs = .stan_citation_funs,
    .stan_citation_pkg_extras = .stan_citation_pkg_extras
  )
)
