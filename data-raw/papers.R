BDA <- bibentry(
  bibtype = "Book",
  key = "BDA",
  title = "Bayesian Data Analysis",
  author = c(
    person("Andrew", "Gelman"),
    person("John", "Carlin"),
    person("Hal", "Stern"),
    person("David", "Dunson"),
    person("Aki", "Vehtari"),
    person("Donald", "Rubin")
  ),
  year = "2013",
  publisher = "Chapman & Hall/CRC",
  address = "London",
  edition = "3"
)

kaplan <- bibentry(
  bibtype = "Article",
  key = "kaplan",
  title = "Nonparametric estimation from incomplete observations",
  author = c(person("Edward", "Kaplan"), person("Paul", "Meier")),
  journal = "Journal of the American Statistical Association",
  year = "1958",
  volume = "53",
  number = "282",
  pages = "457--481",
  doi = "10.1080/01621459.1958.10501452"
)

betancourt2017 <- bibentry(
  bibtype = "Misc",
  key = "betancourt-2017-hmc-intro",
  title = "A conceptual introduction to Hamiltonian Monte Carlo",
  author = person("Michael", "Betancourt"),
  year = "2017",
  note = "arXiv preprint: https://arxiv.org/abs/1701.02434"
)

betancourt_girolami2013 <- bibentry(
  bibtype = "Misc",
  key = "betancourt-girolami-2013-hmc-hierarchical",
  title = "Hamiltonian Monte Carlo for hierarchical models",
  author = c(person("Michael", "Betancourt"), person("Mark", "Girolami")),
  year = "2013",
  note = "arXiv preprint: https://arxiv.org/abs/1312.0906"
)

vehtari2019_rhat <- bibentry(
  bibtype = "Misc",
  key = "vehtari-2019-rhat",
  title = "Rank-normalization, folding, and localization: An improved R-hat for assessing convergence of MCMC",
  author = c(
    person("Aki", "Vehtari"),
    person("Andrew", "Gelman"),
    person("Daniel", "Simpson"),
    person("Bob", "Carpenter"),
    person(given = "Paul-Christian", family = "B\\u00fcrkner")
  ),
  year = "2019",
  note = "arXiv preprint: https://arxiv.org/abs/1903.08008"
)

vehtari2017_loo <- bibentry(
  bibtype = "Article",
  key = "vehtari-2017-loo",
  title = "Practical Bayesian model evaluation using leave-one-out cross-validation and WAIC",
  author = c(
    person("Aki", "Vehtari"),
    person("Andrew", "Gelman"),
    person("Jonah", "Gabry")
  ),
  journal = "Statistics and Computing",
  year = "2017",
  volume = "27",
  number = "5",
  pages = "1413--1432",
  doi = "10.1007/s11222-016-9696-4",
  note = "arXiv preprint: https://arxiv.org/abs/1507.04544"
)

hoffman2014_nuts <- bibentry(
  bibtype = "Article",
  key = "hoffman-2014-nuts",
  title = "The No-U-Turn Sampler: adaptively setting path lengths in Hamiltonian Monte Carlo",
  author = c(person("Matthew", "Hoffman"), person("Andrew", "Gelman")),
  journal = "Journal of Machine Learning Research",
  year = "2014",
  volume = "15",
  pages = "1593--1623"
)

stan_users_guide <- bibentry(
  bibtype = "Manual",
  key = "stan",
  title = "Stan Modeling Language Users Guide and Reference Manual",
  author = person("Stan Development Team"),
  year = "2026",
  url = "https://mc-stan.org/users/documentation/"
)

sailynoja2021 <- bibentry(
  bibtype = "Misc",
  key = "sailynoja-2021-uniformity",
  title = "Graphical Test for Discrete Uniformity and its Applications in Goodness of Fit Evaluation and Multiple Sample Comparison",
  author = c(
    person("Timo", "Sailynoja"),
    person(given = "Paul-Christian", family = "B\\u00fcrkner"),
    person("Aki", "Vehtari")
  ),
  year = "2021",
  note = "arXiv preprint: https://arxiv.org/abs/2103.10522"
)

kleiber2016_rootogram <- bibentry(
  bibtype = "Article",
  key = "kleiber-2016-rootogram",
  title = "Visualizing count data regressions using rootograms",
  author = c(person("Christian", "Kleiber"), person("Achim", "Zeileis")),
  journal = "The American Statistician",
  year = "2016",
  volume = "70",
  number = "3",
  pages = "296--303",
  note = "arXiv preprint: https://arxiv.org/abs/1605.01311"
)

vehtari2024_psis <- bibentry(
  bibtype = "Article",
  key = "vehtari-2024-psis",
  title = "Pareto smoothed importance sampling",
  author = c(
    person("Aki", "Vehtari"),
    person("Daniel", "Simpson"),
    person("Andrew", "Gelman"),
    person("Yuling", "Yao"),
    person("Jonah", "Gabry")
  ),
  journal = "Journal of Machine Learning Research",
  year = "2024",
  volume = "25",
  number = "72",
  pages = "1--58",
  url = "https://jmlr.org/papers/v25/19-556.html"
)

sivula2022_uncertainty <- bibentry(
  bibtype = "Misc",
  key = "sivula-2022-uncertainty",
  title = "Uncertainty in Bayesian leave-one-out cross-validation based model comparison",
  author = c(
    person("Tuomas", "Sivula"),
    person("Mans", "Magnusson"),
    person("Adrian", "Matamoros"),
    person("Aki", "Vehtari")
  ),
  year = "2022",
  note = "arXiv preprint: https://arxiv.org/abs/2008.10296v3"
)

mclatchie2023_bias <- bibentry(
  bibtype = "Misc",
  key = "mclatchie-2023-bias",
  title = "Efficient estimation and correction of selection-induced bias with order statistics",
  author = c(person("Yao", "McLatchie"), person("Aki", "Vehtari")),
  year = "2023",
  note = "arXiv preprint: https://arxiv.org/abs/2309.03742"
)

magnusson2019_large_data <- bibentry(
  bibtype = "InProceedings",
  key = "magnusson-2019-loo-large-data",
  title = "Leave-One-Out Cross-Validation for Large Data",
  author = c(
    person("Mans", "Magnusson"),
    person("Mikael", "Riis Andersen"),
    person("Johan", "Jonasson"),
    person("Aki", "Vehtari")
  ),
  booktitle = "Thirty-sixth International Conference on Machine Learning",
  year = "2019",
  volume = "97",
  pages = "4244--4253",
  note = "PMLR"
)

magnusson2020_large_data <- bibentry(
  bibtype = "InProceedings",
  key = "magnusson-2020-loo-large-data",
  title = "Leave-One-Out Cross-Validation for Model Comparison in Large Data",
  author = c(
    person("Mans", "Magnusson"),
    person("Mikael", "Riis Andersen"),
    person("Johan", "Jonasson"),
    person("Aki", "Vehtari")
  ),
  booktitle = "Proceedings of the 23rd International Conference on Artificial Intelligence and Statistics (AISTATS)",
  year = "2020",
  volume = "108",
  pages = "341--351",
  note = "PMLR"
)

yao2018_stacking <- bibentry(
  bibtype = "Article",
  key = "yao-2018-stacking",
  title = "Using stacking to average Bayesian predictive distributions",
  author = c(
    person("Yuling", "Yao"),
    person("Aki", "Vehtari"),
    person("Daniel", "Simpson"),
    person("Andrew", "Gelman")
  ),
  journal = "Bayesian Analysis",
  year = "2018",
  doi = "10.1214/17-BA1091",
  url = "https://projecteuclid.org/euclid.ba/1516093227"
)

paananen2021_moment_matching <- bibentry(
  bibtype = "Article",
  key = "paananen-2021-moment-matching",
  title = "Implicitly adaptive importance sampling",
  author = c(
    person("Topi", "Paananen"),
    person("Juho", "Piironen"),
    person("Paul-Christian", "Buerkner"),
    person("Aki", "Vehtari")
  ),
  journal = "Statistics and Computing",
  year = "2021",
  volume = "31",
  pages = "16",
  doi = "10.1007/s11222-020-09982-2",
  note = "arXiv preprint: https://arxiv.org/abs/1906.08850"
)

ionides2008_tis <- bibentry(
  bibtype = "Article",
  key = "ionides-2008-tis",
  title = "Truncated importance sampling",
  author = person("Edward", "Ionides"),
  journal = "Journal of Computational and Graphical Statistics",
  year = "2008",
  volume = "17",
  number = "2",
  pages = "295--311"
)

watanabe2010_waic <- bibentry(
  bibtype = "Article",
  key = "watanabe-2010-waic",
  title = "Asymptotic equivalence of Bayes cross validation and widely application information criterion in singular learning theory",
  author = person("Sumio", "Watanabe"),
  journal = "Journal of Machine Learning Research",
  year = "2010",
  volume = "11",
  pages = "3571--3594"
)

zhang2009_gpd <- bibentry(
  bibtype = "Article",
  key = "zhang-2009-gpd",
  title = "A new and efficient estimation method for the generalized Pareto distribution",
  author = c(person("Jian", "Zhang"), person("Michael", "Stephens")),
  journal = "Technometrics",
  year = "2009",
  volume = "51",
  pages = "316--325"
)

bolin2023_scoring <- bibentry(
  bibtype = "Article",
  key = "bolin-2023-scoring",
  title = "Local scale invariance and robustness of proper scoring rules",
  author = c(person("David", "Bolin"), person("Jonas", "Wallin")),
  journal = "Statistical Science",
  year = "2023",
  volume = "38",
  number = "1",
  pages = "140--159"
)

gneiting2007_scoring <- bibentry(
  bibtype = "Article",
  key = "gneiting-2007-scoring",
  title = "Strictly Proper Scoring Rules, Prediction, and Estimation",
  author = c(person("Tilmann", "Gneiting"), person("Adrian", "Raftery")),
  journal = "Journal of the American Statistical Association",
  year = "2007",
  volume = "102",
  number = "477",
  pages = "359--378"
)

stan_cpp_2017 <- bibentry(
  bibtype = "Misc",
  key = "stan-cpp-2017",
  title = "The Stan C++ Library, Version 2.16.0",
  author = person("Stan Development Team"),
  year = "2017",
  url = "https://mc-stan.org/"
)

rstan_2017 <- bibentry(
  bibtype = "Misc",
  key = "rstan-2017",
  title = "RStan: the R interface to Stan, Version 2.16.1",
  author = person("Stan Development Team"),
  year = "2017",
  url = "https://mc-stan.org/"
)

yao2018_vi <- bibentry(
  bibtype = "InProceedings",
  key = "yao-2018-vi",
  title = "Yes, but did it work?: Evaluating variational inference",
  author = c(
    person("Yuling", "Yao"),
    person("Aki", "Vehtari"),
    person("Daniel", "Simpson"),
    person("Andrew", "Gelman")
  ),
  booktitle = "Proceedings of the 35th International Conference on Machine Learning",
  year = "2018",
  volume = "80",
  pages = "5581--5590",
  note = "PMLR"
)
