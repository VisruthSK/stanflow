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

gabry2019_vis <- bibentry(
  bibtype = "Article",
  key = "gabry-2019-vis",
  title = "Visualization in Bayesian workflow",
  author = c(
    person("Jonah", "Gabry"),
    person("Daniel", "Simpson"),
    person("Aki", "Vehtari"),
    person("Michael", "Betancourt"),
    person("Andrew", "Gelman")
  ),
  journal = "J. R. Stat. Soc. A",
  year = "2019",
  volume = "182",
  pages = "389--402",
  doi = "10.1111/rssa.12378"
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

stan_reference_manual <- bibentry(
  bibtype = "Manual",
  key = "stan-reference-manual",
  title = "Stan Reference Manual",
  author = person("Stan Development Team"),
  year = "2026",
  url = "https://mc-stan.org/docs/reference-manual/"
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

gelman_hill_2007 <- bibentry(
  bibtype = "Book",
  key = "gelman-hill-2007",
  title = "Data Analysis Using Regression and Multilevel/Hierarchical Models",
  author = c(person("Andrew", "Gelman"), person("Jennifer", "Hill")),
  year = "2007",
  publisher = "Cambridge University Press",
  address = "Cambridge, UK"
)

gelman_carlin_2014 <- bibentry(
  bibtype = "Article",
  key = "gelman-carlin-2014",
  title = "Beyond power calculations: assessing Type S (sign) and Type M (magnitude) errors",
  author = c(person("Andrew", "Gelman"), person("John", "Carlin")),
  journal = "Perspectives on Psychological Science",
  year = "2014",
  volume = "9",
  number = "6",
  pages = "641--651"
)

morey2016_ci <- bibentry(
  bibtype = "Article",
  key = "morey-2016-ci",
  title = "The fallacy of placing confidence in confidence intervals",
  author = c(
    person("Richard", "Morey"),
    person("Rink", "Hoekstra"),
    person("Jeff", "Rouder"),
    person("Michael", "Lee"),
    person("Eric-Jan", "Wagenmakers")
  ),
  journal = "Psychonomic Bulletin & Review",
  year = "2016",
  volume = "23",
  number = "1",
  pages = "103--123"
)

muth2018 <- bibentry(
  bibtype = "Article",
  key = "muth-2018-rstanarm-shinystan",
  title = "User-friendly Bayesian regression modeling: A tutorial with rstanarm and shinystan",
  author = c(
    person("Carolin", "Muth"),
    person("Zita", "Oravecz"),
    person("Jonah", "Gabry")
  ),
  journal = "The Quantitative Methods for Psychology",
  year = "2018",
  volume = "14",
  number = "2",
  pages = "99--119",
  url = "https://www.tqmp.org/RegularArticles/vol14-2/p099/p099.pdf"
)

piironen2017_horseshoe <- bibentry(
  bibtype = "Misc",
  key = "piironen-2017-horseshoe",
  title = "Sparsity information and regularization in the horseshoe and other shrinkage priors",
  author = c(person("Juho", "Piironen"), person("Aki", "Vehtari")),
  year = "2017",
  note = "arXiv preprint: https://arxiv.org/abs/1707.01694"
)

gelman2019_bayes_r2 <- bibentry(
  bibtype = "Article",
  key = "gelman-2019-bayes-r2",
  title = "R-squared for Bayesian regression models",
  author = c(
    person("Andrew", "Gelman"),
    person("Ben", "Goodrich"),
    person("Jonah", "Gabry"),
    person("Aki", "Vehtari")
  ),
  journal = "The American Statistician",
  year = "2019",
  doi = "10.1080/00031305.2018.1549100"
)

rizopoulos2011 <- bibentry(
  bibtype = "Article",
  key = "rizopoulos-2011",
  title = "Dynamic predictions and prospective accuracy in joint models for longitudinal and time-to-event data",
  author = person("Dimitris", "Rizopoulos"),
  journal = "Biometrics",
  year = "2011",
  volume = "67",
  pages = "819--829"
)

cook2006 <- bibentry(
  bibtype = "Article",
  key = "cook-2006",
  title = "Validation of software for Bayesian models using posterior quantiles",
  author = c(
    person("S.", "Cook"),
    person("Andrew", "Gelman"),
    person("Donald", "Rubin")
  ),
  journal = "Journal of Computational and Graphical Statistics",
  year = "2006",
  volume = "15",
  number = "3",
  pages = "675--692"
)

ferrari2004 <- bibentry(
  bibtype = "Article",
  key = "ferrari-2004-betareg",
  title = "Beta regression for modeling rates and proportions",
  author = c(person("Silvia", "Ferrari"), person("Francisco", "Cribari-Neto")),
  journal = "Journal of Applied Statistics",
  year = "2004",
  volume = "31",
  number = "7",
  pages = "799--815"
)

crainiceanu2005 <- bibentry(
  bibtype = "Article",
  key = "crainiceanu-2005",
  title = "Bayesian analysis for penalized spline regression using WinBUGS",
  author = c(
    person("Ciprian", "Crainiceanu"),
    person("David", "Ruppert"),
    person("Matthew", "Wand")
  ),
  journal = "Journal of Statistical Software",
  year = "2005",
  volume = "14",
  number = "14",
  pages = "1--22",
  url = "https://www.jstatsoft.org/article/view/v014i14"
)

lewandowski2009 <- bibentry(
  bibtype = "Article",
  key = "lewandowski-2009",
  title = "Generating random correlation matrices based on vines and extended onion method",
  author = c(
    person("Daniel", "Lewandowski"),
    person("Dorota", "Kurowicka"),
    person("Harry", "Joe")
  ),
  journal = "Journal of Multivariate Analysis",
  year = "2009",
  volume = "100",
  number = "9",
  pages = "1989--2001"
)

nagler1994 <- bibentry(
  bibtype = "Article",
  key = "nagler-1994",
  title = "Scobit: An Alternative Estimator to Logit and Probit",
  author = person("Jonathan", "Nagler"),
  journal = "American Journal of Political Science",
  year = "1994",
  volume = "38",
  pages = "230--255"
)

gelman2008_prior <- bibentry(
  bibtype = "Article",
  key = "gelman-2008-prior",
  title = "A weakly informative default prior distribution for logistic and other regression models",
  author = c(
    person("Andrew", "Gelman"),
    person("Aleks", "Jakulin"),
    person("Maria", "Pittau"),
    person("Yu-Sung", "Su")
  ),
  journal = "Annals of Applied Statistics",
  year = "2008",
  volume = "2",
  number = "4",
  pages = "1360--1383"
)

gelman_hill_2006 <- bibentry(
  bibtype = "Book",
  key = "gelman-hill-2006",
  title = "Data Analysis Using Regression and Multilevel/Hierarchical Models",
  author = c(person("Andrew", "Gelman"), person("Jennifer", "Hill")),
  year = "2006",
  publisher = "Cambridge University Press",
  address = "Cambridge, UK",
  doi = "10.1017/CBO9780511790942"
)

piironen2020_projpred <- bibentry(
  bibtype = "Article",
  key = "piironen-2020-projpred",
  title = "Projective Inference in High-Dimensional Problems: Prediction and Feature Selection",
  author = c(
    person("Juho", "Piironen"),
    person("Markus", "Paasiniemi"),
    person("Aki", "Vehtari")
  ),
  journal = "Electronic Journal of Statistics",
  year = "2020",
  volume = "14",
  number = "1",
  pages = "2155--2197",
  doi = "10.1214/20-EJS1711"
)

mclatchie2025_projpred <- bibentry(
  bibtype = "Article",
  key = "mclatchie-2025-projpred",
  title = "Advances in Projection Predictive Inference",
  author = c(
    person("Yann", "McLatchie"),
    person("Solvi", "Rognvaldsson"),
    person("Frank", "Weber"),
    person("Aki", "Vehtari")
  ),
  journal = "Statistical Science",
  year = "2025",
  volume = "40",
  number = "1",
  pages = "128--147",
  doi = "10.1214/24-STS949"
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

kucukelbir2017_advi <- bibentry(
  bibtype = "Article",
  key = "kucukelbir-2017-advi",
  title = "Automatic differentiation variational inference",
  author = c(
    person("Alp", "Kucukelbir"),
    person("Dustin", "Tran"),
    person("Rajesh", "Ranganath"),
    person("Andrew", "Gelman"),
    person("David", "Blei")
  ),
  journal = "Journal of Machine Learning Research",
  year = "2017",
  volume = "18",
  number = "14",
  pages = "1--45"
)

zhang2022_pathfinder <- bibentry(
  bibtype = "Article",
  key = "zhang-2022-pathfinder",
  title = "Pathfinder: parallel quasi-Newton variational inference",
  author = c(
    person("Lu", "Zhang"),
    person("Bob", "Carpenter"),
    person("Andrew", "Gelman"),
    person("Aki", "Vehtari")
  ),
  journal = "Journal of Machine Learning Research",
  year = "2022",
  volume = "23",
  number = "306",
  pages = "1--49"
)

tastle2007_dissent <- bibentry(
  bibtype = "Article",
  key = "tastle-2007-dissent",
  title = "Consensus and dissention: A measure of ordinal dispersion",
  author = c(person("William", "Tastle"), person("Mark", "Wierman")),
  journal = "International Journal of Approximate Reasoning",
  year = "2007",
  volume = "45",
  number = "3",
  pages = "531--545",
  doi = "10.1016/j.ijar.2006.06.024"
)

wilcox1967_variation <- bibentry(
  bibtype = "TechReport",
  key = "wilcox-1967-variation",
  title = "Indices of Qualitative Variation",
  author = person("Allen", "Wilcox"),
  year = "1967",
  institution = "Oak Ridge National Laboratory",
  number = "ORNL-TM-1919",
  address = "Oak Ridge, TN"
)

vehtari2021_ess_comparison <- bibentry(
  bibtype = "Misc",
  key = "vehtari-2021-ess-comparison",
  title = "Comparison of MCMC effective sample size estimators",
  author = person("Aki", "Vehtari"),
  year = "2021",
  url = "https://avehtari.github.io/rhat_ess/ess_comparison.html"
)

kenney1951_stats <- bibentry(
  bibtype = "Book",
  key = "kenney-1951-statistics",
  title = "Mathematics of Statistics",
  author = c(person("J. F.", "Kenney"), person("E. S.", "Keeping")),
  year = "1951",
  volume = "2",
  publisher = "D. Van Nostrand Company",
  address = "New York"
)

kitagawa1996_mc_filter <- bibentry(
  bibtype = "Article",
  key = "kitagawa-1996-mc-filter",
  title = "Monte Carlo Filter and Smoother for Non-Gaussian Nonlinear State Space Models",
  author = person("G.", "Kitagawa"),
  journal = "Journal of Computational and Graphical Statistics",
  year = "1996",
  volume = "5",
  number = "1",
  pages = "1--25"
)

margossian2023_nested_rhat <- bibentry(
  bibtype = "Misc",
  key = "margossian-2023-nested-rhat",
  title = "Nested R-hat: Assessing the convergence of Markov chain Monte Carlo when running many short chains",
  author = c(
    person("Charles", "Margossian"),
    person("Matthew", "Hoffman"),
    person("Pavel", "Sountsov"),
    person("Lionel", "Riou-Durand"),
    person("Aki", "Vehtari"),
    person("Andrew", "Gelman")
  ),
  year = "2023",
  note = "arXiv preprint: https://arxiv.org/abs/2110.13017v4"
)

lambert2020_rstar <- bibentry(
  bibtype = "Misc",
  key = "lambert-2020-rstar",
  title = "R*: A robust MCMC convergence diagnostic with uncertainty using gradient-boosted machines",
  author = c(person("Ben", "Lambert"), person("Aki", "Vehtari")),
  year = "2020",
  note = "arXiv preprint: https://arxiv.org/abs/2003.07900"
)

sailynoja2022_uniformity <- bibentry(
  bibtype = "Article",
  key = "sailynoja-2022-uniformity",
  title = "Graphical test for discrete uniformity and its applications in goodness-of-fit evaluation and multiple sample comparison",
  author = c(
    person("Teemu", "Sailynoja"),
    person(given = "Paul-Christian", family = "B\\u00fcrkner"),
    person("Aki", "Vehtari")
  ),
  journal = "Statistics and Computing",
  year = "2022",
  volume = "32",
  pages = "32",
  doi = "10.1007/s11222-022-10090-6"
)

czado2009_pit <- bibentry(
  bibtype = "Article",
  key = "czado-2009-pit",
  title = "Predictive Model Assessment for Count Data",
  author = c(
    person("Claudia", "Czado"),
    person("Tilmann", "Gneiting"),
    person("Leonhard", "Held")
  ),
  journal = "Biometrics",
  year = "2009",
  volume = "65",
  number = "4",
  pages = "1254--1261",
  doi = "10.1111/j.1541-0420.2009.01191.x"
)

gelman_rubin1992 <- bibentry(
  bibtype = "Article",
  key = "gelman-rubin-1992",
  title = "Inference from iterative simulation using multiple sequences",
  author = c(person("Andrew", "Gelman"), person("Donald", "Rubin")),
  journal = "Statistical Science",
  year = "1992",
  volume = "7",
  number = "4",
  pages = "457--472"
)

boneva1971_spline <- bibentry(
  bibtype = "Article",
  key = "boneva-1971-spline",
  title = "Spline transformations: Three new diagnostic aids for the statistical data-analyst",
  author = c(
    person("L. I.", "Boneva"),
    person("D.", "Kendall"),
    person("I.", "Stefanov")
  ),
  journal = "Journal of the Royal Statistical Society: Series B (Methodological)",
  year = "1971",
  volume = "33",
  number = "1",
  pages = "1--71",
  url = "https://www.jstor.org/stable/2986005"
)

hartikainen2017_divergences <- bibentry(
  bibtype = "Misc",
  key = "hartikainen-2017-divergences",
  title = "Concentration of divergences",
  author = person("Aki", "Hartikainen"),
  year = "2017",
  note = "Message posted to The Stan Forums",
  url = "https://discourse.mc-stan.org/t/concentration-of-divergences/1590/21"
)

carvalho2009_horseshoe <- bibentry(
  bibtype = "InProceedings",
  key = "carvalho-2009-horseshoe",
  title = "Handling sparsity via the horseshoe",
  author = c(
    person("Carlos", "Carvalho"),
    person("Nicholas", "Polson"),
    person("James", "Scott")
  ),
  booktitle = "Artificial Intelligence and Statistics",
  year = "2009",
  url = "http://proceedings.mlr.press/v5/carvalho09a"
)

piironen2017_hyperprior <- bibentry(
  bibtype = "Misc",
  key = "piironen-2017-hyperprior",
  title = "On the Hyperprior Choice for the Global Shrinkage Parameter in the Horseshoe Prior",
  author = c(person("Juho", "Piironen"), person("Aki", "Vehtari")),
  year = "2017",
  note = "arXiv preprint: https://arxiv.org/abs/1610.05559v1"
)

zhang2020_r2d2 <- bibentry(
  bibtype = "Article",
  key = "zhang-2020-r2d2",
  title = "Bayesian regression using a prior on the model fit: The R2-D2 shrinkage prior",
  author = c(
    person("Y. D.", "Zhang"),
    person("B. P.", "Naughton"),
    person("H. D.", "Bondell"),
    person("B. J.", "Reich")
  ),
  journal = "Journal of the American Statistical Association",
  year = "2020",
  note = "arXiv preprint: https://arxiv.org/pdf/1609.00046"
)

aguilar2022_r2d2m2 <- bibentry(
  bibtype = "Misc",
  key = "aguilar-2022-r2d2m2",
  title = "Intuitive Joint Priors for Bayesian Linear Multilevel Models: The R2D2M2 prior",
  author = c(
    person("J. E.", "Aguilar"),
    person(given = "Paul-Christian", family = "B\\u00fcrkner")
  ),
  year = "2022",
  note = "arXiv preprint: https://arxiv.org/abs/2208.07132"
)

park2008_lasso <- bibentry(
  bibtype = "Article",
  key = "park-2008-lasso",
  title = "The Bayesian Lasso",
  author = c(person("Trevor", "Park"), person("George", "Casella")),
  journal = "Journal of the American Statistical Association",
  year = "2008",
  volume = "103",
  number = "482",
  pages = "681--686"
)

pedersen2019_gam <- bibentry(
  bibtype = "Article",
  key = "pedersen-2019-gam",
  title = "Hierarchical generalized additive models in ecology: an introduction with mgcv",
  author = c(
    person("E. J.", "Pedersen"),
    person("D. L.", "Miller"),
    person("G. L.", "Simpson"),
    person("N.", "Ross")
  ),
  journal = "PeerJ",
  year = "2019"
)

burkner_charpentier2020_monotonic <- bibentry(
  bibtype = "Article",
  key = "burkner-charpentier-2020-monotonic",
  title = "Modeling Monotonic Effects of Ordinal Predictors in Regression Models",
  author = c(
    person(given = "Paul-Christian", family = "B\\u00fcrkner"),
    person("Emilie", "Charpentier")
  ),
  journal = "British Journal of Mathematical and Statistical Psychology",
  year = "2020",
  doi = "10.1111/bmsp.12195"
)

kosmidis_zeileis2024_beta <- bibentry(
  bibtype = "Misc",
  key = "kosmidis-zeileis-2024-beta",
  title = "Extended-Support Beta Regression for [0, 1] Responses",
  author = c(person("Ioannis", "Kosmidis"), person("Achim", "Zeileis")),
  year = "2024",
  note = "arXiv preprint: https://arxiv.org/abs/2409.07233"
)

gelman_hwang_vehtari2014 <- bibentry(
  bibtype = "Article",
  key = "gelman-hwang-vehtari-2014",
  title = "Understanding predictive information criteria for Bayesian models",
  author = c(
    person("Andrew", "Gelman"),
    person("Jessica", "Hwang"),
    person("Aki", "Vehtari")
  ),
  journal = "Statistics and Computing",
  year = "2014",
  volume = "24",
  pages = "997--1016"
)

vehtari_lampinen2002 <- bibentry(
  bibtype = "Article",
  key = "vehtari-lampinen-2002",
  title = "Bayesian model assessment and comparison using cross-validation predictive densities",
  author = c(person("Aki", "Vehtari"), person("Jouni", "Lampinen")),
  journal = "Neural Computation",
  year = "2002",
  volume = "14",
  number = "10",
  pages = "2439--2468"
)

vehtari2021_rhat <- bibentry(
  bibtype = "Article",
  key = "vehtari-2021-rhat",
  title = "Rank-normalization, folding, and localization: An improved R-hat for assessing convergence of MCMC (with discussion)",
  author = c(
    person("Aki", "Vehtari"),
    person("Andrew", "Gelman"),
    person("Daniel", "Simpson"),
    person("Bob", "Carpenter"),
    person(given = "Paul-Christian", family = "B\\u00fcrkner")
  ),
  journal = "Bayesian Analysis",
  year = "2021",
  volume = "16",
  number = "2",
  pages = "667--718",
  doi = "10.1214/20-BA1221"
)

bates2015_lme4 <- bibentry(
  bibtype = "Article",
  key = "bates-2015-lme4",
  title = "Fitting Linear Mixed-Effects Models Using lme4",
  author = c(
    person("Douglas", "Bates"),
    person("Martin", "M\\u00e4chler"),
    person("Ben", "Bolker"),
    person("Steve", "Walker")
  ),
  journal = "Journal of Statistical Software",
  year = "2015",
  volume = "67",
  number = "1",
  pages = "1--48"
)
