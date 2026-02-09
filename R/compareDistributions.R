
compareContinuousDistributionsInternal <- function(jaspResults, dataset, options, state=NULL){
  comparisonTable <- jaspResults[["comparisonTable"]] %setOrRetrieve%
    .ccdDistributionComparisonTable(jaspResults, options)

  if (options[["variable"]] == "") return()

  variable <- dataset[[options[["variable"]]]]

  distributions <- jaspResults[["distributions"]] %setOrRetrieve% (
    .ccdGetDistributions(jaspResults, variable, options) |>
    createJaspState(dependencies = .ccdDependencies())
  )

  # here the list gets possibly sorted by AIC/BIC values...
  distributions <- .ccdFillDistributionComparisonTable(comparisonTable, options, distributions, variable)

  .ccdPerDistributionOutput(jaspResults, options, distributions, variable)
}

.ccdGetDistributions <- function(jaspResults, variable, options) {
  distributions <- switch(
    options[["distributionSpecification"]],
    auto = .ccdGetDistributionsAuto(jaspResults, variable, options),
    manual = .ccdGetDistributionsManual(jaspResults, variable, options)
  )

  distributions <- lapply(distributions, function(distribution) {
    fitted <- try(DistributionS7::fit_distribution(distribution, estimator=DistributionS7::Mle(), data=variable))
    if (isTryError(fitted)) return(distribution) else return(fitted)
  })

  return(distributions)
}

.ccdGetDistributionsAuto <- function(jaspResults, variable, options) {
  if (options[["dataBoundedBelow"]] && options[["dataBoundedAbove"]]) {
    distributions <- list(
      DistributionS7::uniform(
        min = DistributionS7::fixed(options[["dataBoundedBelowAt"]]),
        max = DistributionS7::fixed(options[["dataBoundedAboveAt"]])
      ),
      DistributionS7::stretched_beta(
        alpha = 1,
        beta = 1,
        min = DistributionS7::fixed(options[["dataBoundedBelowAt"]]),
        max = DistributionS7::fixed(options[["dataBoundedAboveAt"]])
      ),
      DistributionS7::triangular(
        a = DistributionS7::fixed(options[["dataBoundedBelowAt"]]),
        b = DistributionS7::fixed(options[["dataBoundedAboveAt"]]),
        c = (options[["dataBoundedBelowAt"]] + options[["dataBoundedAboveAt"]])/2
      )
    )
  } else if (options[["dataBoundedBelow"]] && options[["dataBoundedBelowAt"]] == 0) {
    distributions <- list(
      DistributionS7::exponential(lambda=1),
      DistributionS7::frechet(alpha=2, sigma=1, theta=DistributionS7::fixed(options[["dataBoundedBelowAt"]])),
      DistributionS7::gamma(alpha = 2, theta = 1),
      DistributionS7::inverse_gamma(alpha = 2, theta = 1),
      DistributionS7::gompertz(eta=2, beta=1),
      DistributionS7::log_logistic(mu=0, sigma=1),
      DistributionS7::log_normal(mu=0, sigma=1),
      DistributionS7::wald(mu=1, lambda=1),
      DistributionS7::weibull(shape=2, scale=1)
    )
  } else if (options[["dataBoundedBelow"]]){
    shift <- DistributionS7::fixed(options[["dataBoundedBelowAt"]])
    distributions <- list(
      DistributionS7::shifted_exponential(lambda=1, shift=shift),
      DistributionS7::shifted_log_normal(mu=0, sigma=1, shift=shift),
      DistributionS7::shifted_gamma(alpha=2, theta=1, shift=shift),
      DistributionS7::shifted_inverse_gamma(alpha=2, theta=1, shift=shift),
      DistributionS7::shifted_log_logistic(mu=0, sigma=1, shift=shift),
      DistributionS7::shifted_wald(mu=1, lambda=1, shift=shift),
      DistributionS7::shifted_weibull(shape=2, scale=1, shift=shift)
    )
  } else if (options[["dataBoundedAbove"]]) {
    .quitAnalysis(gettext("Currently, no distributions bounded from above but unbounded from below are available"))
  } else {
    distributions <- list(
      DistributionS7::normal(mu=0, sigma=1),
      DistributionS7::cauchy(mu=0, sigma=1),
      DistributionS7::student_t(nu=5, mu=0, sigma=1),
      DistributionS7::gumbel(mu=0, beta=1),
      DistributionS7::laplace(mu=0, beta=1),
      DistributionS7::logistic(mu=0, sigma=1),
      DistributionS7::skew_normal(xi=0, omega=1, alpha=0),
      DistributionS7::skew_cauchy(xi=0, omega=1, alpha=0),
      DistributionS7::skew_t(xi=0, omega=1, alpha=0, nu=5),
      DistributionS7::symmetric_generalized_normal(mu=0, alpha=1, beta=2)
    )

    if (options[["shiftedDistributionsIncluded"]]) {
      shiftedDistributions <- list(
        DistributionS7::shifted_exponential(lambda=1),
        DistributionS7::shifted_log_normal(mu=0, sigma=1),
        DistributionS7::shifted_gamma(alpha=2, theta=1),
        DistributionS7::shifted_inverse_gamma(alpha=2, theta=1),
        DistributionS7::shifted_log_logistic(mu=0, sigma=1),
        DistributionS7::shifted_wald(mu=1, lambda=1),
        DistributionS7::shifted_weibull(shape=2, scale=1)
      )
      distributions <- c(distributions, shiftedDistributions)
    }
  }

  return(distributions)
}

.ccdGetDistributionsManual <- function(jaspResults, variable, options) {
  syntax <- strsplit(options[["manualDistributionSpecification"]], "\n")[[1]]
  env <- asNamespace("DistributionS7")
  distributions <- lapply(syntax, function(x) {
    x <- parse(text=x)
    try(eval(x, envir=env))
  })
  return(distributions)
}


.ccdDistributionComparisonTable <- function(jaspResults, options) {
  if (!options[["comparisonTable"]]) return()

  table <- createJaspTable(
    title = gettext("Distribution comparison table"),
    dependencies = .ccdDependencies("comparisonTable", "comparisonTableOrder", "comparisonTableOrderBy")
  )
  table$addColumnInfo(name = "name",    title = gettext("Distribution"), type = "string")
  table$addColumnInfo(name = "n_par",   title = gettext("df"),           type = "integer")
  table$addColumnInfo(name = "n_obs",   title = gettext("n"),            type = "integer")
  table$addColumnInfo(name = "log_lik", title = gettext("Log. Lik"),     type = "number")
  table$addColumnInfo(name = "aic",     title = gettext("AIC"),          type = "number")
  table$addColumnInfo(name = "w_aic",   title = gettext("AIC weight"),   type = "number")
  table$addColumnInfo(name = "bic",     title = gettext("BIC"),          type = "number")
  table$addColumnInfo(name = "w_bic",   title = gettext("BIC weight"),   type = "number")

  return(table)
}

.ccdFillDistributionComparisonTable <- function(comparisonTable, options, distributions, variable) {
  if (is.null(comparisonTable)) return()

  results <- lapply(distributions, DistributionS7::information_criteria, data=variable)
  results <- do.call(rbind, results)

  results[["name"]]  <- vapply(distributions, DistributionS7::as_latex, character(1)) |> mathExpression()
  results[["w_aic"]] <- DistributionS7::weights_ic(results[["aic"]])
  results[["w_bic"]] <- DistributionS7::weights_ic(results[["bic"]])

  if (options[["comparisonTableOrder"]]) {
    order <- switch(
      options[["comparisonTableOrderBy"]],
      aic = order(results[["aic"]], decreasing = FALSE),
      bic = order(results[["bic"]], decreasing = FALSE),
      seq_len(nrow(results))
    )

    results <- results[order, , drop=FALSE]
  }

  comparisonTable$setData(results)

  return(distributions[order])
}

.ccdPerDistributionOutput <- function(jaspResults, options, distributions, variable) {
  if (options[["outputLimit"]] && options[["outputLimitTo"]] <= length(distributions))
    distributions <- distributions[seq_len(options[["outputLimitTo"]])]

  for (distribution in distributions)
    .ccdDistributionOutput(jaspResults, options, distribution, variable)
}

.ccdDistributionOutput <- function(jaspResults, options, distribution, variable) {
  distributionContainer <- jaspResults[[distribution@name]] %setOrRetrieve% createJaspContainer(
    title = distribution@name,
    dependencies = .ccdDependencies("outputLimit", "outputLimitTo", "comparisonTableOrder", "comparisonTableOrderBy"),
    initCollapsed = TRUE
  )

  .ccdParameterTable(distributionContainer, options, distribution, variable)
  .ccdGofTable(distributionContainer, options, distribution, variable)
  .ccdEmpiricalPlots(distributionContainer, options, distribution, variable)

}

.ccdParameterTable <- function(container, options, distribution, variable) {
  if (!options[["parameterEstimates"]]) return()
  if (!is.null(container[["parameterEstimates"]])) return()

  table <- createJaspTable(title = gettext("Parameter estimates"), dependencies = c("parameterEstimates"))

  table$addColumnInfo(name = "key",       title = gettext("Key"),       type = "string")
  table$addColumnInfo(name = "name",      title = gettext("Name"),      type = "string")
  table$addColumnInfo(name = "estimate",  title = gettext("Estimate"),  type = "number")

  results <- list()

  results[["key"]]  <- DistributionS7::parameter_properties(distribution, property="key",  which="free") |> unlist()
  results[["name"]] <- DistributionS7::parameter_properties(distribution, property="name", which="free") |> unlist()
  results[["estimate"]] <- DistributionS7::parameter_values(distribution, which="free") |> unlist()

  table$setData(results)

  container[["parameterEstimates"]] <- table
}

.ccdGofTable <- function(container, options, distribution, variable) {
  if (!options[["goodnessOfFit"]]) return()

  table <- createJaspTable(title = gettext("Goodness of fit"),
                           dependencies = c("goodnessOfFit", "goodnessOfFitBootstrap", "goodnessOfFitBootstrapSamples"))

  table$addColumnInfo(name = "test", title = gettext("Test"), type = "string")
  table$addColumnInfo(name = "statistic", title = gettext("Statistic"), type = "number")
  table$addColumnInfo(name = "p_value", title = gettext("p"), type="pvalue")

  container[["goodnessOfFit"]] <- table

  npar <- !(DistributionS7::parameter_properties(distribution, property="fixed") |> unlist())
  estimated <- sum(npar) > 0

  results <- try(DistributionS7::gof_test(distribution, variable, estimated=estimated))
  results[["test"]] <- .ccdGofTestLabels(results[["test"]])

  if(isTryError(results)) results <- NULL

  table$setData(results)
}

.ccdEmpiricalPlots <- function(distributionContainer, options, distribution, variable) {
  if (!options[["empiricalPlots"]]) return()

  container <- createJaspContainer(title = gettext("Empirical plots"),
                                   dependencies = c("empiricalPlots", "empiricalPlotsCi", "empiricalPlotsCiLevel"))

  container[["hist"]] <- createJaspPlot(
    title = gettext("Histogram vs. theoretical density"),
    plot = DistributionS7::plot_hist(distribution, variable, name=options[["variable"]])
  )
  container[["qq"]] <- createJaspPlot(
    title = gettext("Q-Q plot"),
    plot = DistributionS7::plot_qq(distribution, variable, ci = options[["empiricalPlotsCi"]], ci_level = options[["empiricalPlotsCiLevel"]])
  )
  container[["ecdf"]] <- createJaspPlot(
    title = gettext("Empirical vs. theoretical cumulative probability"),
    plot = DistributionS7::plot_ecdf(distribution, variable, name=options[["variable"]])
  )
  container[["pp"]] <- createJaspPlot(
    title = gettext("P-P plot"),
    plot = DistributionS7::plot_pp(distribution, variable, ci = options[["empiricalPlotsCi"]], ci_level = options[["empiricalPlotsCiLevel"]])
  )

  distributionContainer[["empiricalPlots"]] <- container
}

.ccdGofTestLabels <- function(keys) {
  labels <- list(
    ks_test     = gettext("Kolmogorov-Smirnov"),
    ad_test     = gettext("Anderson-Darling"),
    cvm_test    = gettext("Cramér–von Mises"),
    lillie_test = gettext("Lillienfors"),
    shapiro_wilk_test = gettext("Shapiro-Wilk"),
    shapiro_francia_test = gettext("Shapiro-Francia")
  )

  return(labels[keys])
}

.ccdDependencies <- function(...) {
  c("variable", "distributionSpecification", "manualDistributionSpecification", "shiftedDistributionsIncluded",
    "dataBoundedBelow", "dataBoundedBelowAt", "dataBoundedAbove", "dataBoundedAboveAt", ...)
}


