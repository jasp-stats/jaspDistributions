
compareContinuousDistributionsInternal <- function(jaspResults, dataset, options, state=NULL){
  comparisonTable <- jaspResults[["comparisonTable"]] %setOrRetrieve%
    .ccdDistributionComparisonTable(jaspResults, options)

  if (options[["variable"]] == "") return()
  if (options[["distributionSpecification"]] == "") return()

  variable <- na.omit(dataset[[options[["variable"]]]])

  distributions <- jaspResults[["distributions"]] %setOrRetrieve% (
    .ccdGetDistributions(jaspResults, variable, options) |>
    createJaspState(dependencies = .ccdDependencies())
  )

  # here the list gets possibly sorted by AIC/BIC values...
  distributions <- .ccdFillDistributionComparisonTable(comparisonTable, options, distributions, variable)

  .ccdPerDistributionOutput(jaspResults, options, distributions, variable)
}

.exportedDistributions <- c("Amoroso", "Beta", "BetaPrime", "Binomial", "Cauchy", "CentralF", "ChiSquared",
                            "Exponential", "Frechet", "Gamma", "Gompertz", "Gumbel", "InverseGamma", "Laplace", "Logistic",
                            "LogLogistic", "LogNormal", "NegativeBinomial", "NoncentralChiSquared", "NoncentralF",
                            "NoncentralStudentT", "NoncentralT", "Normal", "Pareto", "ShiftedExponential",
                            "ShiftedGamma", "ShiftedInverseGamma", "ShiftedLogLogistic", "ShiftedLogNormal", "ShiftedWald",
                            "ShiftedWeibull", "SkewCauchy", "SkewedGeneralizedT", "SkewNormal", "SkewT", "StandardNormal",
                            "StandardT", "StretchedBeta", "StudentT", "SymmetricGeneralizedNormal", "Triangular", "Uniform",
                            "Wald", "Weibull")

.getSafeEnv <- function() {
  packenv <- asNamespace("DistributionS7")
  env <- new.env(parent=emptyenv())
  names <- c(.exportedDistributions, "fixed", methods::getGroupMembers("Arith"))
  for (n in names) env[[n]] <- get(n, envir = packenv)
  return(env)
}

.ccdGetDistributions <- function(jaspResults, variable, options) {
  syntax <- strsplit(options[["distributionSpecification"]], "\n")[[1]]
  syntax <- unique(syntax)

  env <- .getSafeEnv()

  distributions <- lapply(syntax, function(txt) {
    expr <- parse(text=txt)[[1]]

    if (!as.character(expr[[1]]) %in% .exportedDistributions)
      jaspBase::.quitAnalysis(
        message = gettextf("Distribution definition was %1$s, but must be one of %2$s",
                           txt, paste(sprintf("`%s`", .exportedDistributions), collapse = ", "))
      )

    # make distribution object
    distribution <- try(eval(expr, envir=env))
    if (isTryError(distribution))
      jaspBase::.quitAnalysis(
        message = gettextf("Could not initialize distribution %1$s, with the following error: </br></br> %2$s",
                           txt, .extractErrorMessage(distribution)))

    # try fitting
    result <- try(DistributionS7::fit(
      distribution,
      estimator=DistributionS7::Mle(),
      data=variable
    ))

    # try manual starting values
    if (isTryError(result))
      result <- try(DistributionS7::fit(
        distribution,
        estimator=DistributionS7::Mle(start = DistributionS7::parameter_values(distribution, which="free")),
        data=variable
      ))

    if (isTryError(result))
      jaspBase::.quitAnalysis(
        message = gettextf("Could not fit distribution %1$s, with the following error: </br> %2$s. </br></br> You can try to change the initial parameter values, or remove the distribution from the distribution specification",
                           txt, .extractErrorMessage(result)))
    return(result)
  })
  distributions <- unique(distributions)


  return(distributions)
}


.ccdDistributionComparisonTable <- function(jaspResults, options) {
  if (!options[["comparisonTable"]]) return()

  table <- createJaspTable(
    title = gettext("Distribution comparison table"),
    dependencies = .ccdDependencies("comparisonTable", "comparisonTableOrder", "comparisonTableOrderBy")
  )
  table$showSpecifiedColumnsOnly <- TRUE
  table$addColumnInfo(name = "name",    title = gettext("Distribution"), type = "string")
  table$addColumnInfo(name = "w_aic",   title = gettext("AIC"),          type = "number", overtitle = gettext("Weights"))
  table$addColumnInfo(name = "w_bic",   title = gettext("BIC"),          type = "number", overtitle = gettext("Weights"))
  table$addColumnInfo(name = "aic",     title = gettext("AIC"),          type = "number")
  table$addColumnInfo(name = "bic",     title = gettext("BIC"),          type = "number")
  table$addColumnInfo(name = "log_lik", title = gettext("Log. Lik"),     type = "number")
  table$addColumnInfo(name = "n_par",   title = gettext("df"),           type = "integer")

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

  comparisonTable$title <- gettextf("Distribution comparison table (n=%1$i)", length(variable))

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
  distributionContainer <- jaspResults[[DistributionS7::as_latex(distribution)]] %setOrRetrieve% createJaspContainer(
    title = mathExpression(DistributionS7::as_latex(distribution)),
    dependencies = .ccdDependencies("outputLimit", "outputLimitTo", "comparisonTableOrder", "comparisonTableOrderBy"),
    initCollapsed = TRUE
  )

  .ccdParameterTable(distributionContainer, options, distribution, variable)
  .ccdGofTable      (distributionContainer, options, distribution, variable)
  .ccdEmpiricalPlots(distributionContainer, options, distribution, variable)

}

.ccdParameterTable <- function(container, options, distribution, variable) {
  if (!options[["parameterEstimates"]]) return()
  if (!is.null(container[["parameterEstimates"]])) return()

  table <- createJaspTable(title = gettext("Parameter estimates"), dependencies = c("parameterEstimates"))

  table$addColumnInfo(name = "key",       title = gettext("Key"),       type = "string")
  table$addColumnInfo(name = "name",      title = gettext("Name"),      type = "string")
  table$addColumnInfo(name = "label",     title = gettext("Label"),     type = "string")
  table$addColumnInfo(name = "estimate",  title = gettext("Estimate"),  type = "number")

  results <- list()

  results[["key"]]  <-  DistributionS7::parameter_properties(distribution, property="key",   which="free") |> unlist()
  results[["name"]] <-  DistributionS7::parameter_properties(distribution, property="name",  which="free") |> unlist()
  results[["label"]] <- DistributionS7::parameter_properties(distribution, property="label", which="free") |> unlist() |> mathExpression()
  results[["estimate"]] <- DistributionS7::parameter_values(distribution, which="free") |> unlist()

  table$setData(results)

  if (DistributionS7::nfixed(distribution) > 0) {
    parameters <- DistributionS7::parameters(distribution, which="fixed")
    output <- vapply(parameters, function(p) sprintf("%1$s = %2$s", p@label, format(p@value, digits=3)), character(1))
    output <- paste(output, collapse = ", ")
    output <- gettextf("The following parameters were held constant: %s.", mathExpression(output))
    table$addFootnote(message = output)
  }

  container[["parameterEstimates"]] <- table
}

.ccdGofTable <- function(container, options, distribution, variable) {
  if (!options[["goodnessOfFit"]]) return()
  if (!is.null(container[["goodnessOfFit"]])) return()

  table <- createJaspTable(title = gettext("Goodness of fit"),
                           dependencies = c("goodnessOfFit", "goodnessOfFitBootstrap", "goodnessOfFitBootstrapSamples"))

  table$addColumnInfo(name = "test",      title = gettext("Test"),      type = "string")
  table$addColumnInfo(name = "statistic", title = gettext("Statistic"), type = "number")
  table$addColumnInfo(name = "p_value",   title = gettext("p"),         type="pvalue")

  container[["goodnessOfFit"]] <- table

  samples <- if(options[["goodnessOfFitBootstrap"]]) options[["goodnessOfFitBootstrapSamples"]] else 0L
  if (samples > 0L) {
    table$addFootnote(message = gettextf("Based on %1$i parametric bootstrap samples.", samples), colNames = "p_value")
    jaspBase::startProgressbar(expectedTicks = samples, label = gettext("Bootstrapping..."))
  }


  results <- try(DistributionS7::gof_test(
    distribution, variable, estimated=TRUE,
    bootstrap=DistributionS7::Bootstrap(samples=samples, callback = jaspBase::progressbarTick)
    ))

  if (isTryError(results)) {
    table$setError(gettextf("Could not obtain goodness of fit: </br></br>: %s", .extractErrorMessage(results)))
    return()
  }
  results[["test"]] <- .ccdGofTestLabels(results[["test"]])



  table$setData(results)
}

.ccdEmpiricalPlots <- function(container, options, distribution, variable) {
  if (!options[["empiricalPlots"]]) return()
  if (!is.null(container[["empiricalPlots"]])) return()

  plotContainer <- createJaspContainer(title = gettext("Empirical plots"),
                                   dependencies = c("empiricalPlots", "empiricalPlotsCi", "empiricalPlotsCiLevel"))

  plotContainer[["hist"]] <- createJaspPlot(
    title = gettext("Histogram vs. theoretical density"),
    plot = DistributionS7::plot_hist(distribution, variable, name=options[["variable"]])
  )
  plotContainer[["qq"]] <- createJaspPlot(
    title = gettext("Q-Q plot"),
    plot = DistributionS7::plot_qq(distribution, variable, ci = options[["empiricalPlotsCi"]], ci_level = options[["empiricalPlotsCiLevel"]])
  )
  plotContainer[["ecdf"]] <- createJaspPlot(
    title = gettext("Empirical vs. theoretical cumulative probability"),
    plot = DistributionS7::plot_ecdf(distribution, variable, name=options[["variable"]])
  )
  plotContainer[["pp"]] <- createJaspPlot(
    title = gettext("P-P plot"),
    plot = DistributionS7::plot_pp(distribution, variable, ci = options[["empiricalPlotsCi"]], ci_level = options[["empiricalPlotsCiLevel"]])
  )

  container[["empiricalPlots"]] <- plotContainer
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
  c("variable", "distributionSpecification", "presetUnbounded", "presetShifted", "presetBounded", "presetBoundedMin",
    ...)
}


