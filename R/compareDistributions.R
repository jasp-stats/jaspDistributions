# todo:
# - fix skewed generalized t
# - check bounded distributions (uniform, triangular, stretched beta)

compareContinuousDistributionsInternal <- function(jaspResults, dataset, options, state=NULL){
  comparisonTable <- jaspResults[["comparisonTable"]] %setOrRetrieve%
    .ccdDistributionComparisonTable(jaspResults, options)

  if (options[["variable"]] == "") return()
  if (length(options[["distributions"]]) == 0L) return()

  variable <- na.omit(dataset[[options[["variable"]]]])

  distributions <- jaspResults[["distributions"]] %setOrRetrieve% (
    .ccdGetDistributions(jaspResults, variable, options) |>
    createJaspState(dependencies = .ccdDependencies())
  )

  # here the list gets possibly sorted by AIC/BIC values...
  distributions <- .ccdFillDistributionComparisonTable(comparisonTable, options, distributions, variable)

  .ccdPerDistributionOutput(jaspResults, options, distributions, variable)
}

.ccdGetDistributions <- function(jaspResults, variable, options) {
  distributions <- lapply(unique(options[["distributions"]]), function(specification) {
    distribution <- try(.makeDistribution(specification))

    # make distribution object
    if (isTryError(distribution))
      jaspBase::.quitAnalysis(
        message = gettextf("Could not initialize distribution %1$s, with the following error: </br></br> %2$s",
                           specification[["distribution"]], .extractErrorMessage(distribution)))

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
                           specification[["distribution"]], .extractErrorMessage(result)))
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

  for (i in seq_along(distributions))
    .ccdDistributionOutput(jaspResults, options, distributions[[i]], variable, sprintf("distribution%i", i))
}

.ccdDistributionOutput <- function(jaspResults, options, distribution, variable, name) {
  distributionContainer <- jaspResults[[name]] %setOrRetrieve% createJaspContainer(
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

  if (DistributionS7::nfree(distribution) > 0L) {
    results <- list()

    results[["key"]]  <-  DistributionS7::parameter_properties(distribution, property="key",   which="free") |> unlist()
    results[["name"]] <-  DistributionS7::parameter_properties(distribution, property="name",  which="free") |> unlist()
    results[["label"]] <- DistributionS7::parameter_properties(distribution, property="label", which="free") |> unlist() |> mathExpression()
    results[["estimate"]] <- DistributionS7::parameter_values(distribution, which="free") |> unlist()

    table$setData(results)
  } else {
    table$addFootnote(message = gettext("Distribution has no parameters to estimate."))
  }


  if (DistributionS7::nfixed(distribution) > 0L) {
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
    lillie_test = gettext("Lilliefors"),
    shapiro_wilk_test = gettext("Shapiro-Wilk"),
    shapiro_francia_test = gettext("Shapiro-Francia")
  )

  return(labels[keys])
}

.ccdDependencies <- function(...) {
  c("variable", "distributions", ...)
}

.makeDistribution <- function(specification) {
  switch(
    specification[["distribution"]],
    Normal                     = .makeNormal(specification),
    StandardNormal             = .makeStandardNormal(specification),
    StandardT                  = .makeStandardT(specification),
    StudentT                   = .makeStudentT(specification),
    NoncentralT                = .makeNoncentralT(specification),
    NoncentralStudentT         = .makeNoncentralStudentT(specification),
    Cauchy                     = .makeCauchy(specification),
    Gumbel                     = .makeGumbel(specification),
    Laplace                    = .makeLaplace(specification),
    Logistic                   = .makeLogistic(specification),
    SkewedGeneralizedT         = .makeSkewedGeneralizedT(specification),
    SymmetricGeneralizedNormal = .makeSymmetricGeneralizedNormal(specification),
    SkewNormal                 = .makeSkewNormal(specification),
    SkewCauchy                 = .makeSkewCauchy(specification),
    SkewT                      = .makeSkewT(specification),
    Beta                       = .makeBeta(specification),
    BetaPrime                  = .makeBetaPrime(specification),
    CentralF                   = .makeCentralF(specification),
    NoncentralF                = .makeNoncentralF(specification),
    ChiSquared                 = .makeChiSquared(specification),
    NoncentralChiSquared       = .makeNoncentralChiSquared(specification),
    Exponential                = .makeExponential(specification),
    Gamma                      = .makeGamma(specification),
    InverseGamma               = .makeInverseGamma(specification),
    Gompertz                   = .makeGompertz(specification),
    LogLogistic                = .makeLogLogistic(specification),
    LogNormal                  = .makeLogNormal(specification),
    Wald                       = .makeWald(specification),
    Weibull                    = .makeWeibull(specification),
    Amoroso                    = .makeAmoroso(specification),
    StretchedBeta              = .makeStretchedBeta(specification),
    Frechet                    = .makeFrechet(specification),
    Pareto                     = .makePareto(specification),
    Triangular                 = .makeTriangular(specification),
    Uniform                    = .makeUniform(specification),
    ShiftedExponential         = .makeShiftedExponential(specification),
    ShiftedLogNormal           = .makeShiftedLogNormal(specification),
    ShiftedGamma               = .makeShiftedGamma(specification),
    ShiftedInverseGamma        = .makeShiftedInverseGamma(specification),
    ShiftedLogLogistic         = .makeShiftedLogLogistic(specification),
    ShiftedWald                = .makeShiftedWald(specification),
    ShiftedWeibull             = .makeShiftedWeibull(specification)
  )
}

.makeNormal <- function(specification) {
  parameters <- list()
  parameters[["mu"]] <- if (specification[["normalMuFixed"]]) DistributionS7::fixed(specification[["normalMu"]]) else specification[["normalMu"]]
  if (specification[["parameters"]] == "sigma")
    parameters[["sigma"]] <- if (specification[["normalSigmaFixed"]]) DistributionS7::fixed(specification[["normalSigma"]]) else specification[["normalSigma"]]
  if (specification[["parameters"]] == "sigma2")
    parameters[["sigma2"]] <- if (specification[["normalSigma2Fixed"]]) DistributionS7::fixed(specification[["normalSigma2"]]) else specification[["normalSigma2"]]
  if (specification[["parameters"]] == "tau")
    parameters[["tau"]] <- if (specification[["normalTauFixed"]]) DistributionS7::fixed(specification[["normalTau"]]) else specification[["normalTau"]]
  do.call(DistributionS7::Normal, parameters)
}

.makeStandardNormal <- function(specification) {
  DistributionS7::StandardNormal()
}

.makeStandardT <- function(specification) {
  parameters <- list()
  parameters[["nu"]] <- if (specification[["standardTNuFixed"]]) DistributionS7::fixed(specification[["standardTNu"]]) else specification[["standardTNu"]]
  do.call(DistributionS7::StandardT, parameters)
}

.makeStudentT <- function(specification) {
  parameters <- list()
  parameters[["nu"]]    <- if (specification[["studentTNuFixed"]])    DistributionS7::fixed(specification[["studentTNu"]])    else specification[["studentTNu"]]
  parameters[["mu"]]    <- if (specification[["studentTMuFixed"]])    DistributionS7::fixed(specification[["studentTMu"]])    else specification[["studentTMu"]]
  parameters[["sigma"]] <- if (specification[["studentTSigmaFixed"]]) DistributionS7::fixed(specification[["studentTSigma"]]) else specification[["studentTSigma"]]
  do.call(DistributionS7::StudentT, parameters)
}

.makeNoncentralT <- function(specification) {
  parameters <- list()
  parameters[["nu"]]    <- if (specification[["noncentralTNuFixed"]])    DistributionS7::fixed(specification[["noncentralTNu"]])    else specification[["noncentralTNu"]]
  parameters[["kappa"]] <- if (specification[["noncentralTKappaFixed"]]) DistributionS7::fixed(specification[["noncentralTKappa"]]) else specification[["noncentralTKappa"]]
  do.call(DistributionS7::NoncentralT, parameters)
}

.makeNoncentralStudentT <- function(specification) {
  parameters <- list()
  parameters[["nu"]]    <- if (specification[["noncentralStudentTNuFixed"]])    DistributionS7::fixed(specification[["noncentralStudentTNu"]])    else specification[["noncentralStudentTNu"]]
  parameters[["kappa"]] <- if (specification[["noncentralStudentTKappaFixed"]]) DistributionS7::fixed(specification[["noncentralStudentTKappa"]]) else specification[["noncentralStudentTKappa"]]
  parameters[["mu"]]    <- if (specification[["noncentralStudentTMuFixed"]])    DistributionS7::fixed(specification[["noncentralStudentTMu"]])    else specification[["noncentralStudentTMu"]]
  parameters[["sigma"]] <- if (specification[["noncentralStudentTSigmaFixed"]]) DistributionS7::fixed(specification[["noncentralStudentTSigma"]]) else specification[["noncentralStudentTSigma"]]
  do.call(DistributionS7::NoncentralStudentT, parameters)
}

.makeCauchy <- function(specification) {
  parameters <- list()
  parameters[["mu"]]    <- if (specification[["cauchyMuFixed"]])    DistributionS7::fixed(specification[["cauchyMu"]])    else specification[["cauchyMu"]]
  parameters[["sigma"]] <- if (specification[["cauchySigmaFixed"]]) DistributionS7::fixed(specification[["cauchySigma"]]) else specification[["cauchySigma"]]
  do.call(DistributionS7::Cauchy, parameters)
}

.makeGumbel <- function(specification) {
  parameters <- list()
  parameters[["mu"]]   <- if (specification[["gumbelMuFixed"]])   DistributionS7::fixed(specification[["gumbelMu"]])   else specification[["gumbelMu"]]
  parameters[["beta"]] <- if (specification[["gumbelBetaFixed"]]) DistributionS7::fixed(specification[["gumbelBeta"]]) else specification[["gumbelBeta"]]
  do.call(DistributionS7::Gumbel, parameters)
}

.makeLaplace <- function(specification) {
  parameters <- list()
  parameters[["mu"]]   <- if (specification[["laplaceMuFixed"]])   DistributionS7::fixed(specification[["laplaceMu"]])   else specification[["laplaceMu"]]
  parameters[["beta"]] <- if (specification[["laplaceBetaFixed"]]) DistributionS7::fixed(specification[["laplaceBeta"]]) else specification[["laplaceBeta"]]
  do.call(DistributionS7::Laplace, parameters)
}

.makeLogistic <- function(specification) {
  parameters <- list()
  parameters[["mu"]]    <- if (specification[["logisticMuFixed"]])    DistributionS7::fixed(specification[["logisticMu"]])    else specification[["logisticMu"]]
  parameters[["sigma"]] <- if (specification[["logisticSigmaFixed"]]) DistributionS7::fixed(specification[["logisticSigma"]]) else specification[["logisticSigma"]]
  do.call(DistributionS7::Logistic, parameters)
}

.makeSkewedGeneralizedT <- function(specification) {
  parameters <- list()
  parameters[["mu"]]     <- if (specification[["skewedGeneralizedTMuFixed"]])     DistributionS7::fixed(specification[["skewedGeneralizedTMu"]])     else specification[["skewedGeneralizedTMu"]]
  parameters[["sigma"]]  <- if (specification[["skewedGeneralizedTSigmaFixed"]])  DistributionS7::fixed(specification[["skewedGeneralizedTSigma"]])  else specification[["skewedGeneralizedTSigma"]]
  parameters[["lambda"]] <- if (specification[["skewedGeneralizedTLambdaFixed"]]) DistributionS7::fixed(specification[["skewedGeneralizedTLambda"]]) else specification[["skewedGeneralizedTLambda"]]
  parameters[["p"]]      <- if (specification[["skewedGeneralizedTPFixed"]])      DistributionS7::fixed(specification[["skewedGeneralizedTP"]])      else specification[["skewedGeneralizedTP"]]
  parameters[["q"]]      <- if (specification[["skewedGeneralizedTQFixed"]])      DistributionS7::fixed(specification[["skewedGeneralizedTQ"]])      else specification[["skewedGeneralizedTQ"]]
  do.call(DistributionS7::SkewedGeneralizedT, parameters)
}

.makeSymmetricGeneralizedNormal <- function(specification) {
  parameters <- list()
  parameters[["mu"]]    <- if (specification[["symmetricGeneralizedNormalMuFixed"]])    DistributionS7::fixed(specification[["symmetricGeneralizedNormalMu"]])    else specification[["symmetricGeneralizedNormalMu"]]
  parameters[["alpha"]] <- if (specification[["symmetricGeneralizedNormalAlphaFixed"]]) DistributionS7::fixed(specification[["symmetricGeneralizedNormalAlpha"]]) else specification[["symmetricGeneralizedNormalAlpha"]]
  parameters[["beta"]]  <- if (specification[["symmetricGeneralizedNormalBetaFixed"]])  DistributionS7::fixed(specification[["symmetricGeneralizedNormalBeta"]])  else specification[["symmetricGeneralizedNormalBeta"]]
  do.call(DistributionS7::SymmetricGeneralizedNormal, parameters)
}

.makeSkewNormal <- function(specification) {
  parameters <- list()
  parameters[["xi"]]    <- if (specification[["skewNormalXiFixed"]])    DistributionS7::fixed(specification[["skewNormalXi"]])    else specification[["skewNormalXi"]]
  parameters[["omega"]] <- if (specification[["skewNormalOmegaFixed"]]) DistributionS7::fixed(specification[["skewNormalOmega"]]) else specification[["skewNormalOmega"]]
  parameters[["alpha"]] <- if (specification[["skewNormalAlphaFixed"]]) DistributionS7::fixed(specification[["skewNormalAlpha"]]) else specification[["skewNormalAlpha"]]
  do.call(DistributionS7::SkewNormal, parameters)
}

.makeSkewCauchy <- function(specification) {
  parameters <- list()
  parameters[["xi"]]    <- if (specification[["skewCauchyXiFixed"]])    DistributionS7::fixed(specification[["skewCauchyXi"]])    else specification[["skewCauchyXi"]]
  parameters[["omega"]] <- if (specification[["skewCauchyOmegaFixed"]]) DistributionS7::fixed(specification[["skewCauchyOmega"]]) else specification[["skewCauchyOmega"]]
  parameters[["alpha"]] <- if (specification[["skewCauchyAlphaFixed"]]) DistributionS7::fixed(specification[["skewCauchyAlpha"]]) else specification[["skewCauchyAlpha"]]
  do.call(DistributionS7::SkewCauchy, parameters)
}

.makeSkewT <- function(specification) {
  parameters <- list()
  parameters[["xi"]]    <- if (specification[["skewTXiFixed"]])    DistributionS7::fixed(specification[["skewTXi"]])    else specification[["skewTXi"]]
  parameters[["omega"]] <- if (specification[["skewTOmegaFixed"]]) DistributionS7::fixed(specification[["skewTOmega"]]) else specification[["skewTOmega"]]
  parameters[["alpha"]] <- if (specification[["skewTAlphaFixed"]]) DistributionS7::fixed(specification[["skewTAlpha"]]) else specification[["skewTAlpha"]]
  parameters[["nu"]]    <- if (specification[["skewTNuFixed"]])    DistributionS7::fixed(specification[["skewTNu"]])    else specification[["skewTNu"]]
  do.call(DistributionS7::SkewT, parameters)
}

.makeBeta <- function(specification) {
  parameters <- list()
  parameters[["alpha"]] <- if (specification[["betaAlphaFixed"]]) DistributionS7::fixed(specification[["betaAlpha"]]) else specification[["betaAlpha"]]
  parameters[["beta"]]  <- if (specification[["betaBetaFixed"]])  DistributionS7::fixed(specification[["betaBeta"]])  else specification[["betaBeta"]]
  do.call(DistributionS7::Beta, parameters)
}

.makeBetaPrime <- function(specification) {
  parameters <- list()
  parameters[["alpha"]] <- if (specification[["betaPrimeAlphaFixed"]]) DistributionS7::fixed(specification[["betaPrimeAlpha"]]) else specification[["betaPrimeAlpha"]]
  parameters[["beta"]]  <- if (specification[["betaPrimeBetaFixed"]])  DistributionS7::fixed(specification[["betaPrimeBeta"]])  else specification[["betaPrimeBeta"]]
  do.call(DistributionS7::BetaPrime, parameters)
}

.makeCentralF <- function(specification) {
  parameters <- list()
  parameters[["nu1"]] <- if (specification[["centralFNu1Fixed"]]) DistributionS7::fixed(specification[["centralFNu1"]]) else specification[["centralFNu1"]]
  parameters[["nu2"]] <- if (specification[["centralFNu2Fixed"]]) DistributionS7::fixed(specification[["centralFNu2"]]) else specification[["centralFNu2"]]
  do.call(DistributionS7::CentralF, parameters)
}

.makeNoncentralF <- function(specification) {
  parameters <- list()
  parameters[["nu1"]]   <- if (specification[["noncentralFNu1Fixed"]])   DistributionS7::fixed(specification[["noncentralFNu1"]])   else specification[["noncentralFNu1"]]
  parameters[["nu2"]]   <- if (specification[["noncentralFNu2Fixed"]])   DistributionS7::fixed(specification[["noncentralFNu2"]])   else specification[["noncentralFNu2"]]
  parameters[["kappa"]] <- if (specification[["noncentralFKappaFixed"]]) DistributionS7::fixed(specification[["noncentralFKappa"]]) else specification[["noncentralFKappa"]]
  do.call(DistributionS7::NoncentralF, parameters)
}

.makeChiSquared <- function(specification) {
  parameters <- list()
  parameters[["nu"]] <- if (specification[["chiSquaredNuFixed"]]) DistributionS7::fixed(specification[["chiSquaredNu"]]) else specification[["chiSquaredNu"]]
  do.call(DistributionS7::ChiSquared, parameters)
}

.makeNoncentralChiSquared <- function(specification) {
  parameters <- list()
  parameters[["nu"]]    <- if (specification[["noncentralChiSquaredNuFixed"]])    DistributionS7::fixed(specification[["noncentralChiSquaredNu"]])    else specification[["noncentralChiSquaredNu"]]
  parameters[["kappa"]] <- if (specification[["noncentralChiSquaredKappaFixed"]]) DistributionS7::fixed(specification[["noncentralChiSquaredKappa"]]) else specification[["noncentralChiSquaredKappa"]]
  do.call(DistributionS7::NoncentralChiSquared, parameters)
}

.makeExponential <- function(specification) {
  parameters <- list()
  if (specification[["parameters"]] == "lambda")
    parameters[["lambda"]] <- if (specification[["exponentialLambdaFixed"]]) DistributionS7::fixed(specification[["exponentialLambda"]]) else specification[["exponentialLambda"]]
  if (specification[["parameters"]] == "beta")
    parameters[["beta"]] <- if (specification[["exponentialBetaFixed"]]) DistributionS7::fixed(specification[["exponentialBeta"]]) else specification[["exponentialBeta"]]
  do.call(DistributionS7::Exponential, parameters)
}

.makeGamma <- function(specification) {
  parameters <- list()
  parameters[["alpha"]] <- if (specification[["gammaAlphaFixed"]]) DistributionS7::fixed(specification[["gammaAlpha"]]) else specification[["gammaAlpha"]]
  if (specification[["parameters"]] == "theta")
    parameters[["theta"]] <- if (specification[["gammaThetaFixed"]]) DistributionS7::fixed(specification[["gammaTheta"]]) else specification[["gammaTheta"]]
  if (specification[["parameters"]] == "lambda")
    parameters[["lambda"]] <- if (specification[["gammaLambdaFixed"]]) DistributionS7::fixed(specification[["gammaLambda"]]) else specification[["gammaLambda"]]
  if (specification[["parameters"]] == "mu")
    parameters[["mu"]] <- if (specification[["gammaMuFixed"]]) DistributionS7::fixed(specification[["gammaMu"]]) else specification[["gammaMu"]]
  do.call(DistributionS7::Gamma, parameters)
}

.makeInverseGamma <- function(specification) {
  parameters <- list()
  parameters[["alpha"]] <- if (specification[["inverseGammaAlphaFixed"]]) DistributionS7::fixed(specification[["inverseGammaAlpha"]]) else specification[["inverseGammaAlpha"]]
  if (specification[["parameters"]] == "theta")
    parameters[["theta"]] <- if (specification[["inverseGammaThetaFixed"]]) DistributionS7::fixed(specification[["inverseGammaTheta"]]) else specification[["inverseGammaTheta"]]
  if (specification[["parameters"]] == "lambda")
    parameters[["lambda"]] <- if (specification[["inverseGammaLambdaFixed"]]) DistributionS7::fixed(specification[["inverseGammaLambda"]]) else specification[["inverseGammaLambda"]]
  if (specification[["parameters"]] == "mu")
    parameters[["mu"]] <- if (specification[["inverseGammaMuFixed"]]) DistributionS7::fixed(specification[["inverseGammaMu"]]) else specification[["inverseGammaMu"]]
  do.call(DistributionS7::InverseGamma, parameters)
}

.makeGompertz <- function(specification) {
  parameters <- list()
  parameters[["eta"]]  <- if (specification[["gompertzEtaFixed"]])  DistributionS7::fixed(specification[["gompertzEta"]])  else specification[["gompertzEta"]]
  parameters[["beta"]] <- if (specification[["gompertzBetaFixed"]]) DistributionS7::fixed(specification[["gompertzBeta"]]) else specification[["gompertzBeta"]]
  do.call(DistributionS7::Gompertz, parameters)
}

.makeLogLogistic <- function(specification) {
  parameters <- list()
  if (specification[["parameters"]] == "mu") {
    parameters[["mu"]]    <- if (specification[["logLogisticMuFixed"]])    DistributionS7::fixed(specification[["logLogisticMu"]])    else specification[["logLogisticMu"]]
    parameters[["sigma"]] <- if (specification[["logLogisticSigmaFixed"]]) DistributionS7::fixed(specification[["logLogisticSigma"]]) else specification[["logLogisticSigma"]]
  } else {
    parameters[["alpha"]] <- if (specification[["logLogisticAlphaFixed"]]) DistributionS7::fixed(specification[["logLogisticAlpha"]]) else specification[["logLogisticAlpha"]]
    parameters[["beta"]]  <- if (specification[["logLogisticBetaFixed"]])  DistributionS7::fixed(specification[["logLogisticBeta"]])  else specification[["logLogisticBeta"]]
  }
  do.call(DistributionS7::LogLogistic, parameters)
}

.makeLogNormal <- function(specification) {
  parameters <- list()
  parameters[["mu"]]    <- if (specification[["logNormalMuFixed"]])    DistributionS7::fixed(specification[["logNormalMu"]])    else specification[["logNormalMu"]]
  parameters[["sigma"]] <- if (specification[["logNormalSigmaFixed"]]) DistributionS7::fixed(specification[["logNormalSigma"]]) else specification[["logNormalSigma"]]
  do.call(DistributionS7::LogNormal, parameters)
}

.makeWald <- function(specification) {
  parameters <- list()
  if (specification[["parameters"]] == "mu") {
    parameters[["mu"]]     <- if (specification[["waldMuFixed"]])     DistributionS7::fixed(specification[["waldMu"]])     else specification[["waldMu"]]
    parameters[["lambda"]] <- if (specification[["waldLambdaFixed"]]) DistributionS7::fixed(specification[["waldLambda"]]) else specification[["waldLambda"]]
  } else {
    parameters[["nu"]]    <- if (specification[["waldNuFixed"]])    DistributionS7::fixed(specification[["waldNu"]])    else specification[["waldNu"]]
    parameters[["alpha"]] <- if (specification[["waldAlphaFixed"]]) DistributionS7::fixed(specification[["waldAlpha"]]) else specification[["waldAlpha"]]
    parameters[["sigma"]] <- if (specification[["waldSigmaFixed"]]) DistributionS7::fixed(specification[["waldSigma"]]) else specification[["waldSigma"]]
  }
  do.call(DistributionS7::Wald, parameters)
}

.makeWeibull <- function(specification) {
  parameters <- list()
  parameters[["shape"]] <- if (specification[["weibullShapeFixed"]]) DistributionS7::fixed(specification[["weibullShape"]]) else specification[["weibullShape"]]
  parameters[["scale"]] <- if (specification[["weibullScaleFixed"]]) DistributionS7::fixed(specification[["weibullScale"]]) else specification[["weibullScale"]]
  do.call(DistributionS7::Weibull, parameters)
}

.makeAmoroso <- function(specification) {
  parameters <- list()
  parameters[["a"]]     <- if (specification[["amorosoAFixed"]])     DistributionS7::fixed(specification[["amorosoA"]])     else specification[["amorosoA"]]
  parameters[["theta"]] <- if (specification[["amorosoThetaFixed"]]) DistributionS7::fixed(specification[["amorosoTheta"]]) else specification[["amorosoTheta"]]
  parameters[["alpha"]] <- if (specification[["amorosoAlphaFixed"]]) DistributionS7::fixed(specification[["amorosoAlpha"]]) else specification[["amorosoAlpha"]]
  parameters[["beta"]]  <- if (specification[["amorosoBetaFixed"]])  DistributionS7::fixed(specification[["amorosoBeta"]])  else specification[["amorosoBeta"]]
  do.call(DistributionS7::Amoroso, parameters)
}

.makeStretchedBeta <- function(specification) {
  parameters <- list()
  parameters[["alpha"]] <- if (specification[["stretchedBetaAlphaFixed"]]) DistributionS7::fixed(specification[["stretchedBetaAlpha"]]) else specification[["stretchedBetaAlpha"]]
  parameters[["beta"]]  <- if (specification[["stretchedBetaBetaFixed"]])  DistributionS7::fixed(specification[["stretchedBetaBeta"]])  else specification[["stretchedBetaBeta"]]
  parameters[["min"]]   <- DistributionS7::fixed(specification[["stretchedBetaMin"]])
  parameters[["max"]]   <- DistributionS7::fixed(specification[["stretchedBetaMax"]])
  do.call(DistributionS7::StretchedBeta, parameters)
}

.makeFrechet <- function(specification) {
  parameters <- list()
  parameters[["alpha"]] <- if (specification[["frechetAlphaFixed"]]) DistributionS7::fixed(specification[["frechetAlpha"]]) else specification[["frechetAlpha"]]
  parameters[["sigma"]] <- if (specification[["frechetSigmaFixed"]]) DistributionS7::fixed(specification[["frechetSigma"]]) else specification[["frechetSigma"]]
  parameters[["theta"]] <- if (specification[["frechetThetaFixed"]]) DistributionS7::fixed(specification[["frechetTheta"]]) else specification[["frechetTheta"]]
  do.call(DistributionS7::Frechet, parameters)
}

.makePareto <- function(specification) {
  parameters <- list()
  parameters[["alpha"]] <- if (specification[["paretoAlphaFixed"]]) DistributionS7::fixed(specification[["paretoAlpha"]]) else specification[["paretoAlpha"]]
  parameters[["beta"]]  <- if (specification[["paretoBetaFixed"]])  DistributionS7::fixed(specification[["paretoBeta"]])  else specification[["paretoBeta"]]
  do.call(DistributionS7::Pareto, parameters)
}

.makeTriangular <- function(specification) {
  parameters <- list()
  parameters[["a"]] <- if (specification[["triangularAFixed"]]) DistributionS7::fixed(specification[["triangularA"]]) else specification[["triangularA"]]
  parameters[["b"]] <- if (specification[["triangularBFixed"]]) DistributionS7::fixed(specification[["triangularB"]]) else specification[["triangularB"]]
  parameters[["c"]] <- if (specification[["triangularCFixed"]]) DistributionS7::fixed(specification[["triangularC"]]) else specification[["triangularC"]]
  do.call(DistributionS7::Triangular, parameters)
}

.makeUniform <- function(specification) {
  parameters <- list()
  parameters[["min"]] <- if (specification[["uniformMinFixed"]]) DistributionS7::fixed(specification[["uniformMin"]]) else specification[["uniformMin"]]
  parameters[["max"]] <- if (specification[["uniformMaxFixed"]]) DistributionS7::fixed(specification[["uniformMax"]]) else specification[["uniformMax"]]
  do.call(DistributionS7::Uniform, parameters)
}

.makeShiftedExponential <- function(specification) {
  parameters <- list()
  if (specification[["parameters"]] == "lambda")
    parameters[["lambda"]] <- if (specification[["shiftedExponentialLambdaFixed"]]) DistributionS7::fixed(specification[["shiftedExponentialLambda"]]) else specification[["shiftedExponentialLambda"]]
  if (specification[["parameters"]] == "beta")
    parameters[["beta"]] <- if (specification[["shiftedExponentialBetaFixed"]]) DistributionS7::fixed(specification[["shiftedExponentialBeta"]]) else specification[["shiftedExponentialBeta"]]
  parameters[["shift"]] <- if (specification[["shiftedExponentialShiftFixed"]]) DistributionS7::fixed(specification[["shiftedExponentialShift"]]) else specification[["shiftedExponentialShift"]]
  do.call(DistributionS7::ShiftedExponential, parameters)
}

.makeShiftedLogNormal <- function(specification) {
  parameters <- list()
  parameters[["mu"]]    <- if (specification[["shiftedLogNormalMuFixed"]])    DistributionS7::fixed(specification[["shiftedLogNormalMu"]])    else specification[["shiftedLogNormalMu"]]
  parameters[["sigma"]] <- if (specification[["shiftedLogNormalSigmaFixed"]]) DistributionS7::fixed(specification[["shiftedLogNormalSigma"]]) else specification[["shiftedLogNormalSigma"]]
  parameters[["shift"]] <- if (specification[["shiftedLogNormalShiftFixed"]]) DistributionS7::fixed(specification[["shiftedLogNormalShift"]]) else specification[["shiftedLogNormalShift"]]
  do.call(DistributionS7::ShiftedLogNormal, parameters)
}

.makeShiftedGamma <- function(specification) {
  parameters <- list()
  parameters[["alpha"]] <- if (specification[["shiftedGammaAlphaFixed"]]) DistributionS7::fixed(specification[["shiftedGammaAlpha"]]) else specification[["shiftedGammaAlpha"]]
  if (specification[["parameters"]] == "theta")
    parameters[["theta"]] <- if (specification[["shiftedGammaThetaFixed"]]) DistributionS7::fixed(specification[["shiftedGammaTheta"]]) else specification[["shiftedGammaTheta"]]
  if (specification[["parameters"]] == "lambda")
    parameters[["lambda"]] <- if (specification[["shiftedGammaLambdaFixed"]]) DistributionS7::fixed(specification[["shiftedGammaLambda"]]) else specification[["shiftedGammaLambda"]]
  if (specification[["parameters"]] == "mu")
    parameters[["mu"]] <- if (specification[["shiftedGammaMuFixed"]]) DistributionS7::fixed(specification[["shiftedGammaMu"]]) else specification[["shiftedGammaMu"]]
  parameters[["shift"]] <- if (specification[["shiftedGammaShiftFixed"]]) DistributionS7::fixed(specification[["shiftedGammaShift"]]) else specification[["shiftedGammaShift"]]
  do.call(DistributionS7::ShiftedGamma, parameters)
}

.makeShiftedInverseGamma <- function(specification) {
  parameters <- list()
  parameters[["alpha"]] <- if (specification[["shiftedInverseGammaAlphaFixed"]]) DistributionS7::fixed(specification[["shiftedInverseGammaAlpha"]]) else specification[["shiftedInverseGammaAlpha"]]
  if (specification[["parameters"]] == "theta")
    parameters[["theta"]] <- if (specification[["shiftedInverseGammaThetaFixed"]]) DistributionS7::fixed(specification[["shiftedInverseGammaTheta"]]) else specification[["shiftedInverseGammaTheta"]]
  if (specification[["parameters"]] == "lambda")
    parameters[["lambda"]] <- if (specification[["shiftedInverseGammaLambdaFixed"]]) DistributionS7::fixed(specification[["shiftedInverseGammaLambda"]]) else specification[["shiftedInverseGammaLambda"]]
  if (specification[["parameters"]] == "mu")
    parameters[["mu"]] <- if (specification[["shiftedInverseGammaMuFixed"]]) DistributionS7::fixed(specification[["shiftedInverseGammaMu"]]) else specification[["shiftedInverseGammaMu"]]
  parameters[["shift"]] <- if (specification[["shiftedInverseGammaShiftFixed"]]) DistributionS7::fixed(specification[["shiftedInverseGammaShift"]]) else specification[["shiftedInverseGammaShift"]]
  do.call(DistributionS7::ShiftedInverseGamma, parameters)
}

.makeShiftedLogLogistic <- function(specification) {
  parameters <- list()
  if (specification[["parameters"]] == "mu") {
    parameters[["mu"]]    <- if (specification[["shiftedLogLogisticMuFixed"]])    DistributionS7::fixed(specification[["shiftedLogLogisticMu"]])    else specification[["shiftedLogLogisticMu"]]
    parameters[["sigma"]] <- if (specification[["shiftedLogLogisticSigmaFixed"]]) DistributionS7::fixed(specification[["shiftedLogLogisticSigma"]]) else specification[["shiftedLogLogisticSigma"]]
  } else {
    parameters[["alpha"]] <- if (specification[["shiftedLogLogisticAlphaFixed"]]) DistributionS7::fixed(specification[["shiftedLogLogisticAlpha"]]) else specification[["shiftedLogLogisticAlpha"]]
    parameters[["beta"]]  <- if (specification[["shiftedLogLogisticBetaFixed"]])  DistributionS7::fixed(specification[["shiftedLogLogisticBeta"]])  else specification[["shiftedLogLogisticBeta"]]
  }
  parameters[["shift"]] <- if (specification[["shiftedLogLogisticShiftFixed"]]) DistributionS7::fixed(specification[["shiftedLogLogisticShift"]]) else specification[["shiftedLogLogisticShift"]]
  do.call(DistributionS7::ShiftedLogLogistic, parameters)
}

.makeShiftedWald <- function(specification) {
  parameters <- list()
  if (specification[["parameters"]] == "mu") {
    parameters[["mu"]]     <- if (specification[["shiftedWaldMuFixed"]])     DistributionS7::fixed(specification[["shiftedWaldMu"]])     else specification[["shiftedWaldMu"]]
    parameters[["lambda"]] <- if (specification[["shiftedWaldLambdaFixed"]]) DistributionS7::fixed(specification[["shiftedWaldLambda"]]) else specification[["shiftedWaldLambda"]]
  } else {
    parameters[["nu"]]    <- if (specification[["shiftedWaldNuFixed"]])    DistributionS7::fixed(specification[["shiftedWaldNu"]])    else specification[["shiftedWaldNu"]]
    parameters[["alpha"]] <- if (specification[["shiftedWaldAlphaFixed"]]) DistributionS7::fixed(specification[["shiftedWaldAlpha"]]) else specification[["shiftedWaldAlpha"]]
    parameters[["sigma"]] <- if (specification[["shiftedWaldSigmaFixed"]]) DistributionS7::fixed(specification[["shiftedWaldSigma"]]) else specification[["shiftedWaldSigma"]]
  }
  parameters[["shift"]] <- if (specification[["shiftedWaldShiftFixed"]]) DistributionS7::fixed(specification[["shiftedWaldShift"]]) else specification[["shiftedWaldShift"]]
  do.call(DistributionS7::ShiftedWald, parameters)
}

.makeShiftedWeibull <- function(specification) {
  parameters <- list()
  parameters[["shape"]] <- if (specification[["shiftedWeibullShapeFixed"]]) DistributionS7::fixed(specification[["shiftedWeibullShape"]]) else specification[["shiftedWeibullShape"]]
  parameters[["scale"]] <- if (specification[["shiftedWeibullScaleFixed"]]) DistributionS7::fixed(specification[["shiftedWeibullScale"]]) else specification[["shiftedWeibullScale"]]
  parameters[["shift"]] <- if (specification[["shiftedWeibullShiftFixed"]]) DistributionS7::fixed(specification[["shiftedWeibullShift"]]) else specification[["shiftedWeibullShift"]]
  do.call(DistributionS7::ShiftedWeibull, parameters)
}

