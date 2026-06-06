#
# Copyright (C) 2013-2026 University of Amsterdam
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#

compareContinuousDistributionsInternal <- function(jaspResults, dataset, options, state=NULL){
  .ccdDistributionComparisonTable(jaspResults, options)

  if (options[["variable"]] == "") return()
  if (length(options[["distributions"]]) == 0L) return()

  .hasErrors(dataset, type=c("infinity", "variance"), all.target = options[["variable"]],
             exitAnalysisIfErrors = TRUE)

  variable <- na.omit(dataset[[options[["variable"]]]])

  distributions <- .ccdGetDistributions(jaspResults, variable, options)

  if (length(distributions) == 0L) return()

  # get distribution comparison results
  comparison <- jaspResults[["comparisonState"]] %setOrRetrieve% (
    .ccdCompareDistributions(distributions, options) |>
    createJaspState(dependencies = c("variable", "distributions", "comparisonTableOrder", "comparisonTableOrderBy"))
  )
  comparison[["name"]] <- .ccdDistributionNames(distributions, full=options[["fullDistributionSpecification"]])

  # sort and display distribution results
  distributions <- distributions[comparison[["rank"]]]
  comparison <- comparison[comparison[["rank"]],]
  .ccdFillDistributionComparisonTable(jaspResults, options, comparison, variable)
  .ccdPerDistributionOutput(jaspResults, options, distributions, variable, comparison[["name"]])
}

.ccdGetDistributions <- function(jaspResults, variable, options) {
  distributions <- list()
  for (i in seq_along(options[["distributions"]])) {
    specification <- options[["distributions"]][[i]]
    if (is.null(specification[["distribution"]]) || specification[["distribution"]] == "") next

    key <- sprintf("distributionState%i", i)

    subOptions <- names(specification)
    nestedOptions <- lapply(subOptions, \(opt) c("distributions", i, opt))
    distributions[[key]] <- jaspResults[[key]] %setOrRetrieve% (
      .ccdComputeDistributionResults(specification, variable) |>
      createJaspState(
        dependencies = jaspDeps(
          options = "variable",
          nestedOptions = nestedOptions
          )
      )
    )
  }
  return(distributions)
}

.ccdMakeParameters <- function(specification) {
  parameters <- list()
  for (par in specification[["parameters"]]) {
    name <- par[["value"]]
    value <- par[[name]]
    if (par[["fixed"]]) value <- DistributionS7::fixed(value)
    parameters[[name]] <- value
  }
  
  return(parameters)
}

.ccdMakeDistribution <- function(specification) {
  parameters <- .ccdMakeParameters(specification)
  constructor <- getExportedValue("DistributionS7", specification[["distribution"]])
  distribution <- do.call(constructor, parameters)
  return(distribution)
}

.ccdComputeDistributionResults <- function(specification, variable) {
  distribution <- try(.ccdMakeDistribution(specification))

  # make distribution object
  if (isTryError(distribution))
    jaspBase::.quitAnalysis(
      message = gettextf("Could not initialize distribution %1$s, with the following error: <br> %2$s",
                         specification[["distribution"]], .extractErrorMessage(distribution))
      )

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
      data=variable)
      )

  if (isTryError(result))
    jaspBase::.quitAnalysis(
      message = gettextf("Could not fit distribution %1$s, with the following error: <br> %2$s. <br> You can try to change the initial parameter values, or remove the distribution from the distribution specification",
                         specification[["distribution"]], .extractErrorMessage(result))
      )

  distribution <- result

  # get information criteria
  result <- try(DistributionS7::information_criteria(distribution=distribution, data=variable))

  if (isTryError(result))
    jaspBase::.quitAnalysis(
      message = gettextf("Could not compute information criteria for %1$s, with the following error: <br> %2$s.",
                         specification[["distribution"]], .extractErrorMessage(result))
    )

  ic <- result

  result <- list(distribution=distribution, ic=ic)
  return(result)
}

.ccdCompareDistributions <- function(distributions, options) {
  results <- lapply(distributions, "[[", "ic")
  results <- do.call(rbind, results)

  results[["w_aic"]] <- DistributionS7::weights_ic(results[["aic"]])
  results[["w_bic"]] <- DistributionS7::weights_ic(results[["bic"]])


  if (!options[["comparisonTableOrder"]]) {
    results[["rank"]] <- seq_along(distributions)
  } else {
    ic <- switch(
      options[["comparisonTableOrderBy"]],
      aic = results[["w_aic"]],
      bic = results[["w_bic"]]
    )

    results[["rank"]] <- order(ic, decreasing = TRUE)
  }

  return(results)
}


.ccdDistributionComparisonTable <- function(jaspResults, options) {
  if (!options[["comparisonTable"]]) return()
  if (!is.null(jaspResults[["comparisonTable"]])) return()

  table <- createJaspTable(
    title = gettext("Distribution comparison table"),
    dependencies = c("variable", "distributions", "comparisonTable", "comparisonTableOrder", "comparisonTableOrderBy"),
    position = 1
  )
  table$showSpecifiedColumnsOnly <- TRUE
  table$addColumnInfo(name = "name",    title = gettext("Distribution"), type = "string")
  table$addColumnInfo(name = "w_aic",   title = gettext("AIC"),          type = "number", overtitle = gettext("Weights"))
  table$addColumnInfo(name = "w_bic",   title = gettext("BIC"),          type = "number", overtitle = gettext("Weights"))
  table$addColumnInfo(name = "aic",     title = gettext("AIC"),          type = "number")
  table$addColumnInfo(name = "bic",     title = gettext("BIC"),          type = "number")
  table$addColumnInfo(name = "log_lik", title = gettext("Log. Lik"),     type = "number")
  table$addColumnInfo(name = "n_par",   title = gettext("df"),           type = "integer")

  jaspResults[["comparisonTable"]] <- table
}

.ccdFillDistributionComparisonTable <- function(jaspResults, options, comparison, variable) {
  if (is.null(jaspResults[["comparisonTable"]])) return()
  jaspResults[["comparisonTable"]]$title <- gettextf("Distribution comparison table (n=%1$i)", length(variable))
  jaspResults[["comparisonTable"]]$setData(comparison)
}

.ccdPerDistributionOutput <- function(jaspResults, options, distributions, variable, titles) {
  if (options[["outputLimit"]] && options[["outputLimitTo"]] < length(distributions)) {
    n <- options[["outputLimitTo"]]
  } else {
    n <- length(distributions)
  }


  for (i in seq_len(n)) {
    key <- sprintf("distributionResults%s", i)
    container <- jaspResults[[key]] %setOrRetrieve% createJaspContainer(
      title = titles[i],
      dependencies = jaspBase::jaspDeps(
        options = c("distributions", "variable", "outputLimit", "outputLimitTo", "comparisonTableOrder", "comparisonTableOrderBy"),
        optionsFromObject = jaspResults[[names(distributions)[[i]]]]
      ),
      initCollapsed = TRUE
    )
    # override title if changed (not saved as dependency as to not recompute results if only the name changed)
    container$title <- titles[i]

    .ccdFillDistributionContainer(container, options, distributions[[i]][["distribution"]], variable)
  }
}

.ccdFillDistributionContainer <- function(container, options, distribution, variable) {
  .ccdParameterTable(container, options, distribution, variable)
  .ccdGofTable      (container, options, distribution, variable)
  .ccdEmpiricalPlots(container, options, distribution, variable)
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
  if (samples > 0L)
    jaspBase::startProgressbar(expectedTicks = samples, label = gettext("Bootstrapping..."))


  results <- try(DistributionS7::gof_test(
    distribution, variable, estimated=TRUE,
    bootstrap=DistributionS7::Bootstrap(samples=samples, callback = jaspBase::progressbarTick)
    ))

  if (isTryError(results)) {
    table$setError(gettextf("Could not obtain goodness of fit: <br> %s", .extractErrorMessage(results)))
    return()
  }
  results[["test"]] <- .ccdGofTestLabels(results[["test"]])

  if (samples > 0L) 
    table$addFootnote(
      message = gettextf("Based on %1$i valid parametric bootstrap samples.", attributes(results)[["bootstrap"]][["valid"]]),
      colNames = "p_value"
    )

  table$setData(results)
}

.ccdEmpiricalPlots <- function(container, options, distribution, variable) {
  if (!options[["empiricalPlots"]]) return()
  if (!is.null(container[["empiricalPlots"]])) return()

  plotContainer <- createJaspContainer(title = gettext("Empirical plots"),
                                   dependencies = c("empiricalPlots", "empiricalPlotsCi", "empiricalPlotsCiLevel"))

  plotContainer[["hist"]] <- createJaspPlot(
    title = gettext("Histogram vs. theoretical density"),
    plot = DistributionS7::plot_hist(distribution, variable, name=options[["variable"]]) +
      jaspGraphs::geom_rangeframe() +
      jaspGraphs::themeJaspRaw()
  )
  plotContainer[["qq"]] <- createJaspPlot(
    title = gettext("Q-Q plot"),
    plot = DistributionS7::plot_qq(distribution, variable, ci = options[["empiricalPlotsCi"]], ci_level = options[["empiricalPlotsCiLevel"]]) +
      jaspGraphs::geom_rangeframe() +
      jaspGraphs::themeJaspRaw()
  )
  plotContainer[["ecdf"]] <- createJaspPlot(
    title = gettext("Empirical vs. theoretical cumulative probability"),
    plot = DistributionS7::plot_ecdf(distribution, variable, name=options[["variable"]]) +
      jaspGraphs::geom_rangeframe() +
      jaspGraphs::themeJaspRaw()
  )
  plotContainer[["pp"]] <- createJaspPlot(
    title = gettext("P-P plot"),
    plot = DistributionS7::plot_pp(distribution, variable, ci = options[["empiricalPlotsCi"]], ci_level = options[["empiricalPlotsCiLevel"]]) +
      jaspGraphs::geom_rangeframe() +
      jaspGraphs::themeJaspRaw()
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


.ccdDistributionNames <- function(distributions, fullNames) {
  distributions <- lapply(distributions, "[[", "distribution")
  if (fullNames)
    return (vapply(distributions, DistributionS7::as_latex, character(1)) |> mathExpression())

  name <- vapply(distributions, S7::prop, character(1), "name")
  counts <- table(name)
  duplicated <- names(counts[counts > 1])

  result <- name
  counters <- setNames(integer(length(duplicated)), duplicated)

  for (i in seq_along(name)) {
    if (name[i] %in% duplicated) {
      counters[name[i]] <- counters[name[i]] + 1
      result[i] <- paste0(name[i], " (", counters[name[i]], ")")
    }
  }

  return(result)
}
