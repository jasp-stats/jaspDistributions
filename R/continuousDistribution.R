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

ContinuousDistributionInternal <- function(jaspResults, dataset = NULL, options) {
  variables <- .cdGetVariables(options, dataset)

  # the descriptives do not need a distribution
  .cdDescriptivesTable(jaspResults, options, variables)

  # without a distribution, show an empty parameter table as a placeholder
  if (options[["distribution"]] == "") {
    .cdParameterTable(jaspResults, options, NULL, variables[["fitVariable"]])
    return()
  }

  distribution <- .ccdMakeDistribution(options)
  distribution <- .cdFitDistribution(jaspResults, options, distribution, variables[["fitVariable"]])

  .cdParameterTable(jaspResults, options, distribution, variables[["fitVariable"]])
  .cdDistributionTable(jaspResults, options, distribution)
  .cdDistributionPlots(jaspResults, options, distribution)
  .cdFitAssessment(jaspResults, options, distribution, variables)

  return()
  simulateRows <- .cdSimulateDataRows(options)
  .cdCheckSimulatedColumns(options, simulateRows)

  # Simulated data from the specified distribution: may be used for estimation
  .cdSimulateColumn(
    jaspResults, options,
    simulateRows = simulateRows,
    rowValue     = "specified",
    stateName    = "simulateSpecifiedColumn",
    sampler      = function(n) stats::rnorm(n), # TODO: draw from the specified distribution
    dependencies = c("distribution", "parametrization", "parameters")
  )

  # Simulated data from the fitted distribution: may be used for assessing the fit only
  if (options[["fitVariable"]] != "") {
    fitData <- stats::na.omit(dataset[[options[["fitVariable"]]]])
    .cdSimulateColumn(
      jaspResults, options,
      simulateRows = simulateRows,
      rowValue     = "fitted",
      stateName    = "simulateFittedColumn",
      sampler      = function(n) stats::rnorm(n, mean(fitData), stats::sd(fitData)), # TODO: draw from the fitted distribution
      dependencies = c("distribution", "parametrization", "parameters", "fitVariable", "parameterEstimationMethod")
    )
  }
}

.cdGetVariables <- function(options, dataset) {
  getVariable <- function(name) if (name == "") NULL else stats::na.omit(dataset[[name]])

  fitVariable    <- getVariable(options[["fitVariable"]])
  sameVariables  <- options[["assessVariable"]] %in% c("", options[["fitVariable"]])
  assessVariable <- if (sameVariables) fitVariable else getVariable(options[["assessVariable"]])

  list(fitVariable = fitVariable, assessVariable = assessVariable, sameVariables = sameVariables)
}


.cdDescriptivesTable <- function(container, options, variables) {
  if (!isTRUE(options[["descriptivesTable"]])) return()
  if (!is.null(container[["descriptivesTable"]])) return()

  # describe both variables if the fit is assessed on a different variable than the one used for estimation
  described <- list()
  if (!is.null(variables[["fitVariable"]]))
    described[[length(described) + 1L]] <- list(variable = options[["fitVariable"]],    role = gettext("Estimation"), values = variables[["fitVariable"]])
  if (!variables[["sameVariables"]])
    described[[length(described) + 1L]] <- list(variable = options[["assessVariable"]], role = gettext("Assessment"), values = variables[["assessVariable"]])

  # without a variable there is nothing to describe (the checkbox is disabled, but may still be checked)
  if (length(described) == 0L) return()

  table <- createJaspTable(
    title = gettext("Descriptives"), 
    position = 2,
    dependencies = c("descriptivesTable", "fitVariable", "assessVariable")
  )
  table$showSpecifiedColumnsOnly <- TRUE
  table$addColumnInfo(name = "variable", title = gettext("Variable"),       type = "string")
  if (length(described) > 1L)
    table$addColumnInfo(name = "role",   title = gettext("Used for"),       type = "string")
  table$addColumnInfo(name = "valid",    title = gettext("Valid"),          type = "integer")
  table$addColumnInfo(name = "mean",     title = gettext("Mean"),           type = "number")
  table$addColumnInfo(name = "sd",       title = gettext("Std. Deviation"), type = "number")
  table$addColumnInfo(name = "min",      title = gettext("Minimum"),        type = "number")
  table$addColumnInfo(name = "max",      title = gettext("Maximum"),        type = "number")
  container[["descriptivesTable"]] <- table

  rows <- lapply(described, function(v) {
    values <- v[["values"]]
    empty  <- length(values) == 0L # e.g., all values of the assessment variable are missing
    data.frame(
      variable = v[["variable"]],
      role     = v[["role"]],
      valid    = length(values),
      mean     = if (empty) NA else mean(values),
      sd       = if (empty) NA else stats::sd(values),
      min      = if (empty) NA else min(values),
      max      = if (empty) NA else max(values)
    )
  })

  table$setData(do.call(rbind, rows))
}

.cdFitDistribution <- function(jaspResults, options, distribution, variable) {
  if (!is.null(jaspResults[["distribution"]])) return (jaspResults[["distribution"]]$object)
  if (is.null(variable)) return(distribution)
  if (DistributionS7::nfree(distribution) == 0L) return(distribution)
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

  jaspResults[["distribution"]] <- createJaspState(
    object = distribution,
    dependencies = c("distribution", "parameters", "fitVariable")
  )

  return(distribution)
}

.cdParameterTable <- function(container, options, distribution, variable) {
  if (!options[["parameterTable"]]) return()
  if (!is.null(container[["parameterTable"]])) return()

  table <- createJaspTable(
    title = if (is.null(distribution)) gettext("Parameters - no distribution selected") else gettextf("Parameters - %s", S7::prop(distribution, "name")),
    position = 1,
    dependencies = c("distribution", "parameters", "fitVariable", "parameterTable", "parameterTableUncertainty", "parameterTableCiLevel")
  )
  table$showSpecifiedColumnsOnly <- TRUE

  table$addColumnInfo(name = "name",      title = gettext("Name"),      type = "string")
  table$addColumnInfo(name = "label",     title = gettext("Label"),     type = "string")
  table$addColumnInfo(name = "estimate",  title = gettext("Value"),  type = "number")
  container[["parameterTable"]] <- table

  if (is.null(distribution)) return()

  if (DistributionS7::nfree(distribution) > 0L) {
    if (options[["parameterTableUncertainty"]] && !is.null(variable)) {
      table$addColumnInfo(name = "se", title = gettext("SE"), type = "number")
      overtitle <- gettextf("%s%% CI", 100*options[["parameterTableCiLevel"]])
      table$addColumnInfo(name = "lower", title = gettext("Lower"), type = "number", overtitle = overtitle)
      table$addColumnInfo(name = "upper", title = gettext("Upper"), type = "number", overtitle = overtitle)

      results <- try(DistributionS7::parameter_inference(
        distribution, 
        DistributionS7::DefaultMethod(ci_level = options[["parameterTableCiLevel"]]), variable)
      )
  
      if (isTryError(results)) {
        table$setError(gettext("Could not obtain parameter uncertainty."))
        return()
      }
      results[["label"]] <- mathExpression(results[["label"]])
    } else {
      results <- list()
      results[["label"]] <- DistributionS7::parameter_properties(distribution, property="label", which="free") |> unlist() |> mathExpression()
      results[["estimate"]] <- DistributionS7::parameter_values(distribution, which="free") |> unlist()
    }
    results[["name"]] <-  DistributionS7::parameter_properties(distribution, property="name",  which="free") |> unlist()
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

}

.cdDistributionTable <- function(container, options, distribution) {
  if (!options[["distributionTable"]]) return()
  if (!is.null(container[["distributionTable"]])) return()

  table <- createJaspTable(
    title        = gettext("Distribution Table"),
    position     = 3,
    dependencies = c("distribution", "parameters", "fitVariable", "distributionTable", "distributionTableBasedOn", "distributionTableType",
                     "distributionTableQuantileMin", "distributionTableQuantileMax", "distributionTableCdfMin", "distributionTableCdfMax",
                     "distributionTableLength", "distributionTableQuantilePoints", "distributionTableCdfPoints")
  )

  basedOnQuantiles <- options[["distributionTableBasedOn"]] == "quantile"

  # the column on which the table is based comes first
  table$addColumnInfo(name = "quantile", title = gettext("Quantile"), type = "number")
  table$addColumnInfo(name = "pdf",      title = gettext("Density"),  type = "number")
  table$addColumnInfo(name = "cdf",      title = gettext("Lower"),    type = "number", overtitle = gettext("Cumulative probability"))
  table$addColumnInfo(name = "ccdf",     title = gettext("Upper"),    type = "number", overtitle = gettext("Cumulative probability"))

  container[["distributionTable"]] <- table

  points <- .cdDistributionTablePoints(options, basedOnQuantiles)
  if (is.character(points)) {
    table$setError(points)
    return()
  }

  if (length(points) == 0L)
    return()

  results <- try({
    if (basedOnQuantiles) {
      quantile <- points
      cdf      <- DistributionS7::cdf(distribution, quantile)
      ccdf     <- DistributionS7::cdf(distribution, quantile, lower.tail = FALSE)
    } else {
      cdf      <- points
      quantile <- DistributionS7::qf(distribution, cdf)
      ccdf     <- 1 - cdf
    }
    list(quantile = quantile, cdf = cdf, pdf = DistributionS7::pdf(distribution, quantile), ccdf = ccdf)
  })

  if (isTryError(results)) {
    table$setError(gettextf("Could not compute the distribution table: %s", .extractErrorMessage(results)))
    return()
  }

  table$setData(results)
}

# the points at which to evaluate the distribution, or an error message
.cdDistributionTablePoints <- function(options, basedOnQuantiles) {
  prefix <- if (basedOnQuantiles) "distributionTableQuantile" else "distributionTableCdf"

  if (options[["distributionTableType"]] == "range") {
    from <- options[[paste0(prefix, "Min")]]
    to   <- options[[paste0(prefix, "Max")]]
    if (from >= to)
      return(gettext("The start of the range must be smaller than its end."))

    return(seq(from, to, length.out = options[["distributionTableLength"]]))
  }

  points <- vapply(options[[paste0(prefix, "Points")]], function(row) .cdParseFormula(row[["point"]]), numeric(1))

  if (anyNA(points))
    return(gettext("Each point of the distribution table must be a number or an R expression that gives a number."))

  if (!basedOnQuantiles && any(points < 0 | points > 1))
    return(gettext("Cumulative probabilities must be between 0 and 1."))

  return(points)
}

# a FormulaField gives a number, or a string with an R expression; returns NA if it does not evaluate to a single number
.cdParseFormula <- function(value) {
  if (!is.numeric(value))
    value <- try(.parseRCodeInOptions(value), silent = TRUE)

  if (isTryError(value) || !is.numeric(value) || length(value) != 1L) NA_real_ else as.numeric(value)
}

.cdDistributionPlots <- function(container, options, distribution) {
  plotNames <- c("pdfPlot", "cdfPlot", "qfPlot")
  plotNames <- plotNames[vapply(plotNames, function(name) isTRUE(options[[name]]), logical(1))]
  if (length(plotNames) == 0L) return()

  plotContainer <- container[["distributionPlots"]]
  if (is.null(plotContainer)) {
    plotContainer <- createJaspContainer(
      title        = gettext("Plots"),
      position     = 4,
      dependencies = c("distribution", "parameters", "fitVariable", "plotRangeBasedOn",
                       "plotQuantileMin", "plotQuantileMax", "plotCdfMin", "plotCdfMax",
                       "plotDensityPoints", "plotProbabilityLimits")
    )
    container[["distributionPlots"]] <- plotContainer
  }

  titles <- c(
    pdfPlot = gettext("Probability density function"),
    cdfPlot = gettext("Cumulative distribution function"),
    qfPlot  = gettext("Quantile function")
  )
  fillers <- list(pdfPlot = DistributionS7::plot_pdf, cdfPlot = DistributionS7::plot_cdf, qfPlot = DistributionS7::plot_qf)

  # the range and the highlights are shared by all plots, so they are only computed when a plot is (re)drawn
  settings <- NULL
  for (name in plotNames) {
    if (!is.null(plotContainer[[name]])) next

    plot <- createJaspPlot(title = titles[[name]], width = 480, height = 320, position = match(name, names(titles)), dependencies = name)
    plotContainer[[name]] <- plot

    if (is.null(settings))
      settings <- .cdDistributionPlotSettings(options, distribution)

    if (is.character(settings)) {
      plot$setError(settings)
      next
    }

    # the cdf and quantile plots list the slopes at the highlighted densities in a legend, in the corner the curve leaves empty
    legend <- if (!is.null(settings[["density"]])) switch(name, cdfPlot = c(1, 0), qfPlot = c(0, 1))
    # DistributionS7 only derives the range when xlim is missing, so an automatic range leaves it out
    args <- list(distribution, density = settings[["density"]], probability = settings[["probability"]])
    if (!is.null(settings[["range"]]))
      args[["xlim"]] <- settings[["range"]]

    result <- try(do.call(fillers[[name]], args) + .cdPlotTheme(legend = legend))
    if (isTryError(result))
      plot$setError(gettextf("Could not draw the plot: %s", .extractErrorMessage(result)))
    else
      plot$plotObject <- result
  }
}

# the plot range and the highlights, or an error message
.cdDistributionPlotSettings <- function(options, distribution) {
  # NULL leaves the range to DistributionS7, which derives it from the support of the distribution
  range <- switch(
    options[["plotRangeBasedOn"]],
    auto     = NULL,
    quantile = c(options[["plotQuantileMin"]], options[["plotQuantileMax"]]),
    cdf      = DistributionS7::qf(distribution, c(options[["plotCdfMin"]], options[["plotCdfMax"]]))
  )

  if (!is.null(range) && (any(!is.finite(range)) || range[1L] >= range[2L]))
    return(gettext("The start of the plot range must be smaller than its end."))

  densityRows <- options[["plotDensityPoints"]]
  density     <- data.frame(
    x     = vapply(densityRows, function(row) .cdParseFormula(row[["point"]]), numeric(1)),
    color = vapply(densityRows, function(row) .cdColor(row[["color"]]), character(1))
  )

  probabilityRows <- options[["plotProbabilityLimits"]]
  probability     <- data.frame(
    xmin  = vapply(probabilityRows, function(row) .cdParseFormula(row[["min"]]), numeric(1)),
    xmax  = vapply(probabilityRows, function(row) .cdParseFormula(row[["max"]]), numeric(1)),
    color = vapply(probabilityRows, function(row) .cdColor(row[["color"]]), character(1))
  )

  if (anyNA(density[["x"]]))
    return(gettext("Each highlighted density must be at a number or an R expression that gives a number."))

  if (anyNA(probability[["xmin"]]) || anyNA(probability[["xmax"]]))
    return(gettext("The limits of each highlighted probability must be numbers or R expressions that give a number."))

  if (any(probability[["xmin"]] > probability[["xmax"]]))
    return(gettext("The lower limit of each highlighted probability must not be larger than its upper limit."))

  return(list(
    range       = range,
    density     = if (nrow(density) > 0L) density,
    probability = if (nrow(probability) > 0L) probability
  ))
}

# a missing color is NA, so that DistributionS7 picks one from its palette
.cdColor <- function(color) {
  if (is.null(color) || color == "") NA_character_ else as.character(color)
}

# DistributionS7 sets no position scales, so the JASP scales extend the axes to cover everything that is drawn
# legend: NULL hides the legend, otherwise the corner of the panel to put it in, e.g., c(1, 0) for the bottom right
.cdPlotTheme <- function(legend = NULL) {
  theme <- list(
    jaspGraphs::scale_x_continuous(),
    jaspGraphs::scale_y_continuous(),
    jaspGraphs::geom_rangeframe()
  )

  if (is.null(legend))
    return(c(theme, list(jaspGraphs::themeJaspRaw())))

  # themeJaspRaw hides the legend by default
  c(theme, list(
    jaspGraphs::themeJaspRaw(legend.position = "inside"),
    ggplot2::theme(legend.position.inside = legend, legend.justification.inside = legend)
  ))
}


.cdFitAssessment <- function(container, options, distribution, variables) {
  # without a variable there is nothing to assess (the options of the disabled section may still be checked)
  if (is.null(variables[["assessVariable"]])) return()

  # check if we need to create the container at all
  plotNames <- c("histPlot", "ecdfPlot", "qqPlot", "ppPlot")
  statNames <- c("modelFitTable", "goodnessOfFit")
  selected  <- options[c(plotNames, statNames)] |> vapply(isTRUE, logical(1))
  if (!any(selected)) return()

  fitContainer <- container[["fitAssessment"]]
  if (is.null(fitContainer)) {
    fitContainer <- createJaspContainer(
      title        = gettext("Fit Assessment"),
      position     = 5,
      dependencies = c("distribution", "parameters", "fitVariable", "assessVariable")
    )
    container[["fitAssessment"]] <- fitContainer
  }

  name <- if (variables[["sameVariables"]]) options[["fitVariable"]] else options[["assessVariable"]]
  .cdFitPlots(fitContainer, options, distribution, variables[["assessVariable"]], name)
  .cdFitStatistics(fitContainer, options, distribution, variables)
}

.cdFitStatistics <- function(container, options, distribution, variables) {
  if (!any(vapply(options[c("modelFitTable", "goodnessOfFit")], isTRUE, logical(1)))) return()

  statisticsContainer <- container[["statistics"]]
  if (is.null(statisticsContainer)) {
    statisticsContainer <- createJaspContainer(title = gettext("Statistics"), position = 2)
    container[["statistics"]] <- statisticsContainer
  }

  .cdModelFitTable(statisticsContainer, options, distribution, variables)
  .cdGofTable(statisticsContainer, options, distribution, variables)
}

.cdModelFitTable <- function(container, options, distribution, variables) {
  if (!options[["modelFitTable"]]) return()
  if (!is.null(container[["modelFitTable"]])) return()

  # AIC and BIC are only meaningful for the data used for estimation;
  # on a different variable, the log-likelihood is the out-of-sample (predictive) log-likelihood
  inSample <- variables[["sameVariables"]]

  table <- createJaspTable(title = gettext("Model Fit"), position = 1, dependencies = "modelFitTable")
  table$showSpecifiedColumnsOnly <- TRUE
  table$addColumnInfo(name = "n_obs",   title = gettext("n"),              type = "integer")
  table$addColumnInfo(name = "n_par",   title = gettext("df"),             type = "integer")
  table$addColumnInfo(name = "log_lik", title = gettext("Log-likelihood"), type = "number")
  if (inSample) {
    table$addColumnInfo(name = "aic",   title = gettext("AIC"),            type = "number")
    table$addColumnInfo(name = "bic",   title = gettext("BIC"),            type = "number")
  }
  container[["modelFitTable"]] <- table

  results <- try(DistributionS7::information_criteria(distribution, variables[["assessVariable"]]))
  if (isTryError(results)) {
    table$setError(gettextf("Could not compute the model fit: %s", .extractErrorMessage(results)))
    return()
  }

  table$setData(results)
}

.cdGofTable <- function(container, options, distribution, variables) {
  if (!options[["goodnessOfFit"]]) return()
  if (!is.null(container[["goodnessOfFit"]])) return()

  bootstrap <- options[["goodnessOfFitPValue"]] == "bootstrap"

  table <- createJaspTable(
    title        = gettext("Goodness-of-fit Tests"),
    position     = 2,
    dependencies = c("goodnessOfFit", "goodnessOfFitPValue", "goodnessOfFitBootstrapSamples", "setSeed", "seed")
  )
  table$addColumnInfo(name = "test",      title = gettext("Test"),      type = "string")
  table$addColumnInfo(name = "statistic", title = gettext("Statistic"), type = "number")
  table$addColumnInfo(name = "p_value",   title = gettext("p"),         type = "pvalue")
  container[["goodnessOfFit"]] <- table

  # the parameters are known (rather than estimated from the data the tests are run on)
  # if the fit is assessed on a different variable, or if all parameters are fixed
  estimated <- variables[["sameVariables"]] && DistributionS7::nfree(distribution) > 0L

  samples <- if (bootstrap) options[["goodnessOfFitBootstrapSamples"]] else 0L
  if (bootstrap) {
    .setSeedJASP(options)
    startProgressbar(expectedTicks = samples, label = gettext("Bootstrapping goodness of fit..."))
  }

  results <- try(DistributionS7::gof_test(
    distribution, variables[["assessVariable"]], estimated = estimated,
    bootstrap = DistributionS7::Bootstrap(samples = samples, callback = progressbarTick)
  ))

  if (isTryError(results)) {
    table$setError(gettextf("Could not obtain the goodness-of-fit tests: %s", .extractErrorMessage(results)))
    return()
  }
  results[["test"]] <- .ccdGofTestLabels(results[["test"]])

  if (bootstrap)
    table$addFootnote(
      message  = gettextf("Based on %1$i valid parametric bootstrap samples.", attributes(results)[["bootstrap"]][["valid"]]),
      colNames = "p_value"
    )

  table$setData(results)
}

.cdFitPlots <- function(container, options, distribution, variable, name=NULL) {
  if (!any(vapply(c("histPlot", "ecdfPlot", "qqPlot", "ppPlot"), function(plotName) isTRUE(options[[plotName]]), logical(1)))) return()

  plotContainer <- container[["plots"]]
  if (is.null(plotContainer)) {
    plotContainer <- createJaspContainer(title = gettext("Plots"), position = 1)
    container[["plots"]] <- plotContainer
  }

  plots <- list(
    histPlot = list(
      title        = gettext("Histogram vs. theoretical pdf"),
      dependencies = "histPlot",
      draw         = function() DistributionS7::plot_hist(distribution, variable, name = name)
    ),
    ecdfPlot = list(
      title        = gettext("Empirical vs. theoretical cdf"),
      dependencies = "ecdfPlot",
      draw         = function() DistributionS7::plot_ecdf(distribution, variable, name = name)
    ),
    qqPlot   = list(
      title        = gettext("Q-Q plot"),
      dependencies = c("qqPlot", "qqPlotCi", "qqPlotCiLevel"),
      draw         = function() DistributionS7::plot_qq(distribution, variable, ci = options[["qqPlotCi"]], ci_level = options[["qqPlotCiLevel"]])
    ),
    ppPlot   = list(
      title        = gettext("P-P plot"),
      dependencies = c("ppPlot", "ppPlotCi", "ppPlotCiLevel"),
      draw         = function() DistributionS7::plot_pp(distribution, variable, ci = options[["ppPlotCi"]], ci_level = options[["ppPlotCiLevel"]])
    )
  )

  for (plotName in names(plots)) {
    if (!isTRUE(options[[plotName]]) || !is.null(plotContainer[[plotName]])) next

    plotInfo <- plots[[plotName]]
    plot     <- createJaspPlot(
      title = plotInfo[["title"]], 
      #width = 480, height = 320, 
      position = match(plotName, names(plots)),
      dependencies = plotInfo[["dependencies"]])
    plotContainer[[plotName]] <- plot

    result <- try(plotInfo[["draw"]]() + .cdPlotTheme())
    if (isTryError(result))
      plot$setError(gettextf("Could not draw the plot: %s", .extractErrorMessage(result)))
    else
      plot$plotObject <- result
  }
}

# the rows of the "simulateData" ComponentsList, named by their value ("specified", "fitted")
.cdSimulateDataRows <- function(options) {
  rows <- options[["simulateData"]]
  names(rows) <- vapply(rows, `[[`, character(1), "value")
  return(rows)
}

.cdSimulatedColumnName <- function(row) {
  if (is.null(row) || !row[["simulate"]]) "" else row[["column"]]
}

.cdCheckSimulatedColumns <- function(options, simulateRows) {
  specifiedColumn <- .cdSimulatedColumnName(simulateRows[["specified"]])
  fittedColumn    <- .cdSimulatedColumnName(simulateRows[["fitted"]])

  if (specifiedColumn != "" && specifiedColumn == fittedColumn)
    .quitAnalysis(gettext("The data simulated from the specified and from the fitted distribution must be saved in different columns."))

  # the column simulated from the fitted distribution depends on the fit, so using it for fitting would create a cycle
  if (fittedColumn != "" && options[["fitVariable"]] != "" && decodeColNames(options[["fitVariable"]]) == fittedColumn)
    .quitAnalysis(gettext("The data simulated from the fitted distribution cannot be used to estimate the parameters."))
}

.cdSimulateColumn <- function(jaspResults, options, simulateRows, rowValue, stateName, sampler, dependencies) {
  row        <- simulateRows[[rowValue]]
  columnName <- .cdSimulatedColumnName(row)
  if (columnName == "")
    return()

  # the column stays in the state until its dependencies change, so that a rerun
  # triggered by writing the column (or by using it in the analysis) does not redraw it
  if (!is.null(jaspResults[[stateName]]))
    return()

  .setSeedJASP(options)
  sample <- sampler(row[["sampleSize"]])

  # depend only on this row of the list, so that changing the other row does not redraw this column
  # (nestedOptions compares the raw option, unlike optionContainsValue, which compares the R-converted row and never matches)
  column <- createJaspColumn(columnName = columnName)
  column$dependOn(c(dependencies, "setSeed", "seed"), nestedOptions = c("simulateData", as.character(match(rowValue, names(simulateRows)))))
  jaspResults[[stateName]] <- column

  if (!column$setScale(sample))
    jaspResults[[paste0(stateName, "Error")]] <- createJaspHtml(
      text         = gettextf("Could not write to column '%s', probably because it wasn't created by this analysis", columnName),
      elementType  = "errorMsg",
      dependencies = "simulateData"
    )
}
