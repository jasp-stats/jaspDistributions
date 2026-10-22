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
  if (options[["distribution"]] == "") return()
  
  variables <- .cdGetVariables(options, dataset)

  distribution <- .ccdMakeDistribution(options)
  distribution <- .cdFitDistribution(jaspResults, options, distribution, variables[["fitVariable"]])

  .cdParameterTable(jaspResults, options, distribution, variables[["fitVariable"]])
  .cdDescriptivesTable(jaspResults, options, variables)
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
    position = 3,
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
    title = gettext("Parameters"), 
    position = 2,
    dependencies = c("distribution", "parameters", "fitVariable", "parameterTable", "parameterTableUncertainty", "parameterTableCiLevel")
  )
  table$showSpecifiedColumnsOnly <- TRUE

  table$addColumnInfo(name = "name",      title = gettext("Name"),      type = "string")
  table$addColumnInfo(name = "label",     title = gettext("Label"),     type = "string")
  table$addColumnInfo(name = "estimate",  title = gettext("Value"),  type = "number")
  container[["parameterTable"]] <- table

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
  fillers <- list(pdfPlot = .cdPdfPlot, cdfPlot = .cdCdfPlot, qfPlot = .cdQfPlot)

  # the range and the highlights are shared by all plots, so they are only computed when a plot is (re)drawn
  settings <- NULL
  for (name in plotNames) {
    if (!is.null(plotContainer[[name]])) next

    plot <- createJaspPlot(title = titles[[name]], width = 480, height = 320, dependencies = name)
    plotContainer[[name]] <- plot

    if (is.null(settings))
      settings <- .cdDistributionPlotSettings(options, distribution)

    if (is.character(settings)) {
      plot$setError(settings)
      next
    }

    result <- try(fillers[[name]](distribution, settings))
    if (isTryError(result))
      plot$setError(gettextf("Could not draw the plot: %s", .extractErrorMessage(result)))
    else
      plot$plotObject <- result
  }
}

# the plot range and the highlights, or an error message
.cdDistributionPlotSettings <- function(options, distribution) {
  if (options[["plotRangeBasedOn"]] == "quantile") {
    range <- c(options[["plotQuantileMin"]], options[["plotQuantileMax"]])
  } else {
    if (options[["plotCdfMin"]] >= options[["plotCdfMax"]])
      return(gettext("The start of the plot range must be smaller than its end."))
    range <- DistributionS7::qf(distribution, c(options[["plotCdfMin"]], options[["plotCdfMax"]]))
  }

  if (any(!is.finite(range)) || range[1L] >= range[2L])
    return(gettext("The start of the plot range must be smaller than its end."))

  # TODO: the colors of the rows are not used yet, DistributionS7 draws all highlights in the same color
  densityPoints <- lapply(options[["plotDensityPoints"]], function(row) list(x = .cdParseFormula(row[["point"]])))
  intervals     <- lapply(options[["plotProbabilityLimits"]], function(row) list(
    lower = .cdParseFormula(row[["min"]]),
    upper = .cdParseFormula(row[["max"]])
  ))

  if (any(vapply(densityPoints, function(point) is.na(point[["x"]]), logical(1))))
    return(gettext("Each highlighted density must be at a number or an R expression that gives a number."))

  if (any(vapply(intervals, function(interval) is.na(interval[["lower"]]) || is.na(interval[["upper"]]), logical(1))))
    return(gettext("The limits of each highlighted probability must be numbers or R expressions that give a number."))

  if (any(vapply(intervals, function(interval) interval[["lower"]] > interval[["upper"]], logical(1))))
    return(gettext("The lower limit of each highlighted probability must not be larger than its upper limit."))

  return(list(range = range, densityPoints = densityPoints, intervals = intervals))
}

# the plots are drawn by DistributionS7 (not exported yet)
.cdPdfPlot <- function(distribution, settings) {
  probability <- data.frame(
    xmin = vapply(settings[["intervals"]], `[[`, numeric(1), "lower"),
    xmax = vapply(settings[["intervals"]], `[[`, numeric(1), "upper")
  )

  DistributionS7:::plot_pdf(
    distribution,
    xlim        = settings[["range"]],
    density     = .cdDensityPoints(settings),
    probability = if (nrow(probability) > 0L) probability
  ) + .cdPlotTheme()
}

.cdCdfPlot <- function(distribution, settings) {
  DistributionS7:::plot_cdf(
    distribution,
    xlim        = settings[["range"]],
    density     = .cdDensityPoints(settings),
    probability = .cdProbabilityPoints(settings)
  ) + .cdPlotTheme()
}

.cdQfPlot <- function(distribution, settings) {
  DistributionS7:::plot_qf(
    distribution,
    xlim        = settings[["range"]],
    density     = .cdDensityPoints(settings),
    probability = .cdProbabilityPoints(settings)
  ) + .cdPlotTheme()
}

.cdDensityPoints <- function(settings) {
  points <- vapply(settings[["densityPoints"]], `[[`, numeric(1), "x")
  if (length(points) > 0L) points
}

# the cdf and quantile plots mark points rather than intervals: mark the finite limits of the intervals
.cdProbabilityPoints <- function(settings) {
  limits <- unlist(lapply(settings[["intervals"]], function(interval) c(interval[["lower"]], interval[["upper"]])))
  limits <- unique(limits[is.finite(limits)])
  if (length(limits) > 0L) limits
}

.cdPlotTheme <- function() {
  list(jaspGraphs::geom_rangeframe(), jaspGraphs::themeJaspRaw())
}


.cdFitAssessment <- function(container, options, distribution, variables) {
  # without a variable there is nothing to assess (the options of the disabled section may still be checked)
  if (is.null(variables[["assessVariable"]])) return()

  # check if we need to create the container at all
  plotNames <- c("histPlot", "ecdfPlot", "qqPlot", "ppPlot")
  selected  <- options[plotNames] |> vapply(isTRUE, logical(1))
  if (!any(selected)) return()

  fitContainer <- container[["fitAssessment"]]
  if (is.null(fitContainer)) {
    fitContainer <- createJaspContainer(
      title        = gettext("Fit Assessment"),
      dependencies = c("distribution", "parameters", "fitVariable", "assessVariable")
    )
    container[["fitAssessment"]] <- fitContainer
  }

  name <- if (variables[["sameVariables"]]) options[["fitVariable"]] else options[["assessVariable"]]
  .cdFitPlots(fitContainer, options, distribution, variables[["assessVariable"]], decodeColNames(name))
  # TODO: a "Statistics" container (fitContainer[["statistics"]]) with the fit statistics
}

.cdFitPlots <- function(container, options, distribution, variable, name=NULL) {
  if (!any(vapply(c("histPlot", "ecdfPlot", "qqPlot", "ppPlot"), function(plotName) isTRUE(options[[plotName]]), logical(1)))) return()

  plotContainer <- container[["plots"]]
  if (is.null(plotContainer)) {
    plotContainer <- createJaspContainer(title = gettext("Plots"))
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
