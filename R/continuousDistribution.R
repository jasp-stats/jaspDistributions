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
  if (options[["fitVariable"]] != "")
    variable <- dataset[[options[["fitVariable"]]]]
  else
    variable <- NULL
  
  
  distribution <- .ccdMakeDistribution(options)
  
  distribution <- .cdFitDistribution(jaspResults, options, distribution, variable)

  .cdParameterTable(jaspResults, options, distribution, variable)

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
