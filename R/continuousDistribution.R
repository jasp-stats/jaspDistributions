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
