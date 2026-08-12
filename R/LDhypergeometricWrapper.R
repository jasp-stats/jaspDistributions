#
# Copyright (C) 2013-2025 University of Amsterdam
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

# This is a generated file. Don't change it!

#' Hypergeometric
#'
#' @param ecdf, Displays an empirical cumulative distribution plot of the selected variable.
#'    Defaults to \code{FALSE}.
#' @param histogram, Display a bar plot of the selected variable.
#'    Defaults to \code{FALSE}.
#' @param moments, Displays a table with the raw and central sample moments of the selected variable. Defaults to first 2 moments.
#'    Defaults to \code{FALSE}.
#' @param newVariableName, Specify the name of the variable. Once filled, creates a column with samples drawn from the specified distribution in the current data set.
#' @param sampleSize, Specify the number of samples.
#' @param summary, Displays a descriptive table of the selected variable.
#'    Defaults to \code{TRUE}.
LDhypergeometric <- function(
          data = NULL,
          version = "0.95",
          draws = 5,
          ecdf = FALSE,
          explanatoryText = FALSE,
          formulas = FALSE,
          highlightDensity = FALSE,
          highlightProbability = FALSE,
          histogram = FALSE,
          histogramBins = 30,
          max = 5,
          max_x = 5,
          min = 0,
          min_x = 0,
          moments = FALSE,
          momentsUpTo = 2,
          newVariableName = "",
          parsSupportMoments = FALSE,
          plotCMF = FALSE,
          plotHeight = 320,
          plotPMF = TRUE,
          plotWidth = 480,
          sampleSize = 0,
          simulateNow = FALSE,
          size = 10,
          success = 5,
          summary = TRUE,
          variable = list(types = list(), value = "")) {

   defaultArgCalls <- formals(jaspDistributions::LDhypergeometric)
   defaultArgs <- lapply(defaultArgCalls, eval)
   options <- as.list(match.call())[-1L]
   options <- lapply(options, eval)
   defaults <- setdiff(names(defaultArgs), names(options))
   options[defaults] <- defaultArgs[defaults]
   options[["data"]] <- NULL
   options[["version"]] <- NULL


   if (!jaspBase::jaspResultsCalledFromJasp() && !is.null(data)) {
      jaspBase::storeDataSet(data)
   }

   optionsWithFormula <- c("variable")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   return(jaspBase::runWrappedAnalysis("jaspDistributions", "LDhypergeometric", "LDhypergeometric.qml", options, version, TRUE))
}