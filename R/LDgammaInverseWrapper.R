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

#' Inverse gamma
#'
#' @param ecdf, Displays an empirical cumulative distribution plot of the selected variable.
#'    Defaults to \code{FALSE}.
#' @param highlightDensity, Highlights the probability density on the probability density plot and cumulative distribution plot at specified values of x
#'    Defaults to \code{FALSE}.
#' @param highlightProbability, Highlights the probability in between the specified values of x in the density plot (area under the curve), and highlights the cumulative probability at the specified values in the cumulative distribution plot
#'    Defaults to \code{FALSE}.
#' @param highlightType, Select the bounds of the interval to display: Density is highlighted at the lower and upper bounds, the probability is displayed for the specified interval.
#' \itemize{
#'   \item \code{"upper"}
#'   \item \code{"minmax"}
#'   \item \code{"lower"}
#' }
#' @param histogram, Display a histogram of the selected variable with the number of specified bins.
#'    Defaults to \code{FALSE}.
#' @param methodMLE, Estimates the parameters by the values in the domain at which the likelihood function is maximized. The likelihood function fixes the data argument (based on the selected variable) in the theoretical density function and views it as a function of the parameters. The optimization procedure is initialized with the values for the parameters entered under "Show Distribution".
#'    Defaults to \code{FALSE}.
#' @param min_x, Defines the limits of the x-axis of the probability density plot and cumulative distribution plot, and the limits of the y-axis of the quantile plot.
#' @param moments, Displays a table with the raw and central sample moments of the selected variable. Defaults to first 2 moments.
#'    Defaults to \code{FALSE}.
#' @param newVariableName, Specify the name of the variable. Once filled, creates a column with samples drawn from the specified distribution in the current data set.
#' @param sampleSize, Specify the number of samples.
#' @param summary, Displays a descriptive table of the selected variable.
#'    Defaults to \code{TRUE}.
LDgammaInverse <- function(
          data = NULL,
          version = "0.95",
          andersonDarling = FALSE,
          biasCorrected = FALSE,
          ciInterval = FALSE,
          ciIntervalInterval = 0.95,
          cramerVonMisses = FALSE,
          ecdf = FALSE,
          estCDF = FALSE,
          estPDF = FALSE,
          explanatoryText = FALSE,
          formulas = FALSE,
          highlightDensity = FALSE,
          highlightProbability = FALSE,
          highlightType = "minmax",
          histogram = FALSE,
          histogramBins = 30,
          kolmogorovSmirnov = FALSE,
          lower_max = 1,
          max = 2,
          max_x = 3,
          methodMLE = FALSE,
          min = 1,
          min_x = 0,
          moments = FALSE,
          momentsUpTo = 2,
          newVariableName = "",
          outputEstimates = TRUE,
          outputSE = FALSE,
          par2 = 1,
          parametrization = "scale",
          parsSupportMoments = FALSE,
          plotCDF = FALSE,
          plotHeight = 320,
          plotPDF = TRUE,
          plotQF = FALSE,
          plotWidth = 480,
          ppplot = FALSE,
          qqplot = FALSE,
          sampleSize = 0,
          shape = 1,
          simulateNow = FALSE,
          summary = TRUE,
          upper_min = 1,
          variable = list(types = list(), value = "")) {

   defaultArgCalls <- formals(jaspDistributions::LDgammaInverse)
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

   optionsWithFormula <- c("parametrization", "variable")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   return(jaspBase::runWrappedAnalysis("jaspDistributions", "LDgammaInverse", "LDgammaInverse.qml", options, version, TRUE))
}