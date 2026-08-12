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

#' Poisson
#'
#' @param chiSquare, Displays the chi-square goodness of fit test
#'    Defaults to \code{FALSE}.
#' @param ecdf, Displays an empirical cumulative distribution plot of the selected variable.
#'    Defaults to \code{FALSE}.
#' @param estCDF, Displays an empirical cumulative distribution plot overlayed with the cumulative distribution function of the fitted distribution
#'    Defaults to \code{FALSE}.
#' @param estPMF, Displays a histogram of the selected variable overlayed with the probability density function of the fitted distribution
#'    Defaults to \code{FALSE}.
#' @param histogram, Display a bar plot of the selected variable.
#'    Defaults to \code{FALSE}.
#' @param methodMLE, Estimates the parameters by the values in the domain at which the likelihood function is maximized. The likelihood function fixes the data argument (based on the selected variable) in the theoretical density function and views it as a function of the parameters. The optimization procedure is initialized with the values for the parameters entered under "Show Distribution".
#'    Defaults to \code{FALSE}.
#' @param moments, Displays a table with the raw and central sample moments of the selected variable. Defaults to first 2 moments.
#'    Defaults to \code{FALSE}.
#' @param newVariableName, Specify the name of the variable. Once filled, creates a column with samples drawn from the specified distribution in the current data set.
#' @param ppplot, Displays the probability-probability plot. The *x*-axis shows the theoretical value of the cumulative density function of the data points under the fitted distribution, the *y*-axis shows the empirical percentiles of the selected variable.
#'    Defaults to \code{FALSE}.
#' @param qqplot, Displays the quantile-quantile plot. The *x*-axis shows the theoretical quantiles of the data points under the fitted distribution, the *y*-axis shows the empirical quantiles of the selected variable.
#'    Defaults to \code{FALSE}.
#' @param sampleSize, Specify the number of samples.
#' @param summary, Displays a descriptive table of the selected variable.
#'    Defaults to \code{TRUE}.
LDpoisson <- function(
          data = NULL,
          version = "0.95",
          biasCorrected = FALSE,
          chiSquare = FALSE,
          ciInterval = FALSE,
          ciIntervalInterval = 0.95,
          ecdf = FALSE,
          estCDF = FALSE,
          estPMF = FALSE,
          explanatoryText = FALSE,
          formulas = FALSE,
          highlightDensity = FALSE,
          highlightProbability = FALSE,
          histogram = FALSE,
          histogramBins = 30,
          lambda = 1,
          max = 5,
          max_x = 5,
          methodMLE = FALSE,
          min = 0,
          min_x = 0,
          moments = FALSE,
          momentsUpTo = 2,
          newVariableName = "",
          outputEstimates = TRUE,
          outputSE = FALSE,
          parsSupportMoments = FALSE,
          plotCMF = FALSE,
          plotHeight = 320,
          plotPMF = TRUE,
          plotWidth = 480,
          ppPlotCi = FALSE,
          ppPlotCiLevel = 0.95,
          ppplot = FALSE,
          qqPlotCi = FALSE,
          qqPlotCiLevel = 0.95,
          qqplot = FALSE,
          sampleSize = 0,
          simulateNow = FALSE,
          summary = TRUE,
          variable = list(types = list(), value = "")) {

   defaultArgCalls <- formals(jaspDistributions::LDpoisson)
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

   return(jaspBase::runWrappedAnalysis("jaspDistributions", "LDpoisson", "LDpoisson.qml", options, version, TRUE))
}