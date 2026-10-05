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

#' Compare Continuous Distributions
#'
#' Specify a set of distributions, estimate their parameters, and compare their fit to data.
#'
#' @param comparisonTable, Outputs the main distribution comparison table.
#'    Defaults to \code{TRUE}.
#' @param comparisonTableOrder, Orders the output by how well the distributions fit the data (according to AIC or BIC).
#'    Defaults to \code{TRUE}.
#' @param distributions, Specify distributions to be compared...
#' @param empiricalPlots, Outputs histogram vs theoretical density plot, empirical vs. theoretical cumulative distribution function, the Q-Q plot, and the P-P plot.
#'    Defaults to \code{FALSE}.
#' @param empiricalPlotsCi, Add the confidence interval to the P-P and Q-Q plots.
#'    Defaults to \code{FALSE}.
#' @param fullDistributionSpecification, Displays the full distribution specification, including its parameters. If unchecked, only distribution names are shown.
#'    Defaults to \code{FALSE}.
#' @param goodnessOfFit, Compute goodness of fit tests. For most of the distributions, the default tests are Cramér-von Mises and Anderson-Darling for composite null hypothesis. Note that these tests rely on randomly splitting the data in two sets;				as a result, the results may be variable, especially for small sample sizes.				When a distribution does not have free parameters (i.e., all parameters are fixed), the tests are Kolmorogov-Smirnov, and Cramér-von Mises and Anderson-Darling for simple null hypothesis. 				For normal distributions with free location and scale parameters, specific versions of goodness of fit tests are computed, appropriate for this setting. If the normal distribution has some parameters fixed, it is treated as any other distribution.
#'    Defaults to \code{FALSE}.
#' @param goodnessOfFitBootstrap, Obtain the p-value of the goodness-of-fit tests using parametric bootstrap. In this case, the test statistics are always Kolmorogov-Smirnov, and Cramér–von Mises and Anderson-Darling for simple null hypothesis.
#'    Defaults to \code{FALSE}.
#' @param outputLimit, Show the detailed output only for the top x distributions.
#'    Defaults to \code{TRUE}.
#' @param parameterEstimates, Obtain a table of parameter estimates. *Note*: All parameters are estimated with maximum likelihood.
#'    Defaults to \code{TRUE}.
compareContinuousDistributions <- function(
          data = NULL,
          version = "1",
          comparisonTable = TRUE,
          comparisonTableOrder = TRUE,
          comparisonTableOrderBy = "bic",
          distributions = list(list(distribution = "", parameters = list(), parametrization = "", value = "#")),
          empiricalPlots = FALSE,
          empiricalPlotsCi = FALSE,
          empiricalPlotsCiLevel = 0.95,
          fullDistributionSpecification = FALSE,
          goodnessOfFit = FALSE,
          goodnessOfFitBootstrap = FALSE,
          goodnessOfFitBootstrapSamples = 1000,
          outputLimit = TRUE,
          outputLimitTo = 1,
          parameterEstimates = TRUE,
          plotHeight = 320,
          plotWidth = 480,
          variable = list(types = list(), value = "")) {

   defaultArgCalls <- formals(jaspDistributions::compareContinuousDistributions)
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

   optionsWithFormula <- c("comparisonTableOrderBy", "distributions", "variable")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   return(jaspBase::runWrappedAnalysis("jaspDistributions", "compareContinuousDistributions", "CompareContinuousDistributions.qml", options, version, TRUE))
}