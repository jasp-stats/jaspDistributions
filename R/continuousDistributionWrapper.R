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

#' Continuous Distribution
#'
#' Specify a continuous distribution, optionally estimate its parameters from data, display it, and assess its fit to data.
#'
#' @param assessVariable, Variable used to assess the fit of the distribution. If empty, the fit is assessed on the variable used for estimation. Using a different variable (e.g., a hold-out sample) allows goodness-of-fit tests that treat the estimated parameters as known.
#' @param cdfPlot, Displays the cumulative distribution function.
#'    Defaults to \code{FALSE}.
#' @param dataDescriptives, Displays a descriptive table of the variable.
#'    Defaults to \code{FALSE}.
#' @param distributionTable, Displays distribution table that shows quantiles, density, cumulative probability, and complement of the cumulative probability.
#'    Defaults to \code{FALSE}.
#' @param distributionTableBasedOn, Select whether you want to specify values of quantiles for which to display density and cumulative probability, or values of cumulative probability for which to display density and quantiles.
#' @param distributionTableLength, How many points to display in the distribution table within the specified range.
#' @param distributionTableMax, The maximum of the range of the distribution table.
#' @param distributionTableMin, The minimum of the range of the distribution table.
#' @param distributionTablePoints, Values on which to evaluate the distribution.
#' @param distributionTableType, Select whether you want to specify the distribution table with a range of values, or manually.
#' @param ecdfPlot, Displays the empirical cumulative distribution function of the variable overlaid with the cumulative distribution function of the distribution.
#'    Defaults to \code{FALSE}.
#' @param explanatoryText, Displays additional texts that explain the displayed elements.
#'    Defaults to \code{FALSE}.
#' @param fitVariable, Variable used to estimate the free (not fixed) parameters of the distribution. Leave empty to use the specified parameter values as they are.
#' @param goodnessOfFit, Displays the Kolmogorov-Smirnov, Cramér-von Mises, and Anderson-Darling tests. When the parameters were estimated from the same data the fit is assessed on, the standard p-values are too conservative; use bootstrapped p-values instead.
#'    Defaults to \code{FALSE}.
#' @param histPlot, Displays a histogram of the variable overlaid with the probability density function of the distribution.
#'    Defaults to \code{FALSE}.
#' @param informationCriteria, Displays the log-likelihood, AIC, and BIC of the distribution. When assessed on a different variable than the one used for estimation, the log-likelihood is the out-of-sample (predictive) log-likelihood.
#'    Defaults to \code{FALSE}.
#' @param momentsTable, Displays a table with the raw and central moments of the variable and the moments of the distribution.
#'    Defaults to \code{FALSE}.
#' @param parameterEstimationMethod, If available, parameter estimates are calculated analytically. If not, then parameters are estimated using numerical optimization of the log-likelihood function.
#' \itemize{
#'   \item \code{"constrained"}: Optimize the log-likelihood on the constrained parameter space. The optimization method used is 'L-BFGS-B'.
#'   \item \code{"unconstrained"}: Optimize the log-likelihood on the unconstrained parameter space. For example, a parameter that is always positive is optimized on the log scale. The optimization method used is 'BFGS'.
#' }
#' @param parameterTable, Displays a table with the parameter values.
#'    Defaults to \code{TRUE}.
#' @param parameterTableUncertainty, Displays the uncertainty about the estimated parameters (e.g., standard errors and confidence intervals). The method is set in the advanced options.
#'    Defaults to \code{FALSE}.
#' @param parameterUncertaintyMethod, How to quantify uncertainty about the parameters.
#' \itemize{
#'   \item \code{"bootstrap"}: SE and CIs are calculated using parametric bootstrap.
#'   \item \code{"default"}: If available, analytic SE and CIs are calculated, otherwise, they are calculated using normal theory based on numerical estimate of the Hessian.
#' }
#' @param parameterUncertaintySymmetricCi, Should the CI be computed on the unconstrained space and then CI bounds transformed into the original parameter space, or should the CIs be computed on the constrained space using transformed Hessian. The latter produces CIs that are symmetric around the point estimate but may, in some extreme cases, produce CIs extending beyond support of the parameter.
#'    Defaults to \code{FALSE}.
#' @param parametersSupportMoments, Displays parameters, support, and moments.
#'    Defaults to \code{FALSE}.
#' @param pdfPlot, Displays the probability density function.
#'    Defaults to \code{FALSE}.
#' @param ppPlot, Displays the probability-probability plot. The *x*-axis shows the theoretical value of the cumulative distribution function of the data points under the distribution, the *y*-axis shows the empirical percentiles of the variable.
#'    Defaults to \code{FALSE}.
#' @param qfPlot, Displays the quantile function.
#'    Defaults to \code{FALSE}.
#' @param qqPlot, Displays the quantile-quantile plot. The *x*-axis shows the theoretical quantiles of the data points under the distribution, the *y*-axis shows the empirical quantiles of the variable.
#'    Defaults to \code{FALSE}.
#' @param simulateData, Draw random samples and save them as new columns in the data set. One row for the specified distribution ("specified", can be used to estimate the parameters) and one for the fitted distribution ("fitted", can only be used to assess the fit), each with \code{simulate}, \code{sampleSize}, and \code{column}. Requires a loaded data set.
ContinuousDistribution <- function(
          data = NULL,
          version = "0.97.2",
          assessVariable = list(types = list(), value = ""),
          cdfPlot = FALSE,
          dataDescriptives = FALSE,
          distribution = "",
          distributionTable = FALSE,
          distributionTableBasedOn = "quantile",
          distributionTableLength = 11,
          distributionTableMax = 1,
          distributionTableMin = 0,
          distributionTablePoints = list(),
          distributionTableType = "range",
          ecdfPlot = FALSE,
          explanatoryText = FALSE,
          fitVariable = list(types = list(), value = ""),
          goodnessOfFit = FALSE,
          goodnessOfFitBootstrapSamples = 1000,
          goodnessOfFitPValue = "asymptotic",
          histPlot = FALSE,
          informationCriteria = FALSE,
          momentsTable = FALSE,
          momentsTableNumber = 2,
          parameterEstimationMethod = "unconstrained",
          parameterTable = TRUE,
          parameterTableCiLevel = 0.95,
          parameterTableUncertainty = FALSE,
          parameterUncertaintyBootstrapSamples = 1000,
          parameterUncertaintyMethod = "default",
          parameterUncertaintySymmetricCi = FALSE,
          parameters = list(),
          parametersSupportMoments = FALSE,
          parametrization = "",
          pdfPlot = FALSE,
          plotDensityPoints = list(),
          plotHeight = 320,
          plotMax = 3,
          plotMin = -3,
          plotProbabilityLimits = list(),
          plotRangeBasedOn = "quantile",
          plotWidth = 480,
          ppPlot = FALSE,
          ppPlotCi = FALSE,
          ppPlotCiLevel = 0.95,
          qfPlot = FALSE,
          qqPlot = FALSE,
          qqPlotCi = FALSE,
          qqPlotCiLevel = 0.95,
          seed = 1,
          setSeed = FALSE,
          simulateData = list(list(column = "", sampleSize = 1, simulate = FALSE, value = "specified"), list(column = "", sampleSize = 1, simulate = FALSE, value = "fitted"))) {

   defaultArgCalls <- formals(jaspDistributions::ContinuousDistribution)
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

   optionsWithFormula <- c("assessVariable", "distribution", "distributionTableBasedOn", "distributionTablePoints", "distributionTableType", "fitVariable", "parameters", "parametrization", "plotDensityPoints", "plotProbabilityLimits", "plotRangeBasedOn")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   return(jaspBase::runWrappedAnalysis("jaspDistributions", "ContinuousDistribution", "ContinuousDistribution.qml", options, version, TRUE))
}