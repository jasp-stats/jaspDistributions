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

#' Single Distribution
#'
#' Specify a continuous distribution, optionally estimate its parameters from data, display it, and assess its fit to data.
#'
#' @param cdfPlot, Displays the cumulative distribution function.
#'    Defaults to \code{FALSE}.
#' @param descriptivesTable, Displays a descriptive table of the variable used for estimation and, if different, of the variable used for assessing the fit.
#'    Defaults to \code{FALSE}.
#' @param distributionPlotCdfMax, The cumulative probability of the largest quantile shown in the plots.
#' @param distributionPlotCdfMin, The cumulative probability of the smallest quantile shown in the plots.
#' @param distributionPlotHighlightDensity, Quantiles at which the density is highlighted: as a point in the probability density plot, and as the slope of a tangent in the cumulative distribution plot.
#' @param distributionPlotHighlightProbability, Intervals whose probability is highlighted in the probability density and cumulative distribution plots. Use -Inf or Inf to leave the interval unbounded on that side (e.g., to highlight a tail).
#' @param distributionPlotQuantileMax, The largest quantile shown in the plots.
#' @param distributionPlotQuantileMin, The smallest quantile shown in the plots.
#' @param distributionPlotRangeBasedOn, Select whether the range of the plots is determined automatically from the support of the distribution, or specified in quantiles or in cumulative probabilities.
#' @param distributionTable, Displays distribution table that shows quantiles, density, cumulative probability, and complement of the cumulative probability.
#'    Defaults to \code{FALSE}.
#' @param distributionTableBasedOn, Select whether you want to specify values of quantiles for which to display density and cumulative probability, or values of cumulative probability for which to display density and quantiles.
#' @param distributionTableCdfMax, The largest cumulative probability in the distribution table.
#' @param distributionTableCdfMin, The smallest cumulative probability in the distribution table.
#' @param distributionTableCdfPoints, Cumulative probabilities at which to evaluate the distribution.
#' @param distributionTablePointCount, How many equally spaced points to display in the distribution table within the specified range.
#' @param distributionTableQuantileMax, The largest quantile in the distribution table.
#' @param distributionTableQuantileMin, The smallest quantile in the distribution table.
#' @param distributionTableQuantilePoints, Quantiles at which to evaluate the distribution.
#' @param distributionTableType, Select whether you want to specify the distribution table with a range of values, or manually.
#' @param ecdfPlot, Displays the empirical cumulative distribution function of the variable overlaid with the cumulative distribution function of the distribution.
#'    Defaults to \code{FALSE}.
#' @param estimationVariable, Variable used to estimate the free (not fixed) parameters of the distribution. Leave empty to use the specified parameter values as they are.
#' @param explanatoryText, Displays additional texts that explain the displayed elements.
#'    Defaults to \code{FALSE}.
#' @param fitAssessmentVariable, Variable used to assess the fit of the distribution. If empty, the fit is assessed on the variable used for estimation. Using a different variable (e.g., a hold-out sample) allows goodness-of-fit tests that treat the estimated parameters as known.
#' @param goodnessOfFitTest, Displays goodness-of-fit tests. If the parameters were not fitted to the variable on which the fit is assessed (i.e., all parameters are fixed, or they were estimated on a different variable), the tests are Kolmogorov-Smirnov, and Cramér-von Mises and Anderson-Darling for a simple null hypothesis. If they were fitted to it, the tests are Cramér-von Mises and Anderson-Darling for a composite null hypothesis (Braun, 1980). These rely on randomly splitting the data, so their p-values can be unstable for small sample sizes; in that case, bootstrap is recommended. For the normal distribution with free location and scale, dedicated tests of normality are computed instead: Lilliefors, Cramér-von Mises, Anderson-Darling, Shapiro-Wilk, and Shapiro-Francia.
#'    Defaults to \code{FALSE}.
#' @param histogramPlot, Displays a histogram of the variable overlaid with the probability density function of the distribution.
#'    Defaults to \code{FALSE}.
#' @param modelFitTable, Displays the log-likelihood, the number of free parameters, and the AIC and BIC of the distribution. When assessed on a different variable than the one used for estimation, the log-likelihood is the out-of-sample (predictive) log-likelihood, and the AIC and BIC are not shown.
#'    Defaults to \code{FALSE}.
#' @param parameterEstimator, How the parameters are estimated. Default uses maximum spacing for shifted distributions, and otherwise the bias-corrected estimator if available (e.g., normal, exponential), and maximum likelihood if not.
#' @param parameterEstimatorOptimizationSpace, Parameter space on which the estimates are optimized if they cannot be calculated analytically: unconstrained (e.g., positive parameters on the log scale; 'BFGS') or constrained ('L-BFGS-B').
#' @param parameterTable, Displays a table with the parameter values.
#'    Defaults to \code{TRUE}.
#' @param parameterUncertainty, Displays the uncertainty about the estimated parameters (e.g., standard errors and confidence intervals) in the parameter table.
#'    Defaults to \code{FALSE}.
#' @param parameterUncertaintyBootstrapCiMethod, Percentile: quantiles of the bootstrap estimates. Basic: bootstrap estimates reflected around the estimate.
#' @param parameterUncertaintyBootstrapSamples, Number of bootstrap samples on which the parameters are re-estimated.
#' @param parameterUncertaintyBootstrapType, Draw the bootstrap samples from the fitted distribution (parametric) or resample the data (nonparametric).
#' @param parameterUncertaintyMethod, How the standard errors and confidence intervals are calculated. Default uses exact intervals if available (e.g., normal, uniform), and normal theory otherwise. Normal theory assumes maximum likelihood estimates. Bootstrap is unreliable for parameters that bound the support (e.g., of the uniform).
#' @param parameterUncertaintyNormalTheorySpace, Compute the confidence intervals on the unconstrained parameter space and transform their bounds back, or directly on the constrained parameter space. The latter produces intervals that are symmetric around the estimate, but that may extend beyond the support of the parameter.
#' @param parametersSupportMoments, Displays parameters, support, and moments.
#'    Defaults to \code{FALSE}.
#' @param pdfPlot, Displays the probability density function.
#'    Defaults to \code{TRUE}.
#' @param ppPlot, Displays the probability-probability plot. The *x*-axis shows the theoretical value of the cumulative distribution function of the data points under the distribution, the *y*-axis shows the empirical percentiles of the variable.
#'    Defaults to \code{FALSE}.
#' @param qfPlot, Displays the quantile function.
#'    Defaults to \code{FALSE}.
#' @param qqPlot, Displays the quantile-quantile plot. The *x*-axis shows the theoretical quantiles of the data points under the distribution, the *y*-axis shows the empirical quantiles of the variable.
#'    Defaults to \code{FALSE}.
#' @param simulatedDataSampleSize, Number of values to draw. Remaining rows of the column are left empty; cannot exceed the number of rows in the data set.
#' @param simulatedDataSavedToData, Draws a random sample from the specified distribution (using the specified parameter values). The column can be used to estimate the parameters, e.g., to see how well they are recovered.
#'    Defaults to \code{FALSE}.
#' @param simulatedDataSavedToDataColumn, Name of the column in which the simulated data are saved.
ContinuousDistribution <- function(
          data = NULL,
          version = "0.97.3",
          cdfPlot = FALSE,
          descriptivesTable = FALSE,
          distribution = "",
          distributionPlotCdfMax = 0.99,
          distributionPlotCdfMin = 0.01,
          distributionPlotHighlightDensity = list(),
          distributionPlotHighlightProbability = list(),
          distributionPlotQuantileMax = 3,
          distributionPlotQuantileMin = -3,
          distributionPlotRangeBasedOn = "auto",
          distributionTable = FALSE,
          distributionTableBasedOn = "cdf",
          distributionTableCdfMax = 1,
          distributionTableCdfMin = 0,
          distributionTableCdfPoints = list(),
          distributionTablePointCount = 11,
          distributionTableQuantileMax = 3,
          distributionTableQuantileMin = -3,
          distributionTableQuantilePoints = list(),
          distributionTableType = "range",
          ecdfPlot = FALSE,
          estimationVariable = list(types = "unknown", value = ""),
          explanatoryText = FALSE,
          fitAssessmentVariable = list(types = "unknown", value = ""),
          goodnessOfFitTest = FALSE,
          goodnessOfFitTestBootstrapSamples = 1000,
          goodnessOfFitTestPValue = "asymptotic",
          histogramPlot = FALSE,
          modelFitTable = FALSE,
          parameterEstimator = "default",
          parameterEstimatorOptimizationSpace = "unconstrained",
          parameterTable = TRUE,
          parameterUncertainty = FALSE,
          parameterUncertaintyBootstrapCiMethod = "percentile",
          parameterUncertaintyBootstrapSamples = 1000,
          parameterUncertaintyBootstrapType = "parametric",
          parameterUncertaintyCiLevel = 0.95,
          parameterUncertaintyMethod = "default",
          parameterUncertaintyNormalTheorySpace = "unconstrained",
          parameters = list(),
          parametersSupportMoments = FALSE,
          parametrization = "",
          pdfPlot = TRUE,
          plotHeight = 320,
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
          simulatedDataRedraw = 0,
          simulatedDataSampleSize = 1,
          simulatedDataSavedToData = FALSE,
          simulatedDataSavedToDataColumn = "") {

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

   optionsWithFormula <- c("distribution", "distributionPlotHighlightDensity", "distributionPlotHighlightProbability", "distributionPlotRangeBasedOn", "distributionTableBasedOn", "distributionTableCdfPoints", "distributionTableQuantilePoints", "distributionTableType", "estimationVariable", "fitAssessmentVariable", "parameterEstimator", "parameterEstimatorOptimizationSpace", "parameterUncertaintyBootstrapCiMethod", "parameterUncertaintyBootstrapType", "parameterUncertaintyMethod", "parameterUncertaintyNormalTheorySpace", "parameters", "parametrization")
   for (name in optionsWithFormula) {
      if ((name %in% optionsWithFormula) && inherits(options[[name]], "formula")) options[[name]] = jaspBase::jaspFormula(options[[name]], data)   }

   return(jaspBase::runWrappedAnalysis("jaspDistributions", "ContinuousDistribution", "ContinuousDistribution.qml", options, version, TRUE))
}