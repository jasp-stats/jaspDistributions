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
#'    Defaults to \code{TRUE}.
#' @param empiricalPlotsCi, Add the confidence interval to the P-P and Q-Q plots.
#'    Defaults to \code{TRUE}.
#' @param fullDistributionName, Displays the full distribution name, including parameter specification. If unchecked, names are shortened.
#'    Defaults to \code{TRUE}.
#' @param goodnessOfFit, Compute goodness of fit tests. For most of the distributions, the default tests are Cramér-von Mises and Anderson-Darling for composite null hypothesis. Note that these tests rely on randomly splitting the data in two sets;				as a result, the results may be variable, especially for small sample sizes.				When a distribution does not have free parameters (i.e., all parameters are fixed), the tests are Kolmorogov-Smirnov, and Cramér-von Mises and Anderson-Darling for simple null hypothesis. 				For normal distributions with free location and scale parameters, specific versions of goodness of fit tests are computed, appropriate for this setting. If the normal distribution has some parameters fixed, it is treated as any other distribution.
#'    Defaults to \code{TRUE}.
#' @param goodnessOfFitBootstrap, Obtain the p-value of the goodness-of-fit tests using parametric bootstrap. In this case, the test statistics are always Kolmorogov-Smirnov, and Cramér–von Mises and Anderson-Darling for simple null hypothesis.
#'    Defaults to \code{FALSE}.
#' @param outputLimit, Show the detailed output only for the top x distributions.
#'    Defaults to \code{TRUE}.
#' @param parameterEstimates, Obtain a table of parameter estimates. *Note*: All parameters are estimated with maximum likelihood.
#'    Defaults to \code{TRUE}.
compareContinuousDistributions <- function(
    data = NULL,
    version = "0.96",
    comparisonTable = TRUE,
    comparisonTableOrder = TRUE,
    comparisonTableOrderBy = "bic",
    distributions = list(list(amorosoA = 0, amorosoAFixed = FALSE, amorosoAlpha = 2, amorosoAlphaFixed = FALSE, amorosoBeta = 1, amorosoBetaFixed = FALSE, amorosoTheta = 1, amorosoThetaFixed = FALSE, betaAlpha = 2, betaAlphaFixed = FALSE, betaBeta = 2, betaBetaFixed = FALSE, betaPrimeAlpha = 2, betaPrimeAlphaFixed = FALSE, betaPrimeBeta = 2, betaPrimeBetaFixed = FALSE, cauchyMu = 0, cauchyMuFixed = FALSE, cauchySigma = 1, cauchySigmaFixed = FALSE, centralFNu1 = 5, centralFNu1Fixed = FALSE, centralFNu2 = 5, centralFNu2Fixed = FALSE, chiSquaredNu = 5, chiSquaredNuFixed = FALSE, distribution = "Normal", exponentialBeta = 1, exponentialBetaFixed = FALSE, exponentialLambda = 1, exponentialLambdaFixed = FALSE, frechetAlpha = 2, frechetAlphaFixed = FALSE, frechetSigma = 1, frechetSigmaFixed = FALSE, frechetTheta = 0, frechetThetaFixed = FALSE, gammaAlpha = 2, gammaAlphaFixed = FALSE, gammaLambda = 1, gammaLambdaFixed = FALSE, gammaMu = 1, gammaMuFixed = FALSE, gammaTheta = 1, gammaThetaFixed = FALSE, gompertzBeta = 1, gompertzBetaFixed = FALSE, gompertzEta = 1, gompertzEtaFixed = FALSE, gumbelBeta = 1, gumbelBetaFixed = FALSE, gumbelMu = 0, gumbelMuFixed = FALSE, inverseGammaAlpha = 2, inverseGammaAlphaFixed = FALSE, inverseGammaLambda = 1, inverseGammaLambdaFixed = FALSE, inverseGammaMu = 1, inverseGammaMuFixed = FALSE, inverseGammaTheta = 1, inverseGammaThetaFixed = FALSE, laplaceBeta = 1, laplaceBetaFixed = FALSE, laplaceMu = 0, laplaceMuFixed = FALSE, logLogisticAlpha = 1, logLogisticAlphaFixed = FALSE, logLogisticBeta = 2, logLogisticBetaFixed = FALSE, logLogisticMu = 0, logLogisticMuFixed = FALSE, logLogisticSigma = 1, logLogisticSigmaFixed = FALSE, logNormalMu = 0, logNormalMuFixed = FALSE, logNormalSigma = 1, logNormalSigmaFixed = FALSE, logisticMu = 0, logisticMuFixed = FALSE, logisticSigma = 1, logisticSigmaFixed = FALSE, noncentralChiSquaredKappa = 1, noncentralChiSquaredKappaFixed = FALSE, noncentralChiSquaredNu = 5, noncentralChiSquaredNuFixed = FALSE, noncentralFKappa = 1, noncentralFKappaFixed = FALSE, noncentralFNu1 = 5, noncentralFNu1Fixed = FALSE, noncentralFNu2 = 5, noncentralFNu2Fixed = FALSE, noncentralStudentTKappa = 0, noncentralStudentTKappaFixed = FALSE, noncentralStudentTMu = 0, noncentralStudentTMuFixed = FALSE, noncentralStudentTNu = 5, noncentralStudentTNuFixed = FALSE, noncentralStudentTSigma = 1, noncentralStudentTSigmaFixed = FALSE, noncentralTKappa = 0, noncentralTKappaFixed = FALSE, noncentralTNu = 5, noncentralTNuFixed = FALSE, normalMu = 0, normalMuFixed = FALSE, normalSigma = 1, normalSigma2 = 1, normalSigma2Fixed = FALSE, normalSigmaFixed = FALSE, normalTau = 1, normalTauFixed = FALSE, parameters = "sigma", paretoAlpha = 2, paretoAlphaFixed = FALSE, paretoBeta = 1, paretoBetaFixed = FALSE, settings = FALSE, shiftedExponentialBeta = 1, shiftedExponentialBetaFixed = FALSE, shiftedExponentialLambda = 1, shiftedExponentialLambdaFixed = FALSE, shiftedExponentialShift = 0, shiftedExponentialShiftFixed = FALSE, shiftedGammaAlpha = 2, shiftedGammaAlphaFixed = FALSE, shiftedGammaLambda = 1, shiftedGammaLambdaFixed = FALSE, shiftedGammaMu = 1, shiftedGammaMuFixed = FALSE, shiftedGammaShift = 0, shiftedGammaShiftFixed = FALSE, shiftedGammaTheta = 1, shiftedGammaThetaFixed = FALSE, shiftedInverseGammaAlpha = 2, shiftedInverseGammaAlphaFixed = FALSE, shiftedInverseGammaLambda = 1, shiftedInverseGammaLambdaFixed = FALSE, shiftedInverseGammaMu = 1, shiftedInverseGammaMuFixed = FALSE, shiftedInverseGammaShift = 0, shiftedInverseGammaShiftFixed = FALSE, shiftedInverseGammaTheta = 1, shiftedInverseGammaThetaFixed = FALSE, shiftedLogLogisticAlpha = 1, shiftedLogLogisticAlphaFixed = FALSE, shiftedLogLogisticBeta = 2, shiftedLogLogisticBetaFixed = FALSE, shiftedLogLogisticMu = 0, shiftedLogLogisticMuFixed = FALSE, shiftedLogLogisticShift = 0, shiftedLogLogisticShiftFixed = FALSE, shiftedLogLogisticSigma = 1, shiftedLogLogisticSigmaFixed = FALSE, shiftedLogNormalMu = 0, shiftedLogNormalMuFixed = FALSE, shiftedLogNormalShift = 0, shiftedLogNormalShiftFixed = FALSE, shiftedLogNormalSigma = 1, shiftedLogNormalSigmaFixed = FALSE, shiftedWaldAlpha = 1, shiftedWaldAlphaFixed = FALSE, shiftedWaldLambda = 1, shiftedWaldLambdaFixed = FALSE, shiftedWaldMu = 1, shiftedWaldMuFixed = FALSE, shiftedWaldNu = 1, shiftedWaldNuFixed = FALSE, shiftedWaldShift = 0, shiftedWaldShiftFixed = FALSE, shiftedWaldSigma = 1, shiftedWaldSigmaFixed = FALSE, shiftedWeibullScale = 1, shiftedWeibullScaleFixed = FALSE, shiftedWeibullShape = 2, shiftedWeibullShapeFixed = FALSE, shiftedWeibullShift = 0, shiftedWeibullShiftFixed = FALSE, skewCauchyAlpha = 0, skewCauchyAlphaFixed = FALSE, skewCauchyOmega = 1, skewCauchyOmegaFixed = FALSE, skewCauchyXi = 0, skewCauchyXiFixed = FALSE, skewNormalAlpha = 0, skewNormalAlphaFixed = FALSE, skewNormalOmega = 1, skewNormalOmegaFixed = FALSE, skewNormalXi = 0, skewNormalXiFixed = FALSE, skewTAlpha = 0, skewTAlphaFixed = FALSE, skewTNu = 5, skewTNuFixed = FALSE, skewTOmega = 1, skewTOmegaFixed = FALSE, skewTXi = 0, skewTXiFixed = FALSE, skewedGeneralizedTLambda = 0, skewedGeneralizedTLambdaFixed = FALSE, skewedGeneralizedTMu = 0, skewedGeneralizedTMuFixed = FALSE, skewedGeneralizedTP = 2, skewedGeneralizedTPFixed = FALSE, skewedGeneralizedTQ = 2, skewedGeneralizedTQFixed = FALSE, skewedGeneralizedTSigma = 1, skewedGeneralizedTSigmaFixed = FALSE, standardTNu = 5, standardTNuFixed = FALSE, stretchedBetaAlpha = 2, stretchedBetaAlphaFixed = FALSE, stretchedBetaBeta = 2, stretchedBetaBetaFixed = FALSE, stretchedBetaMax = 1, stretchedBetaMin = 0, studentTMu = 0, studentTMuFixed = FALSE, studentTNu = 5, studentTNuFixed = FALSE, studentTSigma = 1, studentTSigmaFixed = FALSE, symmetricGeneralizedNormalAlpha = 1, symmetricGeneralizedNormalAlphaFixed = FALSE, symmetricGeneralizedNormalBeta = 2, symmetricGeneralizedNormalBetaFixed = FALSE, symmetricGeneralizedNormalMu = 0, symmetricGeneralizedNormalMuFixed = FALSE, triangularA = 0, triangularAFixed = FALSE, triangularB = 1, triangularBFixed = FALSE, triangularC = 0.5, triangularCFixed = FALSE, uniformMax = 1, uniformMaxFixed = FALSE, uniformMin = 0, uniformMinFixed = FALSE, value = "#", waldAlpha = 1, waldAlphaFixed = FALSE, waldLambda = 1, waldLambdaFixed = FALSE, waldMu = 1, waldMuFixed = FALSE, waldNu = 1, waldNuFixed = FALSE, waldSigma = 1, waldSigmaFixed = FALSE, weibullScale = 1, weibullScaleFixed = FALSE, weibullShape = 2, weibullShapeFixed = FALSE)),
    empiricalPlots = FALSE,
    empiricalPlotsCi = FALSE,
    empiricalPlotsCiLevel = 0.95,
    fullDistributionName = FALSE,
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
