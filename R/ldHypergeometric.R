#
# Copyright (C) 2013-2020 University of Amsterdam
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

LDhypergeometricInternal <- function(jaspResults, dataset, options, state=NULL){
  options <- .ldRecodeOptionsHypergeometric(options)
  #### Show hypergeometric section ----
  .ldShowDistribution(jaspResults = jaspResults, options = options, name = gettext("hypergeometric distribution"),
                      parSupportMoments = .ldHypergeometricParsSupportMoments,
                      formulaPMF        = .ldFormulaHypergeometricPMF,
                      formulaCMF        = .ldFormulaHypergeometricCDF)

  #### Generate and Display data section ----
  # simulate and read data
  .simulateData(jaspResults, options, "ordinal", "nn")

  ready <- options[['variable']] != ""
  errors <- FALSE
  if(ready && is.null(dataset)){
    dataset <- .readDataSetToEnd(columns.as.numeric = options[['variable']])

    variable <- dataset[[.v(options[['variable']])]]
    variable <- variable[!is.na(variable)]
    errors <- .hasErrors(dataset, type = c("observations", "variance", "infinity", "limits"),
                         observations.amount = "<1",
                         limits.min = options$support$min, limits.max = options$support$max,
                         exitAnalysisIfErrors = FALSE)
    errors <- .ldCheckInteger(variable, errors)
  }

  # overview of the data
  .ldDescriptives(jaspResults, variable, options, ready, errors, "discrete")
  return()
}

### options ----
.ldRecodeOptionsHypergeometric <- function(options){
  options[['parValNames']] <- c("size", "success", "draws")

  options[['pars']]   <- list(m = options[['success']], n = options[['size']] - options[['success']], k = options[['draws']])

  options[['pdfFun']] <- stats::dhyper
  options[['cdfFun']] <- stats::phyper
  options[['qFun']]   <- stats::qhyper
  options[['rFun']]   <- stats::rhyper
  options[['distNameInR']] <- "hyper"

  options <- .ldOptionsDeterminePlotLimits(options, FALSE)

  options$support <- list(min = 0, max = options[['size']])
  options$lowerBound <- c(0, 0, 0)
  options$upperBound <- c(Inf, Inf, Inf)

  options$transformations <- c(size = "m + n", success = "m", draws = "k")

  options
}

### text fill functions -----
.ldHypergeometricParsSupportMoments <- function(jaspResults, options){
  if(options$parsSupportMoments && is.null(jaspResults[['parsSupportMoments']])){
    pars <- list()
    pars[[1]] <- gettextf("population size: %s", "N \u2208 {0, 1, 2, ...}")
    pars[[2]] <- gettextf("number of success states in the population: %s", "K \u2208 {0, 1, 2, ..., N}")
    pars[[3]] <- gettextf("number of draws from the population: %s", "n \u2208 {0, 1, 2, ..., N}")

    support <- "number of successes drawn from the population: x \u2208 {0, 1, 2, ..., n}"

    moments <- list()
    moments$expectation <- "n &times; K/N"
    moments$variance <- "n &times; K/N &times; (N-K)/N &times; (N-n)/(N-1)"

    jaspResults[['parsSupportMoments']] <- .ldParsSupportMoments(pars, support, moments)
  }
}

.ldFormulaHypergeometricPMF <- function(options){
}

.ldFormulaHypergeometricCDF <- function(options){
}

.ldFormulaHypergeometricQF <- function(options){
}
