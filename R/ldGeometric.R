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

LDgeometric <- function(jaspResults, dataset, options, state=NULL){
  options <- .ldRecodeOptionsGeometric(options)

  #### Show geometric section ----
  .ldShowDistribution(jaspResults = jaspResults, options = options, name = gettext("geometric distribution"),
                      parSupportMoments = .ldGeometricParsSupportMoments,
                      formulaPMF        = .ldFormulaGeometricPMF,
                      formulaCMF        = .ldFormulaGeometricCDF)

  #### Generate and Display data section ----
  # simulate and read data
  .simulateData(jaspResults, options, "ordinal")

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

  #### Fit data and assess fit ----
  .ldMLE(jaspResults, variable, options, ready, errors, .ldFillGeometricEstimatesTable)
  return()
}

### options ----
.ldRecodeOptionsGeometric <- function(options){
  options[['parValNames']] <- c("prob")

  options[['pars']]   <- list(prob = options[["prob"]])

  options[['pdfFun']] <- stats::dgeom
  options[['cdfFun']] <- stats::pgeom
  options[['qFun']]   <- stats::qgeom
  options[['rFun']]   <- stats::rgeom
  options[['distNameInR']] <- "geom"

  options <- .ldOptionsDeterminePlotLimits(options, FALSE)

  options$support <- list(min = 0, max = Inf)
  options$lowerBound <- 0
  options$upperBound <- 1

  options$transformations <- c(prob = "prob")

  options
}

### text fill functions -----
.ldGeometricParsSupportMoments <- function(jaspResults, options){
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

.ldFormulaGeometricPMF <- function(options){
}

.ldFormulaGeometricCDF <- function(options){
}

.ldFormulaGeometricQF <- function(options){
}

#### Table functions ----

.ldFillGeometricEstimatesTable <- function(table, results, options, ready){
  if(!ready) return()
  if(is.null(results)) return()
  if(is.null(table)) return()

  res <- results$structured
  res$parName <- c("p")

  if(results$fitdist$convergence != 0){
    table$addFootnote(gettext("The optimization did not converge, try adjusting the parameter values."), symbol = gettext("<i>Warning.</i>"))
  }
  if(!is.null(results$fitdist$optim.message)){
    table$addFootnote(results$fitdist$message, symbol = gettext("<i>Warning.</i>"))
  }
  table$setData(res)

  return()
}
