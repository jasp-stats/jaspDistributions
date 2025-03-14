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

LDnormalGeneralizedInternal <- function(jaspResults, dataset, options, state=NULL){
  options <- .recodeOptionsLDNormalGeneralized(options)

  #### Show distribution section ----
  .ldShowDistribution(jaspResults = jaspResults, options = options, name = gettext("generalized normal distribution"),
                      parSupportMoments = .ldNormalGeneralizedParsSupportMoments,
                      formulaPDF        = .ldFormulaNormalGeneralizedPDF,
                      formulaCDF        = .ldFormulaNormalGeneralizedCDF,
                      formulaQF         = .ldFormulaNormalGeneralizedQF)

  #### Generate and Display data section ----
  # simulate and read data
  .simulateData(jaspResults, options)

  ready <- options[['variable']] != ""
  errors <- FALSE
  if(ready){
    variable <- dataset[[options[['variable']]]]
    variable <- variable[!is.na(variable)]
    errors <- .hasErrors(dataset, type = c("observations", "variance", "infinity", "limits"),
                         observations.amount = "<2",
                         limits.min = options$support$min, limits.max = options$support$max,
                         exitAnalysisIfErrors = FALSE)
  }

  # overview of the data
  .ldDescriptives(jaspResults, variable, options, ready, errors, "continuous")

  #### Fit data and assess fit ----
  .ldMLE(jaspResults, variable, options, ready, errors, .ldFillNormalGeneralizedEstimatesTable)

  return()
}

### options ----
.recodeOptionsLDNormalGeneralized <- function(options){
  options[['parValNames']] <- c("mu", "alpha", "beta")

  options[['pars']]   <- list(mu = options[['mu']], alpha = options[['alpha']], beta = options[['beta']])
  options[['pdfFun']] <- gnorm::dgnorm
  options[['cdfFun']] <- gnorm::pgnorm
  options[['qFun']]   <- gnorm::qgnorm
  options[['rFun']]   <- gnorm::rgnorm
  options[['distNameInR']] <- "gnorm"

  options <- .ldOptionsDeterminePlotLimits(options)

  options$support <- list(min = -Inf, max = Inf)
  options$lowerBound <- c(-Inf, 0, 0)
  options$upperBound <- c(Inf, Inf, Inf)

  options$transformations <- c(mu = "mu", alpha = "alpha", beta = "beta")

  options
}

### text fill functions -----
.ldNormalGeneralizedParsSupportMoments <- function(jaspResults, options){
  if(options$parsSupportMoments && is.null(jaspResults[['parsSupportMoments']])){
    pars <- list()
    pars[[1]] <- gettextf("location: &mu; %s","\u2208 \u211D")
    pars[[2]] <- gettextf("scale: %s", "&alpha; \u2208 \u211D<sup>+</sup>")
    pars[[3]] <- gettextf("shape: %s", "&beta; \u2208 \u211D<sup>+</sup>")

    support <- "x \u2208 \u211D"

    moments <- list()
    moments$expectation <- "&mu;"
    moments$variance <- gettextf("see Wikipedia: %s", "https://en.wikipedia.org/wiki/Generalized_normal_distribution#Moments")

    jaspResults[['parsSupportMoments']] <- .ldParsSupportMoments(pars, support, moments)
  }
}

.ldFormulaNormalGeneralizedPDF <- function(options){

}

.ldFormulaNormalGeneralizedCDF <- function(options){

}

.ldFormulaNormalGeneralizedQF <- function(options){

}

#### Table functions ----

.ldFillNormalGeneralizedEstimatesTable <- function(table, results, options, ready){
  if(!ready) return()
  if(is.null(results)) return()
  if(is.null(table)) return()

  pars <- c(mu = "\u03BC", alpha = "\u03B1", beta = "\u03B2")
  res <- results$structured
  res <- res[res$par %in% names(pars),]
  res$parName <- pars

  if(results$fitdist$convergence != 0){
    table$addFootnote(gettext("The optimization did not converge, try adjusting the parameter values."), symbol = gettext("<i>Warning.</i>"))
  }
  if(!is.null(results$fitdist$optim.message)){
    table$addFootnote(results$fitdist$message, symbol = gettext("<i>Warning.</i>"))
  }

  table$setData(res)

  return()
}
