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

LDlaplace <- function(jaspResults, dataset, options, state=NULL){
  options <- .recodeOptionsLDLaplace(options)

  #### Show distribution section ----
  .ldShowDistribution(jaspResults = jaspResults, options = options, name = gettext("Laplace distribution"),
                      parSupportMoments = .ldLaplaceParsSupportMoments,
                      formulaPDF        = .ldFormulaLaplacePDF,
                      formulaCDF        = .ldFormulaLaplaceCDF,
                      formulaQF         = .ldFormulaLaplaceQF)

  #### Generate and Display data section ----
  # simulate and read data
  .simulateData(jaspResults, options)

  ready <- options[['variable']] != ""
  errors <- FALSE
  if(ready && is.null(dataset)){
    dataset <- .readDataSetToEnd(columns.as.numeric = options[['variable']])

    variable <- dataset[[.v(options[['variable']])]]
    variable <- variable[!is.na(variable)]
    errors <- .hasErrors(dataset, type = c("observations", "variance", "infinity", "limits"),
                         observations.amount = "<2",
                         limits.min = options$support$min, limits.max = options$support$max,
                         exitAnalysisIfErrors = FALSE)
  }

  # overview of the data
  .ldDescriptives(jaspResults, variable, options, ready, errors, "continuous")

  #### Fit data and assess fit ----
  .ldMLE(jaspResults, variable, options, ready, errors, .ldFillLaplaceEstimatesTable)

  return()
}

### options ----
.recodeOptionsLDLaplace <- function(options){
  options[['parValNames']] <- c("mu", "beta")

  options[['pars']]   <- list(mu = options[['mu']], beta = options[['beta']])
  options[['pdfFun']] <- function(x, mu, beta, ...) gnorm::dgnorm(x = x, mu = mu, alpha = beta, beta = 1, ...)
  options[['cdfFun']] <- function(q, mu, beta, ...) gnorm::pgnorm(q = q, mu = mu, alpha = beta, beta = 1, ...)
  options[['qFun']]   <- function(p, mu, beta, ...) gnorm::qgnorm(p = p, mu = mu, alpha = beta, beta = 1, ...)
  options[['rFun']]   <- function(n, mu, beta, ...) gnorm::rgnorm(n = n, mu = mu, alpha = beta, beta = 1, ...)
  options[['distNameInR']] <- "gnorm"

  options <- .ldOptionsDeterminePlotLimits(options)

  options$support <- list(min = -Inf, max = Inf)
  options$lowerBound <- c(-Inf,   0)
  options$upperBound <- c( Inf, Inf)

  options$transformations <- c(mu = "mu", beta = "beta")

  options
}

### text fill functions -----
.ldLaplaceParsSupportMoments <- function(jaspResults, options){
  if(options$parsSupportMoments && is.null(jaspResults[['parsSupportMoments']])){
    pars <- list()
    pars[[1]] <- gettextf("location: &mu; %s","\u2208 \u211D")
    pars[[2]] <- gettextf("scale: %s", "&beta; \u2208 \u211D<sup>+</sup>")

    support <- "x \u2208 \u211D"

    moments <- list()
    moments$expectation <- "&mu;"
    moments$variance <- "2&beta;<sup>2</sup>"

    jaspResults[['parsSupportMoments']] <- .ldParsSupportMoments(pars, support, moments)
  }
}

.ldFormulaLaplacePDF <- function(options){

}

.ldFormulaLaplaceCDF <- function(options){

}

.ldFormulaLaplaceQF <- function(options){

}

#### Table functions ----

.ldFillLaplaceEstimatesTable <- function(table, results, options, ready){
  if(!ready) return()
  if(is.null(results)) return()
  if(is.null(table)) return()

  pars <- c(mu = "\u03BC", beta = "\u03B2")
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
