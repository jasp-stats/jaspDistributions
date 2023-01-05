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

LDbetaPrimeInternal <- function(jaspResults, dataset, options, state=NULL){
  options <- .ldRecodeOptionsBetaPrime(options)

  #### Show betaPrime section ----
  .ldShowDistribution(jaspResults = jaspResults, options = options, name = gettext("beta prime distribution"),
                      parSupportMoments = .ldBetaPrimeParsSupportMoments,
                      formulaPDF        = .ldFormulaBetaPrimePDF,
                      formulaCDF        = .ldFormulaBetaPrimeCDF,
                      formulaQF         = .ldFormulaBetaPrimeQF)

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
  .ldMLE(jaspResults, variable, options, ready, errors, .ldFillBetaPrimeEstimatesTable)

  return()
}

### options ----
.ldRecodeOptionsBetaPrime <- function(options){
  options[['parValNames']] <- c("alpha", "beta")

  options[['pars']]   <- list(alpha = options[['alpha']], beta = options[['beta']])
  options[['pdfFun']] <- dbetaprime
  options[['cdfFun']] <- pbetaprime
  options[['qFun']]   <- qbetaprime
  options[['rFun']]   <- rbetaprime
  options[['distNameInR']] <- "betaprime"

  options <- .ldOptionsDeterminePlotLimits(options)

  options$support <- list(min = 0, max = Inf)
  options$lowerBound <- c(0, 0)
  options$upperBound <- c(Inf, Inf)

  options$transformations <- c(alpha = "alpha", beta = "beta")

  options
}

### text fill functions -----

.ldBetaPrimeParsSupportMoments <- function(jaspResults, options){
  if(options$parsSupportMoments && is.null(jaspResults[['parsSupportMoments']])){
    pars <- list()
    pars[[1]] <- gettextf("shape: %s", "&alpha; \u2208 \u211D<sup>+</sup>")
    pars[[2]] <- gettextf("shape: %s", "&beta; \u2208 \u211D<sup>+</sup>")

    support <- "x \u2208 \u211D<sup>+</sup>"

    moments <- list()
    moments$expectation <- "&alpha;/(&alpha;-&beta;)"
    moments$variance    <- "[&alpha;(&alpha;+&beta;-1)]/[(&beta;-2)(&beta;-1)<sup>2</sup>] if &beta; > 2"

    jaspResults[['parsSupportMoments']] <- .ldParsSupportMoments(pars, support, moments)
  }
}

.ldFormulaBetaPrimePDF <- function(options){
}

.ldFormulaBetaPrimeCDF <- function(options){
}

.ldFormulaBetaPrimeQF <- function(options){
}

#### Table functions ----

.ldFillBetaPrimeEstimatesTable <- function(table, results, options, ready){
  if(!ready) return()
  if(is.null(results)) return()
  if(is.null(table)) return()

  pars <- c(alpha = "\u03B1", beta = "\u03B2")

  res <- results$structured
  res <- res[res$par %in% names(pars),]
  res$parName <- c(pars)

  if(results$fitdist$convergence != 0){
    table$addFootnote(gettext("The optimization did not converge, try adjusting the parameter values."), symbol = gettext("<i>Warning.</i>"))
  }
  if(!is.null(results$fitdist$optim.message)){
    table$addFootnote(results$fitdist$message, symbol = gettext("<i>Warning.</i>"))
  }

  table$setData(res)

  return()
}

#### Distribution functions ----

dbetaprime <- function(x, alpha, beta, log = FALSE) {
  out <- (alpha-1) * log(x) - (alpha+beta) * log1p(x) - lbeta(alpha, beta)

  if(!log) out <- exp(out)

  return(out)
}

pbetaprime <- function(q, alpha, beta, lower.tail = TRUE, log.p = FALSE) {
  x <- q/(1+q)

  out <- pbeta(x, alpha, beta, lower.tail = lower.tail, log.p = log.p)

  return(out)
}

qbetaprime <- function(p, alpha, beta, lower.tail = TRUE, log.p = FALSE) {
  x <- qbeta(p, alpha, beta, lower.tail = lower.tail, log.p = log.p)

  q <- x/(1-x)

  return(q)
}

rbetaprime <- function(n, alpha, beta) {
  x <- rbeta(n, alpha, beta)

  return(x/(1-x))
}
