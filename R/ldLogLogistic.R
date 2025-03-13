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

LDlogLogisticInternal <- function(jaspResults, dataset, options, state=NULL) {
  options <- .ldRecodeOptionsLogLogistic(options)

  #### Show distribution section ----
  .ldShowDistribution(jaspResults = jaspResults, options = options, name = gettext("log-logistic distribution"),
                      parSupportMoments = .ldLogLogisticParsSupportMoments,
                      formulaPDF        = .ldFormulaLogLogisticPDF,
                      formulaCDF        = .ldFormulaLogLogisticCDF,
                      formulaQF         = .ldFormulaLogLogisticQF)

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
  .ldMLE(jaspResults, variable, options, ready, errors, .ldFillLogLogisticEstimatesTable)

  return()
}

.ldRecodeOptionsLogLogistic <- function(options){
  options[['parValNames']] <- c("par1", "par2")
  if(options[['parametrization']] == "musigma") {
    options[['alpha']] <- exp(options[['par1']])
    options[['beta']]  <- 1 / options[['par2']]
  } else {
    options[['alpha']] <- options[['par1']]
    options[['beta']]  <- options[['par2']]
  }
  options[['pars']]   <- list(alpha = options[['alpha']], beta = options[['beta']])
  options[['pdfFun']] <- dloglogis
  options[['cdfFun']] <- ploglogis
  options[['qFun']]   <- qloglogis
  options[['rFun']]   <- rloglogis
  options[['distNameInR']] <- "loglogis"

  options <- .ldOptionsDeterminePlotLimits(options)

  options$support <- list(min = -Inf, max = Inf)
  options$lowerBound <- c(0, 0)
  options$upperBound <- c(Inf, Inf)

  options$transformations <- c(mu = "log(alpha)", sigma = "1/beta", alpha = "alpha", beta = "beta")

  options
}

### text fill functions -----
.ldLogLogisticParsSupportMoments <- function(jaspResults, options){
  if(options$parsSupportMoments && is.null(jaspResults[['parsSupportMoments']])){
    pars <- list()

    if(options[["parametrization"]] == "musigma") {
      pars[[1]] <- gettextf("log location: &mu; %s","\u2208 \u211D")
      pars[[2]] <- gettextf("log scale: %s", "&sigma; \u2208 \u211D<sup>+</sup>")
    } else {
      pars[[1]] <- gettextf("scale: &alpha; %s","\u2208 \u211D<sup>+</sup>")
      pars[[2]] <- gettextf("shape: %s", "&beta; \u2208 \u211D<sup>+</sup>")
    }

    support <- "x \u2208 \u211D"

    moments <- list()
    moments$expectation <- gettext("see Wikipedia: https://en.wikipedia.org/wiki/Log-logistic_distribution#Moments")
    moments$variance    <- gettext("see Wikipedia: https://en.wikipedia.org/wiki/Log-logistic_distribution#Moments")

    jaspResults[['parsSupportMoments']] <- .ldParsSupportMoments(pars, support, moments)
  }
}

.ldFormulaLogLogisticPDF <- function(options){
}

.ldFormulaLogLogisticCDF <- function(options){
}

.ldFormulaLogLogisticQF <- function(options){
}

#### Table functions ----

.ldFillLogLogisticEstimatesTable <- function(table, results, options, ready){
  if(!ready) return()
  if(is.null(results)) return()
  if(is.null(table)) return()

  if(options[["parametrization"]] == "musigma") {
    par <- c(mu = "\u03BC", sigma = "\u03C3")
  } else {
    par <- c(alpha = "\u03B1", beta = "\u03B2")
  }

  res <- results$structured
  res <- res[res$par %in% names(par),]
  res$parName <- par

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

dloglogis <- function(x, alpha, beta, log = FALSE) {
  out <- log(beta) - log(alpha) + (beta-1) * (log(x) - log(alpha)) - 2 * log1p((x/alpha)^beta)

  if(!log) out <- exp(out)

  return(out)
}

ploglogis <- function(q, alpha, beta, lower.tail = TRUE, log.p = FALSE) {
  out <- 1 / (1+(q/alpha)^(-beta))

  if(!lower.tail) out <- 1-out
  if(log.p) out <- log(out)

  return(out)
}

qloglogis <- function(p, alpha, beta, lower.tail = TRUE, log.p = FALSE) {
  out <- stats::qlogis(p = p, location = log(alpha), scale = 1/beta, lower.tail = lower.tail, log.p = log.p)
  out <- exp(out)

  return(out)
}

rloglogis <- function(n, alpha, beta) {
  q <- stats::rlogis(n, location = log(alpha), scale = 1/beta)
  q <- exp(q)

  return(q)
}
