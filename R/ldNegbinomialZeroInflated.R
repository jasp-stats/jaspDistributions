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

LDnegbinomialZeroInflated <- function(jaspResults, dataset, options, state=NULL){
  options <- .ldRecodeOptionsNegbinomialZeroInflated(options)

  #### Show negbinomialZeroInflated section ----
  .ldShowDistribution(jaspResults = jaspResults, options = options, name = gettext("zero-inflated negative binomial distribution"),
                      parSupportMoments = .ldNegbinomialZeroInflatedParsSupportMoments,
                      formulaPMF        = .ldFormulaNegbinomialZeroInflatedPMF,
                      formulaCMF        = .ldFormulaNegbinomialZeroInflatedCDF)

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
    errors <- .ldCheckInteger(variable, errors)
  }

  # overview of the data
  .ldDescriptives(jaspResults, variable, options, ready, errors, "discrete")

  #### Fit data and assess fit ----
  .ldMLE(jaspResults, variable, options, ready, errors, .ldFillNegbinomialZeroInflatedEstimatesTable)

  return()
}

### options ----
.ldRecodeOptionsNegbinomialZeroInflated <- function(options){
  if(options$parametrization == "prob"){
    options$mu <- options$size*options$par / (1-options$par)
  } else {
    options$mu <- options$par
  }

  options[['parValNames']] <- c("pi", "size", "par")

  options[['pars']]   <- list(pi = options[['pi']], size = options[['size']], mu = options[['mu']])

  options[['pdfFun']] <- dzinbinom
  options[['cdfFun']] <- pzinbinom
  options[['qFun']]   <- qzinbinom
  options[['rFun']]   <- rzinbinom
  options[['distNameInR']] <- "zinbinom"

  options <- .ldOptionsDeterminePlotLimits(options, FALSE)

  options$support <- list(min = 0, max = Inf)
  options$lowerBound <- c(0,   0,   0)
  options$upperBound <- c(1, Inf, Inf)

  options$transformations <- c(pi = "pi", size = "size", prob = "size / (size + mu)", mu = "mu")

  options
}

### text fill functions -----
.ldNegbinomialZeroInflatedParsSupportMoments <- function(jaspResults, options){
  if(options$parsSupportMoments && is.null(jaspResults[['parsSupportMoments']])){
    pars <- list()
    pars[[1]] <- gettextf("probability of zero process: %s", "\u03C0 \u2208 [0, 1]")
    pars[[2]] <- switch(options[['parametrization']],
                        prob = gettextf("number of successes: %s", "k \u2208 \u211D: \u03D5 \u2265 0"),
                               gettextf("dispersion: %s",          "\u03D5 \u2208 \u211D: \u03D5 \u2265 0"))
    pars[[3]] <- switch(options[['parametrization']],
                        prob = gettextf("probability of success: %s", "p \u2208 \u211D: 0 \u2264 p \u2264 1"),
                               gettextf("mean: %s",                   "\u03BC \u2208 \u211D: \u03BC \u2265 0"))

    support <- "x \u2208 {0, 1, 2, ...}"

    moments <- list()
    moments$expectation <- switch(options[['parametrization']],
                                  prob = "(1-\u03C0)pk/(1-p)",
                                         "(1-\u03C0)\u03BC")
    moments$variance <- switch(options[['parametrization']],
                               prob = "(1-\u03C0)[\u03BC + \u03BC<sup>2</sup>/(\u03C0 + \u03D5)]",
                                      "(1-\u03C0)[\u03BC + \u03BC<sup>2</sup>/(\u03C0 + \u03D5)]")

    jaspResults[['parsSupportMoments']] <- .ldParsSupportMoments(pars, support, moments)
  }
}

.ldFormulaNegbinomialZeroInflatedPMF <- function(options){
}

.ldFormulaNegbinomialZeroInflatedCDF <- function(options){
}

.ldFormulaNegbinomialZeroInflatedQF <- function(options){
}

#### Table functions ----

.ldFillNegbinomialZeroInflatedEstimatesTable <- function(table, results, options, ready){
  if(!ready) return()
  if(is.null(results)) return()
  if(is.null(table)) return()

  res <- results$structured
  if(options$parametrization == "prob"){
    res$parName <- c("\u03C0", "k", "p", "\u03BC")
    res <- res[res$par != "mu",,drop=FALSE]
  } else{
    res$parName <- c("\u03C0", "\u03D5", "p", "\u03BC")
    res <- res[res$par != "prob",,drop=FALSE]
  }

  if(results$fitdist$convergence != 0){
    table$addFootnote(gettext("The optimization did not converge, try adjusting the parameter values."), symbol = gettext("<i>Warning.</i>"))
  }
  if(!is.null(results$fitdist$optim.message)){
    table$addFootnote(results$fitdist$message, symbol = gettext("<i>Warning.</i>"))
  }

  table$setData(res)

  return()
}

#### distribution functions ----
dzinbinom <- function(x, pi, size, mu, log = FALSE) {
  out <- (1-pi) * dnbinom(x, size = size, mu = mu, log = FALSE)
  out[x == 0] <- out[x == 0] + pi

  if(log) out <- log(out)

  return(out)
}

pzinbinom <- function(q, pi, size, mu, lower.tail = TRUE, log.p = FALSE) {
  out <- pi + (1-pi) * pnbinom(q, size = size, mu = mu, lower.tail = TRUE, log.p = FALSE)

  if(!lower.tail) out <- 1-out
  if(log.p) out <- log(out)

  return(out)
}

qzinbinom <- function(p, pi, size, mu, lower.tail = TRUE, log.p = FALSE) {
  if(log.p) p <- exp(p)
  if(!lower.tail) p <- 1-p

  .q <- function(p, pi, size, mu) {
    q <- 0
    cdf <- 0
    while(cdf < p) {
      cdf <- cdf + dzinbinom(q, pi=pi, size=size, mu=mu, log = FALSE)
      q <- q + 1
    }
    return(q)
  }

  out <- sapply(p, .q, pi=pi, size=size, mu=mu)

  return(out)
}

rzinbinom <- function(n, pi, size, mu) {
  zeros <- rbinom(n, 1, pi)
  out <- ifelse(zeros, 0, rnbinom(n, size=size, mu=mu))

  return(out)
}
