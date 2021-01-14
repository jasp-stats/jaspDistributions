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

LDbetaBinomial <- function(jaspResults, dataset, options, state=NULL){
  options <- .ldRecodeOptionsbetaBinomial(options)

  #### Show binomial section ----
  .ldShowDistribution(jaspResults = jaspResults, options = options, name = gettext("beta-binomial distribution"),
                      parSupportMoments = .ldbetaBinomialParsSupportMoments,
                      formulaPMF        = .ldFormulabetaBinomialPMF,
                      formulaCMF        = .ldFormulabetaBinomialCDF)

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
                         observations.amount = "<1",
                         limits.min = options$support$min, limits.max = options$support$max,
                         exitAnalysisIfErrors = FALSE)
    errors <- .ldCheckInteger(variable, errors)
  }

  # overview of the data
  .ldDescriptives(jaspResults, variable, options, ready, errors, "discrete")

  #### Fit data and assess fit ----
  .ldMLE(jaspResults, variable, options, ready, errors, .ldFillbetaBinomialEstimatesTable)

  return()
}

### options ----
.ldRecodeOptionsbetaBinomial <- function(options){

  options[['parValNames']] <- c("alpha", "beta", "size")

  options[['pars']]   <- list(alpha = options[['alpha']], beta = options[['beta']], size = options[['size']])
  options[['fix.pars']] <- list(size = options[['size']])

  options[['pdfFun']] <- dbetabinom
  options[['cdfFun']] <- pbetabinom
  options[['qFun']]   <- qbetabinom
  options[['rFun']]   <- rbetabinom
  options[['distNameInR']] <- "betabinom"

  options <- .ldOptionsDeterminePlotLimits(options, FALSE)

  options$support <- list(min = 0, max = options[['size']])
  options$lowerBound <- c(0, 0)
  options$upperBound <- c(Inf, Inf)

  options$transformations <- c(alpha = "alpha", beta = "beta")

  options
}

### text fill functions -----
.ldbetaBinomialParsSupportMoments <- function(jaspResults, options){
  if(options$parsSupportMoments && is.null(jaspResults[['parsSupportMoments']])){
    pars <- list()
    pars[[1]] <- gettextf("shape: %s", "&alpha; \u2208 \u211D<sup>+</sup>")
    pars[[2]] <- gettextf("shape: %s", "&beta; \u2208 \u211D<sup>+</sup>")
    pars[[3]] <- gettextf("number of trials: %s",       "n \u2208 \u2124: n \u2265 0")

    support <- "x \u2208 {0, 1, ..., n}"

    moments <- list()
    moments$expectation <- "n&alpha;/(&alpha; + &beta;)"
    moments$variance <- "n&alpha;&beta;(&alpha; + &beta;+n)/[(&alpha; + &beta;)<sup>2</sup>(&alpha; + &beta;+1)]"

    jaspResults[['parsSupportMoments']] <- .ldParsSupportMoments(pars, support, moments)
  }
}

.ldFormulabetaBinomialPMF <- function(options){
}

.ldFormulabetaBinomialCDF <- function(options){
}

.ldFormulabetaBinomialQF <- function(options){
}

#### Table functions ----

.ldFillbetaBinomialEstimatesTable <- function(table, results, options, ready){
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

  table$addFootnote(message = gettextf("Parameter n was fixed at value %s.", options[['size']]))
  table$setData(res)

  return()
}

#### distribution functions ----
dbetabinom <- function(x, size, alpha, beta, log = FALSE) {
  out <- lchoose(size, x) + lbeta(x + alpha, size - x + beta) - lbeta(alpha, beta)

  if(!log) out <- exp(out)

  return(out)
}

pbetabinom <- function(q, size, alpha, beta, lower.tail = TRUE, log.p = FALSE) {
  # define non vectorized calculation
  .p <- function(q, size, alpha, beta) {
    if(q < 0) {
      0
    } else if (q >= size) {
      1
    } else {
      sum(dbetabinom(0:q, size, alpha, beta, log = FALSE))
    }

    #else {
      # U <- c(1, -q, size-q+beta)
      # L <- c(size-q-1, 1-q-alpha)
      # lchoose(size, q) + lbeta(q + alpha, size - q + beta) - lbeta(alpha, beta) +
      #   log( hypergeo::genhypergeo(U = U, L = L, z = 1) )
    #}
  }

  out <- sapply(q, .p, size=size, alpha=alpha, beta=beta)

  if(!lower.tail) out <- 1-out
  if(log.p) out <- log(out)

  return(out)
}

qbetabinom <- function(p, size, alpha, beta, lower.tail = TRUE, log.p = FALSE) {
  if(log.p) p <- exp(p)
  if(!lower.tail) p <- 1-p

  .q <- function(p, size, alpha, beta) {
    q <- 0
    cdf <- 0
    while(cdf < p) {
      cdf <- cdf + dbetabinom(q, size, alpha, beta, log = FALSE)
      q   <- q + 1
    }
    return(q)
  }

  out <- sapply(p, .q, size=size, alpha=alpha, beta=beta)

  return(out)
}

rbetabinom <- function(n, size, alpha, beta) {
  p <- rbeta(n, alpha, beta)
  out <- rbinom(n, size, p)

  return(out)
}
