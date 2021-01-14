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

LDpoissonZeroInflated <- function(jaspResults, dataset, options, state=NULL){
  options <- .ldRecodeOptionsPoissonZeroInflated(options)

  #### Show poissonZeroInflated section ----
  .ldShowDistribution(jaspResults = jaspResults, options = options, name = gettext("zero-inflated Poisson distribution"),
                      parSupportMoments = .ldPoissonZeroInflatedParsSupportMoments,
                      formulaPMF        = .ldFormulaPoissonZeroInflatedPMF,
                      formulaCMF        = .ldFormulaPoissonZeroInflatedCDF)

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
  .ldMLE(jaspResults, variable, options, ready, errors, .ldFillPoissonZeroInflatedEstimatesTable)

  return()
}

### options ----
.ldRecodeOptionsPoissonZeroInflated <- function(options){

  options[['parValNames']] <- c("prob", "lambda")

  options[['pars']]   <- list(prob = options[['prob']], lambda = options[['lambda']])

  options[['pdfFun']] <- dzipois
  options[['cdfFun']] <- pzipois
  options[['qFun']]   <- qzipois
  options[['rFun']]   <- rzipois
  options[['distNameInR']] <- "zipois"

  options <- .ldOptionsDeterminePlotLimits(options, FALSE)

  options$support <- list(min = 0, max = Inf)
  options$lowerBound <- c(0,   0)
  options$upperBound <- c(1, Inf)

  options$transformations <- c(prob = "prob", lambda = "lambda")

  options
}

### text fill functions -----
.ldPoissonZeroInflatedParsSupportMoments <- function(jaspResults, options){
  if(options$parsSupportMoments && is.null(jaspResults[['parsSupportMoments']])){
    pars <- list()
    pars[[1]] <- gettextf("probability of zero process: %s", "\u03C0 \u2208 [0, 1]")
    pars[[2]] <- gettextf("rate: %s", "\u03BB \u2208 \u211D: \u03BB \u003E 0")

    support <- "x \u2208 {0, 1, 2, ...}"

    moments <- list()
    moments$expectation <- "(1-\u03C0)\u03BB"
    moments$variance <- "\u03BB(1-\u03C0)(1+\u03C0\u03BB)"

    jaspResults[['parsSupportMoments']] <- .ldParsSupportMoments(pars, support, moments)
  }
}

.ldFormulaPoissonZeroInflatedPMF <- function(options){
    text <- "<MATH>
    f(x; <span style='color:red'>\u03BB</span>) =
    </MATH>"

  return(gsub(pattern = "\n", replacement = " ", x = text))
}

.ldFormulaPoissonZeroInflatedCDF <- function(options){
  text <- "<MATH>
    F(x; <span style='color:red'>\u03BB</span>) =
    </MATH>"

  return(gsub(pattern = "\n", replacement = " ", x = text))
}

.ldFormulaPoissonZeroInflatedQF <- function(options){
  text <- "<MATH>
    Q(x; <span style='color:red'>\u03BB</span>) =
    </MATH>"

  return(gsub(pattern = "\n", replacement = " ", x = text))
}

#### Table functions ----

.ldFillPoissonZeroInflatedEstimatesTable <- function(table, results, options, ready){
  if(!ready) return()
  if(is.null(results)) return()
  if(is.null(table)) return()

  res <- results$structured
  res$parName <- c("\u03C0", "\u03BB")

  if(results$fitdist$convergence != 0){
    table$addFootnote(gettext("The optimization did not converge, try adjusting the parameter values."), symbol = gettext("<i>Warning.</i>"))
  }
  if(!is.null(results$fitdist$optim.message)){
    table$addFootnote(results$fitdist$message, symbol = gettext("<i>Warning.</i>"))
  }

  table$setData(res)

  return()
}


dzipois <- function(x, prob, lambda, log = FALSE) {
  out <- (1-prob) * dpois(x, lambda, log = FALSE)
  out[x == 0] <- out[x == 0] + prob

  if(log) out <- log(out)

  return(out)
}

pzipois <- function(q, prob, lambda, lower.tail = TRUE, log.p = FALSE) {
  out <- prob + (1-prob) * ppois(q, lambda, lower.tail = TRUE, log.p = FALSE)

  if(!lower.tail) out <- 1-out
  if(log.p) out <- log(out)

  return(out)
}

qzipois <- function(p, prob, lambda, lower.tail = TRUE, log.p = FALSE) {
  if(log.p) p <- exp(p)
  if(!lower.tail) p <- 1-p

  .q <- function(p, prob, lambda) {
    q <- 0
    cdf <- 0
    while(cdf < p) {
      cdf <- cdf + pzipois(q, prob, lambda)
      q <- q + 1
    }
    return(q)
  }

  out <- sapply(p, .q, prob=prob, lambda=lambda)

  return(out)
}

rzipois <- function(n, prob, lambda) {
  zeros <- rbinom(n, 1, prob)
  out <- ifelse(zeros, 0, rpois(n, lambda))

  return(out)
}
