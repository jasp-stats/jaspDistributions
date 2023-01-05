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

LDgumbelInternal <- function(jaspResults, dataset, options, state=NULL){
  options <- .ldRecodeOptionsGumbel(options)

  #### Show distribution section ----
  .ldShowDistribution(jaspResults = jaspResults, options = options, name = gettext("Gumbel distribution"),
                      parSupportMoments = .ldGumbelParsSupportMoments,
                      formulaPDF        = .ldFormulaGumbelPDF,
                      formulaCDF        = .ldFormulaGumbelCDF,
                      formulaQF         = .ldFormulaGumbelQF)

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
  .ldMLE(jaspResults, variable, options, ready, errors, .ldFillGumbelEstimatesTable)

  return()
}

.ldRecodeOptionsGumbel <- function(options){
  options[['parValNames']] <- c("mu", "beta")

  options[['pars']]   <- list(mu = options[['mu']], beta = options[['beta']])
  options[['pdfFun']] <- dgumbel
  options[['cdfFun']] <- pgumbel
  options[['qFun']]   <- qgumbel
  options[['rFun']]   <- rgumbel
  options[['distNameInR']] <- "gumbel"

  options <- .ldOptionsDeterminePlotLimits(options)

  options$support <- list(min = -Inf, max = Inf)
  options$lowerBound <- c(-Inf, 0)
  options$upperBound <- c(Inf, Inf)

  options$transformations <- c(mu = "mu", beta = "beta")

  options
}

### text fill functions -----
.ldGumbelParsSupportMoments <- function(jaspResults, options){
  if(options$parsSupportMoments && is.null(jaspResults[['parsSupportMoments']])){
    pars <- list()
    pars[[1]] <- gettextf("location: &mu; %s","\u2208 \u211D")
    pars[[2]] <- gettextf("scale: %s", "&beta; \u2208 \u211D<sup>+</sup>")

    support <- "x \u2208 \u211D"

    moments <- list()
    moments$expectation <- "&mu; + &beta;&gamma;"
    moments$variance <- "&beta;<sup>2</sup> &pi;<sup>2</sup>/6"

    jaspResults[['parsSupportMoments']] <- .ldParsSupportMoments(pars, support, moments)
  }
}

.ldFormulaGumbelPDF <- function(options){
}

.ldFormulaGumbelCDF <- function(options){
}

.ldFormulaGumbelQF <- function(options){
}

#### Table functions ----

.ldFillGumbelEstimatesTable <- function(table, results, options, ready){
  if(!ready) return()
  if(is.null(results)) return()
  if(is.null(table)) return()

  par <- c(location = "\u03BC", scale = "\u03B2")
  res <- results$structured
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

dgumbel <- function(x, mu, beta, log = FALSE) {
  z <- (x - mu)/beta

  out <- -log(beta) - (z + exp(-z))

  if(!log) out <- exp(out)

  return(out)
}

pgumbel <- function(q, mu, beta, lower.tail = TRUE, log.p = FALSE) {
  z <- (q - mu)/beta
  out <- exp(-exp(-z))

  if(!lower.tail) out <- 1-out
  if(log.p) out <- log(out)

  return(out)
}

qgumbel <- function(p, mu, beta, lower.tail = TRUE, log.p = FALSE) {
  if(log.p) p <- exp(p)
  if(!lower.tail) p <- 1-p

  out <- mu - beta * log(-log(p))

  return(out)
}

rgumbel <- function(n, mu, beta) {
  p <- runif(n)
  q <- qgumbel(p, mu, beta)

  return(q)
}
