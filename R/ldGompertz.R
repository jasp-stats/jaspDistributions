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

LDgompertzInternal <- function(jaspResults, dataset, options, state=NULL){
  options <- .ldRecodeOptionsGompertz(options)

  #### Show gompertz section ----
  .ldShowDistribution(jaspResults = jaspResults, options = options, name = gettext("Gompertz distribution"),
                      parSupportMoments = .ldGompertzParsSupportMoments,
                      formulaPDF        = .ldFormulaGompertzPDF,
                      formulaCDF        = .ldFormulaGompertzCDF,
                      formulaQF         = .ldFormulaGompertzQF)

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
  .ldMLE(jaspResults, variable, options, ready, errors, .ldFillGompertzEstimatesTable)

  return()
}

### options ----
.ldRecodeOptionsGompertz <- function(options){
  options[['parValNames']] <- c("shape", "scale")

  options[['pars']]   <- list(shape = options[["shape"]], scale = options[["scale"]])
  options[['pdfFun']] <- dgompertz
  options[['cdfFun']] <- pgompertz
  options[['qFun']]   <- qgompertz
  options[['rFun']]   <- rgompertz
  options[['distNameInR']] <- "gompertz"

  options <- .ldOptionsDeterminePlotLimits(options)

  options$support <- list(min = 0, max = Inf)
  options$lowerBound <- c(0, 0)
  options$upperBound <- c(Inf, Inf)

  options$transformations <- c(shape = "shape", scale = "scale")

  options
}

### text fill functions -----
.ldGompertzParsSupportMoments <- function(jaspResults, options){
  if(options$parsSupportMoments && is.null(jaspResults[['parsSupportMoments']])){
    pars <- list()
    pars[[1]] <- gettextf("shape: %1$s \nscale: %2$s", "\u03B7 \u2208 \u211D<sup>+</sup>", "b \u2208 \u211D<sup>+</sup>")

    support <- "x \u2208 \u211D<sup>+</sup>"

    moments <- list()
    moments$expectation <- gettextf("see Wikipedia: %s", "https://en.wikipedia.org/wiki/Gompertz_distribution")
    moments$variance <- gettextf("see Wikipedia: %s", "https://en.wikipedia.org/wiki/Gompertz_distribution")

    jaspResults[['parsSupportMoments']] <- .ldParsSupportMoments(pars, support, moments)
  }
}

.ldFormulaGompertzPDF <- function(options){
}

.ldFormulaGompertzCDF <- function(options){
}

.ldFormulaGompertzQF <- function(options){
}

#### Table functions ----

.ldFillGompertzEstimatesTable <- function(table, results, options, ready){
  if(!ready) return()
  if(is.null(results)) return()
  if(is.null(table)) return()

  par <- c(shape = "\u03B7", scale = "b")
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

dgompertz <- function(x, shape, scale, log = FALSE) {
  out <- log(scale) + log(shape) + (shape + scale*x - shape * exp(scale*x))

  if(!log) out <- exp(out)

  return(out)
}

pgompertz <- function(q, shape, scale, lower.tail = TRUE, log.p = FALSE) {
  out <- exp( -shape*(exp(scale*q) - 1) )

  if(lower.tail) out <- 1-out
  if(log.p) out <- log(out)

  return(out)
}

qgompertz <- function(p, shape, scale, lower.tail = TRUE, log.p = FALSE) {
  if(log.p) p <- exp(p)
  if(!lower.tail) p <- 1-p

  out <- 1/scale * log(1 - 1/shape * log1p(-p))

  return(out)
}

rgompertz <- function(n, shape, scale) {
  p <- runif(n)
  q <- qgompertz(p, shape, scale)

  return(q)
}
