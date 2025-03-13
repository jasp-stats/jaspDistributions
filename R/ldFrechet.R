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

LDfrechetInternal <- function(jaspResults, dataset, options, state=NULL){
  options <- .recodeOptionsLDFrechet(options)

  #### Show distribution section ----
  .ldShowDistribution(jaspResults = jaspResults, options = options, name = gettext("Fréchet distribution"),
                      parSupportMoments = .ldFrechetParsSupportMoments,
                      formulaPDF        = .ldFormulaFrechetPDF,
                      formulaCDF        = .ldFormulaFrechetCDF,
                      formulaQF         = .ldFormulaFrechetQF)

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
  .ldMLE(jaspResults, variable, options, ready, errors, .ldFillFrechetEstimatesTable)

  return()
}

.recodeOptionsLDFrechet <- function(options){
  options[['parValNames']] <- c("shape", "location", "scale")

  options[['pars']]   <- list(shape = options[['shape']], location = options[['location']], scale = options[['scale']])
  options[['pdfFun']] <- dfrechet
  options[['cdfFun']] <- pfrechet
  options[['qFun']]   <- qfrechet
  options[['rFun']]   <- rfrechet
  options[['distNameInR']] <- "frechet"

  options <- .ldOptionsDeterminePlotLimits(options)

  options$support <- list(min = -Inf, max = Inf)
  options$lowerBound <- c(  0, -Inf,   0)
  options$upperBound <- c(Inf,  Inf, Inf)

  options$transformations <- c(shape = "shape", location = "location", scale = "scale")

  options
}

### text fill functions -----
.ldFrechetParsSupportMoments <- function(jaspResults, options){
  if(options$parsSupportMoments && is.null(jaspResults[['parsSupportMoments']])){
    pars <- list()
    pars[[1]] <- gettextf("shape: %s",      "&alpha; \u2208 \u211D<sup>+</sup>")
    pars[[2]] <- gettextf("location: %s", "&mu; \u2208 \u211D")
    pars[[3]] <- gettextf("scale: %s",    "&sigma; \u2208 \u211D<sup>+</sup>")

    support <- "x \u2208 \u211D; x > &mu;"

    moments <- list()
    moments$expectation <- gettextf("see Wikipedia: https://en.wikipedia.org/wiki/Fr%%C3%%A9chet_distribution")
    moments$variance <- gettextf("see Wikipedia: https://en.wikipedia.org/wiki/Fr%%C3%%A9chet_distribution")

    jaspResults[['parsSupportMoments']] <- .ldParsSupportMoments(pars, support, moments)
  }
}

.ldFormulaFrechetPDF <- function(options){
}

.ldFormulaFrechetCDF <- function(options){
}

.ldFormulaFrechetQF <- function(options){
}

#### Table functions ----

.ldFillFrechetEstimatesTable <- function(table, results, options, ready){
  if(!ready) return()
  if(is.null(results)) return()
  if(is.null(table)) return()

  par <- c(shape = "\u03B1", location = "\u03BC", scale = "\u03C3")
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
dfrechet <- function(x, shape, location, scale, log = FALSE) {
  xi <- (x-location)/scale
  out <- ifelse(xi <= 0, -Inf, log(shape) - log(scale) -(1+shape)*log(xi) - xi^(-shape))

  if(!log) out <- exp(out)

  return(out)
}

pfrechet <- function(q, shape, location, scale, lower.tail = TRUE, log.p = FALSE) {
  xi <- (q-location)/scale
  out <- ifelse(xi <= 0, 0, exp(-xi^(-shape)))

  if(!lower.tail) out <- 1-out
  if(log.p)       out <- log(out)

  return(out)
}

qfrechet <- function(p, shape, location, scale, lower.tail = TRUE, log.p = FALSE) {
  if(log.p) p <- exp(p)
  if(!lower.tail) p <- 1-p

  out <- location + scale * (-log(p))^(-1/shape)

  return(out)
}

rfrechet <- function(n, shape, location, scale) {
  p <- stats::runif(n)
  x <- qfrechet(p, shape = shape, location = location, scale = scale)

  return(x)
}
