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

LDtriangularInternal <- function(jaspResults, dataset, options, state=NULL){
  options <- .recodeOptionsLDTriangular(options)

  #### Show distribution section ----
  .ldShowDistribution(jaspResults = jaspResults, options = options, name = gettext("triangular distribution"),
                      parSupportMoments = .ldTriangularParsSupportMoments,
                      formulaPDF        = .ldFormulaTriangularPDF,
                      formulaCDF        = .ldFormulaTriangularCDF,
                      formulaQF         = .ldFormulaTriangularQF)

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
  if(ready && isFALSE(errors)) {
    options$lowerBound <- c(-Inf,          max(variable), min(variable))
    options$upperBound <- c(min(variable), Inf,           max(variable))
  }
  .ldMLE(jaspResults, variable, options, ready, errors, .ldFillTriangularEstimatesTable)

  return()
}

.recodeOptionsLDTriangular <- function(options){
  options[['parValNames']] <- c("a", "b", "c")

  options[['pars']]   <- list(a  = options[['a']], b = options[['b']], c = options[['c']])
  options[['pdfFun']] <- dtriangular
  options[['cdfFun']] <- ptriangular
  options[['qFun']]   <- qtriangular
  options[['rFun']]   <- rtriangular
  options[['distNameInR']] <- "triangular"

  options <- .ldOptionsDeterminePlotLimits(options)

  options$support <- list(min = -Inf, max = Inf)
  options$lowerBound <- c(-Inf, -Inf, -Inf)
  options$upperBound <- c( Inf,  Inf,  Inf)

  options$transformations <- c(a = "a", b = "b", c = "c")

  options
}

### text fill functions -----
.ldTriangularParsSupportMoments <- function(jaspResults, options){
  if(options$parsSupportMoments && is.null(jaspResults[['parsSupportMoments']])){
    pars <- list()
    pars[[1]] <- gettextf("minimum: %s","a \u2208 \u211D")
    pars[[2]] <- gettextf("maximum: %s","b \u2208 \u211D")
    pars[[3]] <- gettextf("mode: %s","c \u2208 \u211D; a < c < b")

    support <- "x \u2208 \u211D; a < x < b"

    moments <- list()
    moments$expectation <- "(a+b+c)/3"
    moments$variance <- "(a<sup>2</sup> + b<sup>2</sup> + c<sup>2</sup> - ab - ac - bc)/18"

    jaspResults[['parsSupportMoments']] <- .ldParsSupportMoments(pars, support, moments)
  }
}

.ldFormulaTriangularPDF <- function(options){
}

.ldFormulaTriangularCDF <- function(options){
}

.ldFormulaTriangularQF <- function(options){
}

#### Table functions ----

.ldFillTriangularEstimatesTable <- function(table, results, options, ready){
  if(!ready) return()
  if(is.null(results)) return()
  if(is.null(table)) return()

  par <- c(a = "a", b = "b", c = "c")
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

#### distribution functions ----

dtriangular <- function(x, a, b, c, log = FALSE) {
  out <- sapply(x, function(xx) {
    if(xx < a || xx > b) {
      return(0)
    } else if(xx == a || xx == b) {
      return(.Machine$double.xmin)
    } else if(xx < c) {
      return(2*(xx-a) / ((b-a)*(c-a)))
    } else if(xx == c) {
      return(2/(b-a))
    } else {
      return(2*(b-xx) / ((b-a)*(b-c)))
    }
  })

  if(log) out <- log(out)

  return(out)
}

ptriangular <- function(q, a, b, c, lower.tail = TRUE, log.p = FALSE) {
  out <- sapply(q, function(qq) {
    if(qq < a) {
      return(0)
    } else if(qq <= c) {
      return((qq-a)^2 / ((b-a)*(c-a)))
    } else if(qq <= b) {
      return(1 - (b-qq)^2/ ((b-a)*(b-c)))
    } else {
      return(1)
    }
  })

  if(!lower.tail) out <- 1 - out
  if(log.p) out <- log(out)

  return(out)
}

qtriangular <- function(p, a, b, c, lower.tail = TRUE, log.p = FALSE) {
  if(log.p) p <- exp(p)
  if(!lower.tail) p <- 1-p

  n <- length(p)
  q <- sapply(seq_len(n), function(i) {.getQuantileTriangular(p[i], a, b, c) })

  return(q)
}

.getQuantileTriangular <- function(p, a, b, c) {
  o <- try(optim(par = c, fn = .pErrorTriangular, lower = a, upper = b, method = "L-BFGS-B",
                 p = p, pars = list(a = a, b = b, c = c)), silent = TRUE)

  if(inherits(o, "try-error")) {
    return(NA)
  } else {
    return(o[["par"]])
  }
}

.pErrorTriangular <- function(q, p, pars) {
  args <- c(q=q, pars)
  pp <- do.call(ptriangular, args)

  return((pp-p)^2)
}

rtriangular <- function(n, a, b, c) {
  p <- runif(n, 0, 1)
  q <- qtriangular(p, a, b, c)

  return(q)
}
