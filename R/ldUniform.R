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

LDuniformInternal <- function(jaspResults, dataset, options, state=NULL){
  options <- .ldRecodeOptionsUniform(options)

  #### Show beta section ----
  .ldShowDistribution(jaspResults = jaspResults, options = options, name = gettext("uniform distribution"),
                      parSupportMoments = .ldUniformParsSupportMoments,
                      formulaPDF        = .ldFormulaUniformPDF,
                      formulaCDF        = .ldFormulaUniformCDF,
                      formulaQF         = .ldFormulaUniformQF)

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
  analyticEstimates <- .ldMLEUniform(variable, options, ready, errors)
  .ldMLE(jaspResults, variable, options, ready, errors, .ldFillUniformEstimatesTable, analyticEstimates)

  return()
}

### options ----
.ldRecodeOptionsUniform <- function(options){
  options[['parValNames']] <- c("lowerBoundPar", "upperBoundPar")

  options[['pars']]   <- list(min = options[['lowerBoundPar']], max = options[['upperBoundPar']])
  options[['pdfFun']] <- dunif
  options[['cdfFun']] <- punif
  options[['qFun']]   <- qunif
  options[['rFun']]   <- runif
  options[['distNameInR']] <- "unif"

  options <- .ldOptionsDeterminePlotLimits(options)

  options$support <- list(min = -Inf, max = Inf)
  options$lowerBound <- c(0, 0)
  options$upperBound <- c(Inf, Inf)

  options$transformations <- c(min = "min", max = "max")

  options
}

### text fill functions -----

.ldUniformParsSupportMoments <- function(jaspResults, options){
  if(options$parsSupportMoments && is.null(jaspResults[['parsSupportMoments']])){
    pars <- list()
    pars[[1]] <- gettextf("lower bound: %s", "a \u2208 \u211D<sup>+</sup>")
    pars[[2]] <- gettextf("upper bound: %s", "b \u2208 \u211D<sup>+</sup>")

    support <- "x \u2208 [a, b]"

    moments <- list()
    moments$expectation <- "(a+b)/2"
    moments$variance    <- "(b-a)<sup>2</sup>/12"

    jaspResults[['parsSupportMoments']] <- .ldParsSupportMoments(pars, support, moments)
  }
}

.ldFormulaUniformPDF <- function(options){
}

.ldFormulaUniformCDF <- function(options){
}

.ldFormulaUniformQF <- function(options){
}

#### Table functions ----

.ldFillUniformEstimatesTable <- function(table, results, options, ready){
  if(!ready) return()
  if(is.null(results)) return()
  if(is.null(table)) return()

  pars <- c(min = "a", max = "b")

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

#### MLE estimation ----

.ldMLEUniform <- function(variable, options, ready, errors) {
  if(!ready || !isFALSE(errors)) return()
  results <- list()

  mm <- range(variable, na.rm = TRUE)
  range <- diff(mm)
  n <- length(variable)
  alpha <- 1-options[["ciIntervalInterval"]]
  offSet <- alpha^(-1/(n-1)) - 1
  ci <- mm + c(-1, 1) * offSet * range
  results$structured <- data.frame(par      = c("min", "max"),
                                   estimate = mm,
                                   se       = c(NA, NA),
                                   lower    = c(ci[1], mm[2]),
                                   upper    = c(mm[1], ci[2])
                                   )

  results$fitdist <- list()
  results$fitdist$convergence <- 0
  results$fitdist$estimate <- setNames(results$structured$estimate, results$structured$par)
  results$ci.possible <- TRUE
  results$se.possible <- FALSE
  results
}
