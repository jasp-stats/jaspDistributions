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

LDparetoInternal <- function(jaspResults, dataset, options, state=NULL){
  options <- .ldRecodeOptionsPareto(options)

  #### Show pareto section ----
  .ldShowDistribution(jaspResults = jaspResults, options = options, name = gettext("Pareto distribution"),
                      parSupportMoments = .ldParetoParsSupportMoments,
                      formulaPDF        = .ldFormulaParetoPDF,
                      formulaCDF        = .ldFormulaParetoCDF,
                      formulaQF         = .ldFormulaParetoQF)

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
  analyticEstimates <- .ldMLEPareto(variable, options, ready, errors)
  .ldMLE(jaspResults, variable, options, ready, errors, .ldFillParetoEstimatesTable, analyticEstimates)

  return()
}

### options ----
.ldRecodeOptionsPareto <- function(options){
  options[['parValNames']] <- c("shape", "scale")

  options[['pars']]   <- list(shape = options[["shape"]], scale = options[["scale"]])
  options[['pdfFun']] <- dpareto
  options[['cdfFun']] <- ppareto
  options[['qFun']]   <- qpareto
  options[['rFun']]   <- rpareto
  options[['distNameInR']] <- "pareto"

  options <- .ldOptionsDeterminePlotLimits(options)

  options$support <- list(min = 0, max = Inf)
  options$lowerBound <- c(0, 0)
  options$upperBound <- c(Inf, Inf)

  options$transformations <- c(shape = "shape", scale = "scale")

  options
}

### text fill functions -----
.ldParetoParsSupportMoments <- function(jaspResults, options){
  if(options$parsSupportMoments && is.null(jaspResults[['parsSupportMoments']])){
    pars <- list()
    pars[[1]] <- gettextf("shape: %1$s \nscale: %2$s", "k \u2208 \u211D<sup>+</sup>", "&lambda; \u2208 \u211D<sup>+</sup>")

    support <- "x \u2208 \u211D<sup>+</sup>"

    moments <- list()
    moments$expectation <- "&lambda;"
    moments$variance <- "&lambda;<sup>2</sup>"

    jaspResults[['parsSupportMoments']] <- .ldParsSupportMoments(pars, support, moments)
  }
}

.ldFormulaParetoPDF <- function(options){
}

.ldFormulaParetoCDF <- function(options){
}

.ldFormulaParetoQF <- function(options){
}

#### Table functions ----

.ldFillParetoEstimatesTable <- function(table, results, options, ready){
  if(!ready) return()
  if(is.null(results)) return()
  if(is.null(table)) return()

  par <- c(shape = "\u03B1", scale = "\u03B2")
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

dpareto <- function(x, shape, scale, log = FALSE) {
  lfirst <- log(shape) + shape * log(scale)
  lsecond <- (shape + 1) * log(x)

  out <- ifelse(x < scale, -Inf, lfirst - lsecond)

  if(!log) out <- exp(out)

  return(out)
}

ppareto <- function(q, shape, scale, lower.tail = TRUE, log.p = FALSE) {
  out <- ifelse(q < scale, 0, 1 - (scale / q) ^ shape)

  if(!lower.tail) out <- 1-out
  if(log.p) out <- log(out)

  return(out)
}

qpareto <- function(p, shape, scale, lower.tail = TRUE, log.p = FALSE) {
  if(log.p) p <- exp(p)
  if(!lower.tail) p <- 1-p

  out <- scale / (1-p)^(1/shape)

  return(out)
}

rpareto <- function(n, shape, scale) {
  p <- runif(n = n)
  q <- qpareto(p, shape, scale
               )

  return(q)
}

#### MLE estimation ----

.ldMLEPareto <- function(variable, options, ready, errors) {
  if(!ready || !isFALSE(errors)) return()
  results <- list()

  pLowerCI <- (1-options[['ciIntervalInterval']]) / 2
  pUpperCI <- 1 - pLowerCI
  n <- length(variable)
  scaleHat <- min(variable)
  shapeHat <- n / sum(log(variable/scaleHat))

  results$structured <- data.frame(par      = c("shape", "scale"),
                                   estimate = c(shapeHat, scaleHat),
                                   se       = c(shapeHat / sqrt(n), NA),
                                   lower    = c(NA, NA),
                                   upper    = c(NA, NA))

  results$fitdist <- list()
  results$fitdist$convergence <- 0
  results$fitdist$estimate <- setNames(results$structured$estimate, results$structured$par)
  results$ci.possible <- FALSE
  results$se.possible <- FALSE
  results
}
