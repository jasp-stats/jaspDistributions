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

LDtSkew <- function(jaspResults, dataset, options, state=NULL){
  options <- .recodeOptionsLDTSkew(options)

  #### Show distribution section ----
  .ldShowDistribution(jaspResults = jaspResults, options = options, name = gettext("skew normal distribution"),
                      parSupportMoments = .ldTSkewParsSupportMoments,
                      formulaPDF        = .ldFormulaTSkewPDF,
                      formulaCDF        = .ldFormulaTSkewCDF,
                      formulaQF         = .ldFormulaTSkewQF)

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
  .ldMLE(jaspResults, variable, options, ready, errors, .ldFillTSkewEstimatesTable)

  return()
}

### options ----
.recodeOptionsLDTSkew <- function(options){
  options[['parValNames']] <- c("xi", "omega", "alpha", "df")

  options[['pars']]   <- list(xi = options[['xi']], omega = options[['omega']], alpha = options[['alpha']], nu = options[['df']])
  options[['pdfFun']] <- sn::dst
  options[['cdfFun']] <- function(q, xi, omega, alpha, nu) sn::pst(q, xi, omega, alpha, nu)
  options[['qFun']]   <- sn::qst
  options[['rFun']]   <- sn::rst
  options[['distNameInR']] <- "st"

  options <- .ldOptionsDeterminePlotLimits(options)

  options$support <- list(min = -Inf, max = Inf)
  options$lowerBound <- c(-Inf,   0, -Inf,   0)
  options$upperBound <- c( Inf, Inf,  Inf, Inf)

  options$transformations <- c(xi = "xi", omega = "omega", alpha = "alpha", df = "nu")

  options
}

### text fill functions -----
.ldTSkewParsSupportMoments <- function(jaspResults, options){
  if(options$parsSupportMoments && is.null(jaspResults[['parsSupportMoments']])){
    pars <- list()
    pars[[1]] <- gettextf("location: %s","&xi; \u2208 \u211D")
    pars[[2]] <- gettextf("scale: %s", "&omega; \u2208 \u211D<sup>+</sup>")
    pars[[3]] <- gettextf("shape: %s", "&alpha; \u2208 \u211D")
    pars[[4]] <- gettextf("scale: %s", "df \u2208 \u211D<sup>+</sup>")

    support <- "x \u2208 \u211D"

    moments <- list()
    moments$expectation <- gettext("see Azzalini & Capitanio (2003)")
    moments$variance <- gettext("see Azzalini & Capitanio (2003)")

    jaspResults[['parsSupportMoments']] <- .ldParsSupportMoments(pars, support, moments)
  }
}

.ldFormulaTSkewPDF <- function(options){

}

.ldFormulaTSkewCDF <- function(options){

}

.ldFormulaTSkewQF <- function(options){

}

#### Table functions ----

.ldFillTSkewEstimatesTable <- function(table, results, options, ready){
  if(!ready) return()
  if(is.null(results)) return()
  if(is.null(table)) return()

  pars <- c(xi = "\u03BE", omega = "\u03C9", alpha = "\u03B1", df = "df")
  res <- results$structured
  res <- res[res$par %in% names(pars),]
  res$parName <- pars

  if(results$fitdist$convergence != 0){
    table$addFootnote(gettext("The optimization did not converge, try adjusting the parameter values."), symbol = gettext("<i>Warning.</i>"))
  }
  if(!is.null(results$fitdist$optim.message)){
    table$addFootnote(results$fitdist$message, symbol = gettext("<i>Warning.</i>"))
  }

  table$setData(res)

  return()
}
