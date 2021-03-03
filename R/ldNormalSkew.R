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

LDnormalSkew <- function(jaspResults, dataset, options, state=NULL){
  options <- .recodeOptionsLDNormalSkew(options)

  #### Show distribution section ----
  .ldShowDistribution(jaspResults = jaspResults, options = options, name = gettext("skew normal distribution"),
                      parSupportMoments = .ldNormalSkewParsSupportMoments,
                      formulaPDF        = .ldFormulaNormalSkewPDF,
                      formulaCDF        = .ldFormulaNormalSkewCDF,
                      formulaQF         = .ldFormulaNormalSkewQF)

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
  .ldMLE(jaspResults, variable, options, ready, errors, .ldFillNormalSkewEstimatesTable)

  return()
}

### options ----
.recodeOptionsLDNormalSkew <- function(options){
  options[['parValNames']] <- c("xi", "omega", "alpha")

  options[['pars']]   <- list(xi = options[['xi']], omega = options[['omega']], alpha = options[['alpha']])
  options[['pdfFun']] <- sn::dsn
  options[['cdfFun']] <- function(q, xi, omega, alpha) sn::psn(q, xi, omega, alpha)
  options[['qFun']]   <- sn::qsn
  options[['rFun']]   <- sn::rsn
  options[['distNameInR']] <- "sn"

  options <- .ldOptionsDeterminePlotLimits(options)

  options$support <- list(min = -Inf, max = Inf)
  options$lowerBound <- c(-Inf,   0, -Inf)
  options$upperBound <- c( Inf, Inf,  Inf)

  options$transformations <- c(xi = "xi", omega = "omega", alpha = "alpha")

  options
}

### text fill functions -----
.ldNormalSkewParsSupportMoments <- function(jaspResults, options){
  if(options$parsSupportMoments && is.null(jaspResults[['parsSupportMoments']])){
    pars <- list()
    pars[[1]] <- gettextf("location: %s","&xi; \u2208 \u211D")
    pars[[2]] <- gettextf("scale: %s", "&omega; \u2208 \u211D<sup>+</sup>")
    pars[[3]] <- gettextf("shape: %s", "&alpha; \u2208 \u211D")

    support <- "x \u2208 \u211D"

    moments <- list()
    moments$expectation <- "&xi; + &omega;&delta;(2/&pi;)<sup>1/2</sup>"
    moments$variance <- "&omega;<sup>2</sup> (1-2&delta;<sup>2</sup>/&pi;) \n where &delta; = &alpha;/(1+&alpha;<sup>2</sup>)<sup>1/2</sup>"

    jaspResults[['parsSupportMoments']] <- .ldParsSupportMoments(pars, support, moments)
  }
}

.ldFormulaNormalSkewPDF <- function(options){

}

.ldFormulaNormalSkewCDF <- function(options){

}

.ldFormulaNormalSkewQF <- function(options){

}

#### Table functions ----

.ldFillNormalSkewEstimatesTable <- function(table, results, options, ready){
  if(!ready) return()
  if(is.null(results)) return()
  if(is.null(table)) return()

  pars <- c(xi = "\u03BE", omega = "\u03C9", alpha = "\u03B1")
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
