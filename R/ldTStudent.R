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

LDtStudentInternal <- function(jaspResults, dataset, options, state=NULL){
  options <- .ldRecodeOptionsTStudent(options)

  #### Show t section ----
  .ldShowDistribution(jaspResults = jaspResults, options = options, name = gettext("scaled, shifted Student's t-distribution"),
                      parSupportMoments = .ldTStudentParsSupportMoments,
                      formulaPDF        = .ldFormulaTStudentPDF,
                      formulaCDF        = .ldFormulaTStudentCDF,
                      formulaQF         = .ldFormulaTStudentQF)

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
  .ldMLE(jaspResults, variable, options, ready, errors, .ldFillTStudentEstimatesTable)


  return()
}

### options ----
.ldRecodeOptionsTStudent <- function(options){
  options[['parValNames']] <- c("df", "location", "scale")

  options[['pars']]   <- list(df = options[['df']], location = options[['location']], scale = options[['scale']])
  options[['pdfFun']] <- dtscaledshifted
  options[['cdfFun']] <- ptscaledshifted
  options[['qFun']]   <- qtscaledshifted
  options[['rFun']]   <- rtscaledshifted
  options[['distNameInR']] <- "tscaledshifted"

  options <- .ldOptionsDeterminePlotLimits(options)

  options$support <- list(min = -Inf, max = Inf)
  options$lowerBound <- c(0,  -Inf,   0)
  options$upperBound <- c(Inf, Inf, Inf)

  options$transformations <- c(df = "df", location = "location", scale = "scale")

  options
}

### text fill functions -----
.ldTStudentParsSupportMoments <- function(jaspResults, options){
  if(options$parsSupportMoments && is.null(jaspResults[['parsSupportMoments']])){
    pars <- list()
    pars[[1]] <- gettextf("degrees of freedom: %s", "df \u2208 \u211D<sup>+</sup>")
    pars[[2]] <- gettextf("location: %s",     "\u03BC \u2208 \u211D")
    pars[[3]] <- gettextf("scale: %s",     "\u03C3 \u2208 \u211D")

    support <- "x \u2208 \u211D"

    moments <- list()
    moments$expectation <- gettext("does not exist unless df > 1")
    moments$variance <- gettext("does not exist unless df > 2")

    jaspResults[['parsSupportMoments']] <- .ldParsSupportMoments(pars, support, moments)
  }
}

.ldFormulaTStudentPDF <- function(options){
}

.ldFormulaTStudentCDF <- function(options){
}

.ldFormulaTStudentQF <- function(options){
}

#### Table functions ----

.ldFillTStudentEstimatesTable <- function(table, results, options, ready){
  if(!ready) return()
  if(is.null(results)) return()
  if(is.null(table)) return()

  pars <- c(df = "df", location = "\u03BC", scale = "\u03C3")
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

#### Distribution functions ----
dtscaledshifted <- function(x, df, location, scale, log = FALSE) {
  xx <- (x-location)/scale
  out <- stats::dt(xx, df = df, ncp = 0, log = TRUE) - log(scale)

  if(!log) out <- exp(out)

  return(out)
}

ptscaledshifted <- function(q, df, location, scale, lower.tail = TRUE, log.p = FALSE) {
  qq <- (q-location)/scale
  out <- stats::pt(qq, df = df, ncp = 0, lower.tail = lower.tail, log.p = log.p)

  return(out)
}

qtscaledshifted <- function(p, df, location, scale, lower.tail = TRUE, log.p = FALSE) {
  qq <- stats::qt(p, df = df, ncp = 0, lower.tail = lower.tail, log.p = log.p)
  q <- location + qq*scale

  return(q)
}

rtscaledshifted <- function(n, df, location, scale) {
  tt <- stats::rt(n = n, df = df, ncp = 0)
  out <- location + tt*scale

  return(out)
}
