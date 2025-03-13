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

LDamorosoInternal <- function(jaspResults, dataset, options, state=NULL){
  options <- .recodeOptionsLDAmoroso(options)

  #### Show distribution section ----
  .ldShowDistribution(jaspResults = jaspResults, options = options, name = gettext("Amoroso distribution"),
                      parSupportMoments = .ldAmorosoParsSupportMoments,
                      formulaPDF        = .ldFormulaAmorosoPDF,
                      formulaCDF        = .ldFormulaAmorosoCDF,
                      formulaQF         = .ldFormulaAmorosoQF)

  #### Generate and Display data section ----
  # simulate and read data
  .simulateData(jaspResults, options)

  ready <- options[["variable"]] != ""
  errors <- FALSE
  if(ready){
    variable <- dataset[[options[["variable"]]]]
    variable <- variable[!is.na(variable)]
    errors <- .hasErrors(dataset, type = c("observations", "variance", "infinity", "limits"),
                         observations.amount = "<2",
                         limits.min = options$support$min, limits.max = options$support$max,
                         exitAnalysisIfErrors = FALSE)
  }

  # overview of the data
  .ldDescriptives(jaspResults, variable, options, ready, errors, "continuous")

  #### Fit data and assess fit ----
  .ldMLE(jaspResults, variable, options, ready, errors, .ldFillAmorosoEstimatesTable)

  return()
}

.recodeOptionsLDAmoroso <- function(options){
  options[["parValNames"]] <- c("a", "theta", "alpha", "beta")

  options[["pars"]]   <- list(a = options[["a"]], theta = options[["theta"]], alpha = options[["alpha"]], beta = options[["beta"]])
  options[["pdfFun"]] <- damoroso
  options[["cdfFun"]] <- pamoroso
  options[["qFun"]]   <- qamoroso
  options[["rFun"]]   <- ramoroso
  options[["distNameInR"]] <- "amoroso"

  options <- .ldOptionsDeterminePlotLimits(options)

  options$support <- list(min = -Inf, max = Inf)
  if(options[['theta']] > 0) {
    options$lowerBound <- c(-Inf,   0,   0,   0)
    options$upperBound <- c( Inf, Inf, Inf, Inf)
  } else {
    options$lowerBound <- c(-Inf, -Inf,   0,   0)
    options$upperBound <- c( Inf,    0, Inf, Inf)
  }

  options$transformations <- c(a = "a", theta = "theta", alpha = "alpha", beta = "beta")

  options
}

### text fill functions -----
.ldAmorosoParsSupportMoments <- function(jaspResults, options){
  if(options$parsSupportMoments && is.null(jaspResults[["parsSupportMoments"]])){
    pars <- list()
    pars[[1]] <- gettextf("location: a %s","\u2208 \u211D")
    pars[[2]] <- gettextf("scale: %s", "&theta; \u2208 \u211D")
    pars[[3]] <- gettextf("shape: %s", "&alpha; \u2208 \u211D<sup>+</sup>")
    pars[[4]] <- gettextf("shape: %s", "&beta; \u2208 \u211D<sup>+</sup>")

    support <- "x \u2208 \u211D; x > a if &theta; > 0, x < a if &theta; < 0"

    moments <- list()
    moments$expectation <- "a + &theta; &Gamma;(&alpha; + 1/&beta;)/&Gamma;(&alpha;)"
    moments$variance <- "&theta;<sup>2</sup>[&Gamma;(&alpha; + 2/&beta;)/&Gamma;(&alpha;) - &Gamma;(&alpha; + 1/&beta;)<sup>2</sup>/&Gamma;(&alpha;)<sup>2</sup>]"

    jaspResults[["parsSupportMoments"]] <- .ldParsSupportMoments(pars, support, moments)
  }
}

.ldFormulaAmorosoPDF <- function(options){
}

.ldFormulaAmorosoCDF <- function(options){
}

.ldFormulaAmorosoQF <- function(options){
}

#### Table functions ----

.ldFillAmorosoEstimatesTable <- function(table, results, options, ready){
  if(!ready) return()
  if(is.null(results)) return()
  if(is.null(table)) return()

  par <- c(a = "a", theta = "\u03B8", alpha = "\u03B1", beta = "\u03B2")
  res <- results$structured
  res$parName <- par

  if(results$fitdist$convergence != 0){
    table$addFootnote(gettext("The optimization did not converge, try adjusting the parameter values."), symbol = gettext("<i>Warning.</i>"))
  }
  if(!is.null(results$fitdist$optim.message)){
    table$addFootnote(results$fitdist$message, symbol = gettext("<i>Warning.</i>"))
  }
  if(options[['theta']] > 0) {
    table$addFootnote(gettextf("Scale parameter %s is assumed positive.", "\u03B8"))
  } else {
    table$addFootnote(gettextf("Scale parameter %s is assumed negative.", "\u03B8"))
  }

  table$setData(res)

  return()
}

#### Distribution functions ----
damoroso <- function(x, a, theta, alpha, beta, log = FALSE) {
  terms <- list()
  xi <- (x-a)/theta

  terms[[1]] <- -lgamma(alpha)
  terms[[2]] <- log(abs(beta/theta))
  terms[[3]] <- (alpha*beta - 1) * log(xi)
  terms[[4]] <- - xi^beta

  out <- Reduce("+", terms)
  if(theta > 0) {
    out[x<a] <- -Inf
  } else {
    out[x>a] <- -Inf
  }
  if(!log) out <- exp(out)

  return(out)
}

pamoroso <- function(q, a, theta, alpha, beta, lower.tail = TRUE, log.p = FALSE) {
  xi <- (q-a)/theta
  arg1 <- alpha
  arg2 <- xi^beta
  out <- stats::pgamma(q=arg2, scale = 1, shape = arg1, lower.tail = lower.tail, log.p = FALSE)

  if(theta > 0) {
    out[q<a] <- if(lower.tail) 0 else 1
  } else {
    out <- 1 - out
    out[q>a] <- if(lower.tail) 1 else 0
  }
  if(log.p) out <- log(out)

  return(out)
}

qamoroso <- function(p, a, theta, alpha, beta, lower.tail = TRUE, log.p = FALSE) {
  gamma <- stats::qgamma(p = p, scale = 1, shape = alpha, lower.tail = lower.tail, log.p = log.p)
  q <- a + theta*gamma^(1/beta)

  return(q)
}


ramoroso <- function(n, a, theta, alpha, beta) {
  gamma <- stats::rgamma(n = n, scale = 1, shape = alpha)
  q <- a + theta*gamma^(1/beta)

  return(q)
}
