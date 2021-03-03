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

LDmixtureNormalUniform <- function(jaspResults, dataset, options, state=NULL){
  options <- .ldRecodeOptionsMixtureNormalUniform(options)

  #### Show distribution section ----
  .ldShowDistribution(jaspResults = jaspResults, options = options, name = gettext("mixture of a normal and a uniform distribution"),
                      parSupportMoments = .ldMixtureNormalUniformParsSupportMoments,
                      formulaPDF        = .ldFormulaMixtureNormalUniformPDF,
                      formulaCDF        = .ldFormulaMixtureNormalUniformCDF,
                      formulaQF         = .ldFormulaMixtureNormalUniformQF)

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
  .ldMLE(jaspResults, variable, options, ready, errors, .ldFillMixtureNormalUniformEstimatesTable)

  return()
}

.ldRecodeOptionsMixtureNormalUniform <- function(options){
  options[['parValNames']] <- c("pi", "mu", "sigma", "lowerBoundPar", "upperBoundPar")

  options[['pars']]   <- list(pi = options[["pi"]], mu = options[["mu"]], sigma = options[["sigma"]], min = options[["lowerBoundPar"]], max = options[["upperBoundPar"]])
  options[['fix.pars']] <- list(min = options[["lowerBoundPar"]], max = options[["upperBoundPar"]])
  options[['pdfFun']] <- dmixnormunif
  options[['cdfFun']] <- pmixnormunif
  options[['qFun']]   <- qmixnormunif
  options[['rFun']]   <- rmixnormunif
  options[['distNameInR']] <- "mixnormunif"

  options <- .ldOptionsDeterminePlotLimits(options)

  options$support <- list(min = -Inf, max = Inf)
  options$lowerBound <- c(0, -Inf,   0, -Inf, -Inf)
  options$upperBound <- c(1,  Inf, Inf,  Inf,  Inf)

  options$transformations <- c(pi = "pi", mu = "mu", sigma = "sigma")

  options
}

### text fill functions -----
.ldMixtureNormalUniformParsSupportMoments <- function(jaspResults, options){
  if(options$parsSupportMoments && is.null(jaspResults[['parsSupportMoments']])){
    pars <- list()
    pars[[1]] <- gettextf("probability of normal component: %s",       "&pi; \u2208 [0, 1]")
    pars[[2]] <- gettextf("mean of normal distribution: %s",           "&mu; \u2208 \u211D")
    pars[[3]] <- gettextf("std. deviation of normal distribution: %s", "&sigma; \u2208 \u211D<sup>+</sup>")
    pars[[4]] <- gettextf("minimum of uniform distribution: %s",       "a \u2208 \u211D, a < b")
    pars[[5]] <- gettextf("maximum of uniform distribution: %s",       "b \u2208 \u211D, b > a")

    support <- "x \u2208 \u211D"

    moments <- list()
    moments$expectation <- "&pi;&mu; + (1-&pi;)(a+b)/2"
    moments$variance <- ""

    jaspResults[['parsSupportMoments']] <- .ldParsSupportMoments(pars, support, moments)
  }
}

.ldFormulaMixtureNormalUniformPDF <- function(options){
}

.ldFormulaMixtureNormalUniformCDF <- function(options){
}

.ldFormulaMixtureNormalUniformQF <- function(options){
}

#### Table functions ----

.ldFillMixtureNormalUniformEstimatesTable <- function(table, results, options, ready){
  if(!ready) return()
  if(is.null(results)) return()
  if(is.null(table)) return()

  par <- c(pi = "\u03C0", mu = "\u03BC", sigma = "\u03C3")
  res <- results$structured
  res$parName <- par

  table$addFootnote(gettextf("Parameter a was fixed at value %s, parameter b was fixed at value %s.", options$fix.pars[1], options$fix.pars[2]))
  if(results$fitdist$convergence != 0){
    table$addFootnote(gettext("The optimization did not converge, try adjusting the parameter values."), symbol = gettext("<i>Warning.</i>"))
  }
  if(!is.null(results$fitdist$optim.message)){
    table$addFootnote(results$fitdist$message, symbol = gettext("<i>Warning.</i>"))
  }

  table$setData(res)

  return()
}

### Distribution functions ----

dmixnormunif <- function(x, pi, mu, sigma, min, max, log = FALSE) {
  out <- pi * dnorm(x, mu, sigma) + (1-pi) * dunif(x, min, max)

  if(log) out <- log(out)

  return(out)
}

pmixnormunif <- function(q, pi, mu, sigma, min, max, lower.tail = TRUE, log.p = FALSE) {
  out <- pi * pnorm(q, mu, sigma) + (1-pi) * punif(q, min, max)

  if(!lower.tail) out <- 1-out
  if(log.p) out <- log(out)

  return(out)
}

qmixnormunif <- function(p, pi, mu, sigma, min, max, lower.tail = TRUE, log.p = FALSE) {
  if(log.p) p <- exp(p)
  if(!lower.tail) p <- 1-p
  n <- length(p)
  q <- sapply(seq_len(n), function(i) { .getQuantileMixtureNormalUniform(p[i], pi, mu, sigma, min, max) })

  return(q)
}

.getQuantileMixtureNormalUniform <- function(p, pi, mu, sigma, min, max) {
  o <- try(optim(par = mu, fn = .pErrorMixtureNormalUniform, lower = -Inf, upper = Inf, method = "L-BFGS-B",
                 p = p, pars = list(pi=pi, mu=mu, sigma=sigma, min=min, max=max)), silent = TRUE)

  if(inherits(o, "try-error")) {
    return(NA)
  } else {
    return(o[["par"]])
  }
}

.pErrorMixtureNormalUniform <- function(q, p, pars) {
  args <- c(q=q, pars)
  pp <- do.call(pmixnormunif, args)

  return((pp-p)^2)
}

rmixnormunif <- function(n, pi, mu, sigma, min, max) {
  component <- sample(1:2, size = n, replace = TRUE, prob = c(pi, 1-pi))

  out <- ifelse(component == 1, rnorm(n, mu, sigma), runif(n, min, max))

  return(out)
}
