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

LDmixtureNormalNormalInternal <- function(jaspResults, dataset, options, state=NULL){
  options <- .ldRecodeOptionsMixtureNormalNormal(options)

  #### Show distribution section ----
  .ldShowDistribution(jaspResults = jaspResults, options = options, name = gettext("mixture of two normal distributions"),
                      parSupportMoments = .ldMixtureNormalNormalParsSupportMoments,
                      formulaPDF        = .ldFormulaMixtureNormalNormalPDF,
                      formulaCDF        = .ldFormulaMixtureNormalNormalCDF,
                      formulaQF         = .ldFormulaMixtureNormalNormalQF)

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
  .ldMLE(jaspResults, variable, options, ready, errors, .ldFillMixtureNormalNormalEstimatesTable)

  return()
}

.ldRecodeOptionsMixtureNormalNormal <- function(options){
  options[['parValNames']] <- c("pi", "mu1", "sigma1", "mu2", "sigma2")

  options[['pars']]   <- options[options[['parValNames']]]
  options[['pdfFun']] <- dmixnormnorm
  options[['cdfFun']] <- pmixnormnorm
  options[['qFun']]   <- qmixnormnorm
  options[['rFun']]   <- rmixnormnorm
  options[['distNameInR']] <- "mixnormnorm"

  options <- .ldOptionsDeterminePlotLimits(options)

  options$support <- list(min = -Inf, max = Inf)
  options$lowerBound <- c(0, -Inf,   0, -Inf,   0)
  options$upperBound <- c(1,  Inf, Inf,  Inf, Inf)

  options$transformations <- setNames(options[['parValNames']], options[['parValNames']])

  options
}

### text fill functions -----
.ldMixtureNormalNormalParsSupportMoments <- function(jaspResults, options){
  if(options$parsSupportMoments && is.null(jaspResults[['parsSupportMoments']])){
    pars <- list()
    pars[[1]] <- gettextf("probability of first component: %s", "&pi; \u2208 [0, 1]")
    pars[[2]] <- gettextf("mean 1: %s",           "&mu;\u2081 \u2208 \u211D")
    pars[[3]] <- gettextf("std. deviation 1: %s", "&sigma;\u2081 \u2208 \u211D<sup>+</sup>")
    pars[[4]] <- gettextf("mean 2: %s",           "&mu;\u2082 \u2208 \u211D")
    pars[[5]] <- gettextf("std. deviation 2: %s", "&sigma;\u2082 \u2208 \u211D<sup>+</sup>")

    support <- "x \u2208 \u211D"

    moments <- list()
    moments$expectation <- "&pi;&mu;\u2081 + (1-&pi;)&mu;\u2082"
    moments$variance <- "&pi;&sigma;\u2081<sup>2</sup> + (1-&pi;)&sigma;\u2082<sup>2</sup> + [&pi;&mu;\u2081<sup>2</sup> + (1-&pi;)&mu;\u2082<sup>2</sup> - (&pi;&mu;\u2081 + (1-&pi;)&mu;\u2082)<sup>2</sup>]"

    jaspResults[['parsSupportMoments']] <- .ldParsSupportMoments(pars, support, moments)
  }
}

.ldFormulaMixtureNormalNormalPDF <- function(options){
}

.ldFormulaMixtureNormalNormalCDF <- function(options){
}

.ldFormulaMixtureNormalNormalQF <- function(options){
}

#### Table functions ----

.ldFillMixtureNormalNormalEstimatesTable <- function(table, results, options, ready){
  if(!ready) return()
  if(is.null(results)) return()
  if(is.null(table)) return()

  par <- c(pi = "\u03C0", mu1 = "\u03BC\u2081", sigma1 = "\u03C3\u2081", mu2 = "\u03BC\u2082", sigma2 = "\u03C3\u2082" )
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

### Distribution functions ----

dmixnormnorm <- function(x, pi, mu1, sigma1, mu2, sigma2, log = FALSE) {
  out <- pi * dnorm(x, mu1, sigma1) + (1-pi) * dnorm(x, mu2, sigma2)

  if(log) out <- log(out)

  return(out)
}

pmixnormnorm <- function(q, pi, mu1, sigma1, mu2, sigma2, lower.tail = TRUE, log.p = FALSE) {
  out <- pi * pnorm(q, mu1, sigma1) + (1-pi) * pnorm(q, mu2, sigma2)

  if(!lower.tail) out <- 1-out
  if(log.p) out <- log(out)

  return(out)
}

qmixnormnorm <- function(p, pi, mu1, sigma1, mu2, sigma2, lower.tail = TRUE, log.p = FALSE) {
  if(log.p) p <- exp(p)
  if(!lower.tail) p <- 1-p
  n <- length(p)
  q <- sapply(seq_len(n), function(i) { .getQuantileMixtureNormalNormal(p[i], pi, mu1, sigma1, mu2, sigma2) })

  return(q)
}

.getQuantileMixtureNormalNormal <- function(p, pi, mu1, sigma1, mu2, sigma2) {
  o <- try(optim(par = pi*mu1 + (1-pi)*mu2, fn = .pErrorMixtureNormalNormal, lower = -Inf, upper = Inf, method = "L-BFGS-B",
                 p = p, pars = list(pi=pi, mu1=mu1, sigma1=sigma1, mu2=mu2, sigma2=sigma2)), silent = TRUE)

  if(inherits(o, "try-error")) {
    return(NA)
  } else {
    return(o[["par"]])
  }
}

.pErrorMixtureNormalNormal <- function(q, p, pars) {
  args <- c(q=q, pars)
  pp <- do.call(pmixnormnorm, args)

  return((pp-p)^2)
}

rmixnormnorm <- function(n, pi, mu1, sigma1, mu2, sigma2) {
  component <- sample(1:2, size = n, replace = TRUE, prob = c(pi, 1-pi))

  out <- rnorm(n = n, mean = c(mu1, mu2)[component], sd = c(sigma1, sigma2)[component])

  return(out)
}
