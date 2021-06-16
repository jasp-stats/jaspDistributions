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

LDwald <- function(jaspResults, dataset, options, state=NULL){
  options <- .ldRecodeOptionsWald(options)

  #### Show wald section ----
  .ldShowDistribution(jaspResults = jaspResults, options = options, name = gettext("Wald distribution"),
                      parSupportMoments = .ldWaldParsSupportMoments,
                      formulaPDF        = .ldFormulaWaldPDF,
                      formulaCDF        = .ldFormulaWaldCDF,
                      formulaQF         = .ldFormulaWaldQF)

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
  .ldMLE(jaspResults, variable, options, ready, errors, .ldFillWaldEstimatesTable)

  return()
}

### options ----
.ldRecodeOptionsWald <- function(options){
  if(options[["parametrization"]] == "mulambda"){
    options[["mu"]] <- options[["par1"]]
    options[["lambda"]] <- options[["par2"]]
  } else if(options[["parametrization"]] == "alphanu"){
    options[["mu"]] <- options[["par1"]] / options[["par2"]]
    options[["lambda"]] <- options[["par1"]]^2
  } else if(options[["parametrization"]] == "alphasigma"){
    options[["mu"]] <- options[["par1"]]
    options[["lambda"]] <- (options[["par1"]] / options[["par2"]])^2
  } else if(options[["parametrization"]] == "nusigma"){
    options[["mu"]] <- 1 / options[["par1"]]
    options[["lambda"]] <- (1 / options[["par2"]])^2
  }

  options[['parValNames']] <- c("par1", "par2")

  options[['pars']]   <- list(mu = options[['mu']], lambda = options[['lambda']])
  options[['pdfFun']] <- dwald
  options[['cdfFun']] <- pwald
  options[['qFun']]   <- qwald
  options[['rFun']]   <- rwald
  options[['distNameInR']] <- "wald"

  options <- .ldOptionsDeterminePlotLimits(options)

  options$support <- list(min = 0, max = Inf)
  options$lowerBound <- c(0, 0)
  options$upperBound <- c(Inf, Inf)

  options$transformations <- switch(options[["parametrization"]],
                                    alphasigma = c(mu = "mu", lambda = "lambda", alpha = "mu",           nu = "1",               sigma = "mu/sqrt(lambda)"),
                                    nusigma    = c(mu = "mu", lambda = "lambda", alpha = "1",            nu = "1/mu",            sigma = "1/sqrt(lambda)"),
                                                 c(mu = "mu", lambda = "lambda", alpha = "sqrt(lambda)", nu = "sqrt(lambda)/mu", sigma = "1"))

  options
}

### text fill functions -----
.ldWaldParsSupportMoments <- function(jaspResults, options){
  if(options$parsSupportMoments && is.null(jaspResults[['parsSupportMoments']])){
    pars <- list()
    pars[[1]] <- switch(options[['parametrization']],
                        mulambda = gettextf("mean: %s",       "&mu; \u2208 \u211D<sup>+</sup>"),
                        nusigma  = gettextf("drift rate: %s", "&nu; \u2208 \u211D<sup>+</sup>"),
                                   gettextf("threshold: %s",  "&alpha; \u2208 \u211D<sup>+</sup>"))
    pars[[2]] <- switch(options[['parametrization']],
                        mulambda = gettextf("shape: %s",      "&lambda; \u2208 \u211D<sup>+</sup>"),
                        alphanu  = gettextf("drift rate: %s", "&nu; \u2208 \u211D<sup>+</sup>"),
                                   gettextf("noise: %s",      "&sigma; \u2208 \u211D<sup>+</sup>"))

    support <- "x \u2208 \u211D<sup>+</sup>"

    moments <- list()
    moments$expectation <- switch(options[['parametrization']],
                                  mulambda   = "&mu;",
                                  alphanu    = "&alpha;/&nu;",
                                  alphasigma = "&alpha;",
                                  nusigma    = "1/&nu;")
    moments$variance <- switch(options[['parametrization']],
                               mulambda   = "&mu;<sup>3</sup>/&lambda;",
                               alphanu    = "&alpha;/&nu;<sup>3</sup>",
                               alphasigma = "&alpha;&sigma;<sup>2</sup>",
                               nusigma    = "&sigma;<sup>2</sup>/&nu;<sup>3</sup>")

    jaspResults[['parsSupportMoments']] <- .ldParsSupportMoments(pars, support, moments)
  }
}

.ldFormulaWaldPDF <- function(options){
}

.ldFormulaWaldCDF <- function(options){
}

.ldFormulaWaldQF <- function(options){
}

#### Table functions ----

.ldFillWaldEstimatesTable <- function(table, results, options, ready){
  if(!ready) return()
  if(is.null(results)) return()
  if(is.null(table)) return()

  whichParametrization <- which(options[["parametrization"]] == c("mulambda", "alphanu", "alphasigma", "nusigma"))
  par1 <- c(mu = "\u03BC", alpha = "\u03B1", alpha = "\u03B1", nu = "\u03BD")[whichParametrization]
  par2 <- c(lambda = "\u03BB", nu = "\u03BD", sigma = "\u03C3", sigma = "\u03C3")[whichParametrization]
  res <- results$structured
  res <- res[res$par %in% names(c(par1, par2)),]
  res$parName <- c(par1, par2)

  if(options[["parametrization"]] %in% c("alphanu", "alphasigma", "nusigma")) {
    parFixed <- switch(options[["parametrization"]],
                       alphanu    = "\u03C3",
                       alphasigma = "\u03BD",
                       nusigma    = "\u03B1")
    table$addFootnote(gettextf("Parameter %s was fixed to 1.", parFixed))
  }
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

dwald <- function(x, mu, lambda, log = FALSE) {
  alpha <- sqrt(lambda)
  nu <- alpha / mu

  out <- log(alpha) - 1.0/2.0 * log(2*pi) - 3.0/2.0*log(x) - (alpha - nu*x)^2/(2*x)
  out[x<=0] <- -Inf

  if(!log) out <- exp(out)

  return(out)
}

pwald <- function(q, mu, lambda, lower.tail = TRUE, log.p = FALSE) {
  lx <- sqrt(lambda / q)
  xmu <- q / mu
  elmu <- exp(2*lambda / mu)

  out <- ifelse(q < 0, 0,
                pnorm(lx*(xmu - 1)) + elmu * pnorm(-lx * (xmu + 1)))

  if(!lower.tail) out <- 1-out
  if(log.p) out <- log(p)

  return(out)
}

qwald <- function(p, mu, lambda, lower.tail = TRUE, log.p = FALSE) {
  if(log.p) p <- exp(p)
  if(!lower.tail) p <- 1-p

  n <- length(p)
  q <- sapply(seq_len(n), function(i) {.getQuantileWald(p[i], mu, lambda) })

  return(q)
}

.getQuantileWald <- function(p, mu, lambda) {
  o <- try(optim(par = mu, fn = .pErrorWald, lower = 0, upper = Inf, method = "L-BFGS-B",
                 p = p, pars = list(mu = mu, lambda = lambda)), silent = TRUE)

  if(inherits(o, "try-error")) {
    return(NA)
  } else {
    return(o[["par"]])
  }
}

.pErrorWald <- function(q, p, pars) {
  args <- c(q=q, pars)
  pp <- do.call(pwald, args)

  return((pp-p)^2)
}

rwald <- function(n, mu, lambda) {
  nu <- rnorm(n)
  y <- nu^2
  x <- mu + mu^2*y / (2*lambda) - mu / (2*lambda) * sqrt(4*mu*lambda*y + mu^2*y^2)
  z <- runif(n)

  out <- ifelse(z <= mu / (mu + x), x, mu^2 / x)

  return(out)
}
