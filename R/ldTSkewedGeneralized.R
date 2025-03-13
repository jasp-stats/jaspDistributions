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

LDtSkewedGeneralizedInternal <- function(jaspResults, dataset, options, state=NULL){
  options <- .ldRecodeOptionsTSkewedGeneralized(options)

  #### Show t section ----
  .ldShowDistribution(jaspResults = jaspResults, options = options, name = gettext("skewed generalized t-distribution"),
                      parSupportMoments = .ldTSkewedGeneralizedParsSupportMoments,
                      formulaPDF        = .ldFormulaTSkewedGeneralizedPDF,
                      formulaCDF        = .ldFormulaTSkewedGeneralizedCDF,
                      formulaQF         = .ldFormulaTSkewedGeneralizedQF)

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
  .ldMLE(jaspResults, variable, options, ready, errors, .ldFillTSkewedGeneralizedEstimatesTable)

  return()
}

### options ----
.ldRecodeOptionsTSkewedGeneralized <- function(options){
  options[['parValNames']] <- c("mu", "sigma", "lambda", "p", "q")

  #options[['pars']]   <- setNames(options[options[['parValNames']]], options[['parValNames']])
  options[['pars']] <- list(mu = options[['mu']], sigma = options[['sigma']], lambda = options[['lambda']],
                            pp = options[['p']], qq = options[['q']])
  options[['pdfFun']] <- function(x, mu, sigma, lambda, pp, qq, ...) sgt::dsgt(x     = x, mu = mu, sigma = sigma, lambda = lambda, p = pp, q = qq, mean.cent = FALSE, var.adj = FALSE, ...)
  options[['cdfFun']] <- function(q, mu, sigma, lambda, pp, qq, ...) sgt::psgt(quant = q, mu = mu, sigma = sigma, lambda = lambda, p = pp, q = qq, mean.cent = FALSE, var.adj = FALSE, ...)
  options[['qFun']]   <- function(p, mu, sigma, lambda, pp, qq, ...) sgt::qsgt(prob  = p, mu = mu, sigma = sigma, lambda = lambda, p = pp, q = qq, mean.cent = FALSE, var.adj = FALSE, ...)
  options[['rFun']]   <- function(n, mu, sigma, lambda, pp, qq, ...) sgt::rsgt(n     = n, mu = mu, sigma = sigma, lambda = lambda, p = pp, q = qq, mean.cent = FALSE, var.adj = FALSE, ...)
  options[['distNameInR']] <- "sgt"

  options <- .ldOptionsDeterminePlotLimits(options)

  options$support <- list(min = -Inf, max = Inf)
  options$lowerBound <- c(-Inf,   0, -1,   0,   0)
  options$upperBound <- c( Inf, Inf,  1, Inf, Inf)

  options$transformations <- c(mu = "mu", sigma = "sigma", lambda = "lambda", pp = "pp", qq = "qq")

  options
}

### text fill functions -----
.ldTSkewedGeneralizedParsSupportMoments <- function(jaspResults, options){
  if(options$parsSupportMoments && is.null(jaspResults[['parsSupportMoments']])){
    pars <- list()
    pars[[1]] <- gettextf("location: %s", "\u03BC \u2208 \u211D")
    pars[[2]] <- gettextf("scale: %s",    "\u03C3 \u2208 \u211D<sup>+</sup>")
    pars[[3]] <- gettextf("skewness: %s",    "\u03BB \u2208 [-1, 1]")
    pars[[4]] <- gettextf("kurtosis: %s",    "p \u2208 \u211D<sup>+</sup>")
    pars[[5]] <- gettextf("kurtosis: %s",    "q \u2208 \u211D<sup>+</sup>")

    support <- "x \u2208 \u211D"

    moments <- list()
    moments$expectation <- gettext("see Wikipedia: https://en.wikipedia.org/wiki/Skewed_generalized_t_distribution#Moments")
    moments$variance <- gettext("see Wikipedia: https://en.wikipedia.org/wiki/Skewed_generalized_t_distribution#Moments")

    jaspResults[['parsSupportMoments']] <- .ldParsSupportMoments(pars, support, moments)
  }
}

.ldFormulaTSkewedGeneralizedPDF <- function(options){
    text <- "<MATH>
    f(x; <span style='color:red'>&df;</span>, <span style='color:blue'>ncp</span>) =
    </MATH>"

  return(gsub(pattern = "\n", replacement = " ", x = text))
}

.ldFormulaTSkewedGeneralizedCDF <- function(options){
  text <- "<MATH>
    F(x; <span style='color:red'>&df;</span>, <span style='color:blue'>ncp</span>) =
    </MATH>"

  return(gsub(pattern = "\n", replacement = " ", x = text))
}

.ldFormulaTSkewedGeneralizedQF <- function(options){
  text <- "<MATH>
    Q(p; <span style='color:red'>&df;</span>, <span style='color:blue'>ncp</span>) =
    </MATH>"

  return(gsub(pattern = "\n", replacement = " ", x = text))
}

#### Table functions ----

.ldFillTSkewedGeneralizedEstimatesTable <- function(table, results, options, ready){
  if(!ready) return()
  if(is.null(results)) return()
  if(is.null(table)) return()

  pars <- c(mu = "\u03BC", sigma = "\u03C3", lambda = "\u03BB", pp = "p", qq = "q")
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
