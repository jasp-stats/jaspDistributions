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

LDtNoncentralInternal <- function(jaspResults, dataset, options, state=NULL){
  options <- .ldRecodeOptionsTNoncentral(options)

  #### Show t section ----
  .ldShowDistribution(jaspResults = jaspResults, options = options, name = gettext("noncentral t-distribution"),
                      parSupportMoments = .ldTNoncentralParsSupportMoments,
                      formulaPDF        = .ldFormulaTNoncentralPDF,
                      formulaCDF        = .ldFormulaTNoncentralCDF,
                      formulaQF         = .ldFormulaTNoncentralQF)

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
  .ldMLE(jaspResults, variable, options, ready, errors, .ldFillTNoncentralEstimatesTable)

  return()
}

### options ----
.ldRecodeOptionsTNoncentral <- function(options){
  options[['parValNames']] <- c("df", "ncp")

  options[['pars']]   <- list(df = options[['df']], ncp = options[['ncp']])
  options[['pdfFun']] <- stats::dt
  options[['cdfFun']] <- stats::pt
  options[['qFun']]   <- stats::qt
  options[['rFun']]   <- stats::rt
  options[['distNameInR']] <- "t"

  options <- .ldOptionsDeterminePlotLimits(options)

  options$support <- list(min = -Inf, max = Inf)
  options$lowerBound <- c(0,  -Inf)
  options$upperBound <- c(Inf, Inf)

  options$transformations <- c(df = "df", ncp = "ncp")

  options
}

### text fill functions -----
.ldTNoncentralParsSupportMoments <- function(jaspResults, options){
  if(options$parsSupportMoments && is.null(jaspResults[['parsSupportMoments']])){
    pars <- list()
    pars[[1]] <- gettextf("degrees of freedom: %s", "df \u2208 \u211D<sup>+</sup>")
    pars[[2]] <- gettextf("non-centrality: %s",    "ncp \u2208 \u211D")

    support <- "x \u2208 \u211D"

    moments <- list()
    moments$expectation <- gettext("does not exist unless df > 1")
    moments$variance <- gettext("does not exist unless df > 2")

    jaspResults[['parsSupportMoments']] <- .ldParsSupportMoments(pars, support, moments)
  }
}

.ldFormulaTNoncentralPDF <- function(options){
    text <- "<MATH>
    f(x; <span style='color:red'>&df;</span>, <span style='color:blue'>ncp</span>) =
    </MATH>"

  return(gsub(pattern = "\n", replacement = " ", x = text))
}

.ldFormulaTNoncentralCDF <- function(options){
  text <- "<MATH>
    F(x; <span style='color:red'>&df;</span>, <span style='color:blue'>ncp</span>) =
    </MATH>"

  return(gsub(pattern = "\n", replacement = " ", x = text))
}

.ldFormulaTNoncentralQF <- function(options){
  text <- "<MATH>
    Q(p; <span style='color:red'>&df;</span>, <span style='color:blue'>ncp</span>) =
    </MATH>"

  return(gsub(pattern = "\n", replacement = " ", x = text))
}

#### Table functions ----

.ldFillTNoncentralEstimatesTable <- function(table, results, options, ready){
  if(!ready) return()
  if(is.null(results)) return()
  if(is.null(table)) return()

  par1 <- c(df = "df")
  par2 <- c(ncp = "ncp")
  res <- results$structured
  res <- res[res$par %in% names(c(par1, par2)),]
  res$parName <- c(par1, par2)

  if(results$fitdist$convergence != 0){
    table$addFootnote(gettext("The optimization did not converge, try adjusting the parameter values."), symbol = gettext("<i>Warning.</i>"))
  }
  if(!is.null(results$fitdist$optim.message)){
    table$addFootnote(results$fitdist$message, symbol = gettext("<i>Warning.</i>"))
  }

  table$setData(res)

  return()
}
