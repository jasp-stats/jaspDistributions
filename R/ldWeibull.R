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

LDweibullInternal <- function(jaspResults, dataset, options, state=NULL){
  options <- .ldRecodeOptionsWeibull(options)

  #### Show weibull section ----
  .ldShowDistribution(jaspResults = jaspResults, options = options, name = gettext("Weibull distribution"),
                      parSupportMoments = .ldWeibullParsSupportMoments,
                      formulaPDF        = .ldFormulaWeibullPDF,
                      formulaCDF        = .ldFormulaWeibullCDF,
                      formulaQF         = .ldFormulaWeibullQF)

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
  .ldMLE(jaspResults, variable, options, ready, errors, .ldFillWeibullEstimatesTable)

  return()
}

### options ----
.ldRecodeOptionsWeibull <- function(options){
  options[['parValNames']] <- c("shape", "scale")

  options[['pars']]   <- list(shape = options[["shape"]], scale = options[["scale"]])
  options[['pdfFun']] <- stats::dweibull
  options[['cdfFun']] <- stats::pweibull
  options[['qFun']]   <- stats::qweibull
  options[['rFun']]   <- stats::rweibull
  options[['distNameInR']] <- "weibull"

  options <- .ldOptionsDeterminePlotLimits(options)

  options$support <- list(min = 0, max = Inf)
  options$lowerBound <- c(0)
  options$upperBound <- c(Inf)

  options$transformations <- c(shape = "shape", scale = "scale")

  options
}

### text fill functions -----
.ldWeibullParsSupportMoments <- function(jaspResults, options){
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

.ldFormulaWeibullPDF <- function(options){
  if(options[['parametrization']] == "scale"){
    text <- "<MATH>
    f(x; <span style='color:red'>&beta;</span>) =
    </MATH>"
  } else {
    text <- "<MATH>
    f(x; <span style='color:red'>&lambda;</span>) = <span style='color:red'>&lambda;</span>exp(-<span style='color:red'>&lambda;</span>x)
    </MATH>"
  }

  return(gsub(pattern = "\n", replacement = " ", x = text))
}

.ldFormulaWeibullCDF <- function(options){
  if(options$parametrization == "scale"){
    text <- "<MATH>
    F(x; <span style='color:red'>&beta;</span>) =
    </MATH>"
  } else{

  }

  return(gsub(pattern = "\n", replacement = " ", x = text))
}

.ldFormulaWeibullQF <- function(options){
  if(options$parametrization == "rate"){
    text <- "<MATH>
    Q(p; <span style='color:red'>&beta;</span>) =
    </MATH>"
  } else{

  }

  return(gsub(pattern = "\n", replacement = " ", x = text))
}

#### Table functions ----

.ldFillWeibullEstimatesTable <- function(table, results, options, ready){
  if(!ready) return()
  if(is.null(results)) return()
  if(is.null(table)) return()

  par <- c(shape = "k", scale = "\u03BB")
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
