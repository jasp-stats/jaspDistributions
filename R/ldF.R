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

LDfInternal <- function(jaspResults, dataset, options, state=NULL){
  options <- .ldRecodeOptionsF(options)

  #### Show f section ----
  .ldShowDistribution(jaspResults = jaspResults, options = options, name = gettext("F-distribution"),
                      parSupportMoments = .ldFParsSupportMoments,
                      formulaPDF        = .ldFormulaFPDF,
                      formulaCDF        = .ldFormulaFCDF,
                      formulaQF         = .ldFormulaFQF)

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
  .ldMLE(jaspResults, variable, options, ready, errors, .ldFillFEstimatesTable)

  return()
}

### options ----
.ldRecodeOptionsF <- function(options){
  options[['parValNames']] <- c("df1", "df2", "ncp")

  options[['pars']]   <- list(df1 = options[['df1']], df2 = options[['df2']], ncp = options[['ncp']])
  options[['pdfFun']] <- stats::df
  options[['cdfFun']] <- stats::pf
  options[['qFun']]   <- stats::qf
  options[['rFun']]   <- stats::rf
  options[['distNameInR']] <- "f"

  options <- .ldOptionsDeterminePlotLimits(options)

  options$support <- list(min = 0, max = Inf)
  options$lowerBound <- c(0, 0, -Inf)
  options$upperBound <- c(Inf, Inf, Inf)

  options$transformations <- c(df1 = "df1", df2 = "df2", ncp = "ncp")

  options
}

### text fill functions -----
.ldFParsSupportMoments <- function(jaspResults, options){
  if(options$parsSupportMoments && is.null(jaspResults[['parsSupportMoments']])){
    pars <- list()
    pars[[1]] <- gettextf("degree of freedom: %s", "df 1 \u2208 \u211D<sup>+</sup>")
    pars[[2]] <- gettextf("degree of freedom: %s",  "df 2 \u2208 \u211D<sup>+</sup>")
    pars[[3]] <- gettextf("non-centrality: %s",     "ncp \u2208 \u211D")

    support <- "x \u2208 \u211D<sup>+</sup>"

    jaspResults[['parsSupportMoments']] <- .ldParsSupportMoments(pars, support, NA)
  }
}

.ldFormulaFPDF <- function(options){
    text <- "<MATH>
    f(x; <span style='color:red'>df 1</span>,
    <span style='color:green'>df 2</span>,
    <span style='color:blue'>ncp</span>)
    </MATH>"

  return(gsub(pattern = "\n", replacement = " ", x = text))
}

.ldFormulaFCDF <- function(options){
  text <- "<MATH>
    F(x; <span style='color:red'>df 1</span>,
    <span style='color:green'>df 2</span>,
    <span style='color:blue'>ncp</span>)
    </MATH>"

  return(gsub(pattern = "\n", replacement = " ", x = text))
}

.ldFormulaFQF <- function(options){
  text <- "<MATH>
    Q(p; <span style='color:red'>df 1</span>,
    <span style='color:green'>df 2</span>,
    <span style='color:blue'>ncp</span>)
    </MATH>"

  return(gsub(pattern = "\n", replacement = " ", x = text))
}

#### Table functions ----

.ldFillFEstimatesTable <- function(table, results, options, ready){
  if(!ready) return()
  if(is.null(results)) return()
  if(is.null(table)) return()

  res <- results$structured
  res$parName <- c("df 1", "df 2", "ncp")

  if(results$fitdist$convergence != 0){
    table$addFootnote(gettext("The optimization did not converge, try adjusting the parameter values."), symbol = gettext("<i>Warning.</i>"))
  }
  if(!is.null(results$fitdist$optim.message)){
    table$addFootnote(results$fitdist$message, symbol = gettext("<i>Warning.</i>"))
  }

  table$setData(res)

  return()
}
