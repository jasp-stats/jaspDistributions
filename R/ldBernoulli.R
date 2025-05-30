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

LDbernoulliInternal <- function(jaspResults, dataset, options, state=NULL){
  options <- .ldRecodeOptionsBernoulli(options)

  #### Show bernoulli section ----
  .ldShowDistribution(jaspResults = jaspResults, options = options, name = gettext("Bernoulli distribution"),
                      parSupportMoments = .ldBernoulliParsSupportMoments,
                      formulaPMF        = .ldFormulaBernoulliPMF,
                      formulaCMF        = .ldFormulaBernoulliCDF)

  #### Generate and Display data section ----
  # simulate and read data
  .simulateData(jaspResults, options, as="nominal")
  ready <- options[['variable']] != ""
  errors <- FALSE
  if(ready){
    variable <- dataset[[options[['variable']]]]
    variable <- as.factor(variable[!is.na(variable)])
    errors <- .hasErrors(dataset, type = c("observations", "factorLevels"),
                         observations.amount = "<2",
                         factorLevels.amount = "!=2",
                         exitAnalysisIfErrors = FALSE)
  }

  # overview of the data
  .ldDescriptives(jaspResults, variable, options, ready, errors, "factor")

  #### Fit data and assess fit ----
  .ldMLE(jaspResults, as.numeric(variable)-1, options, ready, errors, .ldFillBernoulliEstimatesTable, levels = levels(variable))

  return()
}

### options ----
.ldRecodeOptionsBernoulli <- function(options){

  options[['parValNames']] <- c("prob")

  options[['pars']]   <- list(prob = options[['prob']])
  options[['fix.pars']] <- list(size = 1)

  options[['pdfFun']] <- function(x, size = 1, prob, log = FALSE){
    stats::dbinom(x = x, size = size, prob = prob, log = log)
    }
  options[['cdfFun']] <- function(q, size = 1, prob, lower.tail = TRUE, log.p = FALSE){
    stats::pbinom(q = q, size = size, prob = prob, lower.tail = lower.tail, log.p = log.p)
    }
  options[['qFun']]   <- function(p, size = 1, prob, lower.tail = TRUE, log.p = FALSE){
    stats::qbinom(p = p, size = size, prob = prob, lower.tail = lower.tail, log.p = log.p)
  }
  options[['rFun']]   <- function(n, size = 1, prob) { rbinom(n = n, size = 1, prob = prob)}
  options[['distNameInR']] <- "binom"

  options[['range_x']] <- c(0, 1)

  options[['highlightmin']] <- 0
  options[['highlightmax']] <- 1

  options$support <- list(min = 0, max = 1)
  options$lowerBound <- c(0)
  options$upperBound <- c(1)

  options$transformations <- c(prob0 = "1-prob", prob1 = "prob")

  options
}

### text fill functions -----
.ldBernoulliParsSupportMoments <- function(jaspResults, options){
  if(options$parsSupportMoments && is.null(jaspResults[['parsSupportMoments']])){
    pars <- list()
    pars[[1]] <- gettextf("probability of success: p %1$s %2$s: 0 %3$s p %4$s 1", "\u2208", "\u211D", "\u2264", "\u2264")
    support <- gettextf("x %s {0, 1}", "\u2208")

    moments <- list()
    moments$expectation <- gettext("p")
    moments$variance <- gettext("p(1-p)")

    jaspResults[['parsSupportMoments']] <- .ldParsSupportMoments(pars, support, moments)
  }
}

.ldFormulaBernoulliPMF <- function(options){
    text <- "<MATH>
    f(x; <span style='color:red'>p</span>) =
    </MATH>"

  return(gsub(pattern = "\n", replacement = " ", x = text))
}

.ldFormulaBernoulliCDF <- function(options){
  text <- "<MATH>
    F(x; <span style='color:red'>p</span>) =
    </MATH>"

  return(gsub(pattern = "\n", replacement = " ", x = text))
}

.ldFormulaBernoulliQF <- function(options){
  text <- "<MATH>
    Q(x; <span style='color:red'>p</span>) =
    </MATH>"

  return(gsub(pattern = "\n", replacement = " ", x = text))
}

#### Table functions ----

.ldFillBernoulliEstimatesTable <- function(table, results, options, ready, levels){
  if(!ready) return()
  if(is.null(results)) return()
  if(is.null(table)) return()

  res <- results$structured
  res$parName <- sprintf("p (%s)", levels)

  if(results$fitdist$convergence != 0){
    table$addFootnote(gettext("The optimization did not converge, try adjusting the parameter values."), symbol = gettext("<i>Warning.</i>"))
  }
  if(!is.null(results$fitdist$optim.message)){
    table$addFootnote(results$fitdist$message, symbol = gettext("<i>Warning.</i>"))
  }

  table$setData(res)

  return()
}
