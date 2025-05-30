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

LDgaussianunivariateInternal <- function(jaspResults, dataset, options, state=NULL){
  options <- .recodeOptionsLDGaussianUnivariate(options)

  #### Show distribution section ----
  .ldShowDistribution(jaspResults = jaspResults, options = options, name = gettext("normal distribution"),
                      parSupportMoments = .ldGaussianParsSupportMoments,
                      formulaPDF        = .ldFormulaGaussianPDF,
                      formulaCDF        = .ldFormulaGaussianCDF,
                      formulaQF         = .ldFormulaGaussianQF)

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
  analyticEstimates <- .ldMLEGaussian(variable, options, ready, errors)
  .ldMLE(jaspResults, variable, options, ready, errors, .ldFillGaussianEstimatesTable, analyticEstimates, normality=TRUE)

  return()
}

### options ----
.recodeOptionsLDGaussianUnivariate <- function(options){
  if(options$parametrization == "sigma2"){
    options$sd <- sqrt(options$varValue)
  } else if(options$parametrization == "sigma"){
    options$sd <- options$varValue
  } else if(options$parametrization == "tau"){
    options$sd <- sqrt(1/options$varValue)
  } else if(options$parametrization == "kappa"){
    options$sd <- 1/options$varValue
  }

  options[['parValNames']] <- c("mu", "varValue")

  options[['pars']]   <- list(mean = options[['mu']], sd = options[['sd']])
  options[['pdfFun']] <- stats::dnorm
  options[['cdfFun']] <- stats::pnorm
  options[['qFun']]   <- stats::qnorm
  options[['rFun']]   <- stats::rnorm
  options[['distNameInR']] <- "norm"

  options <- .ldOptionsDeterminePlotLimits(options)

  options$support <- list(min = -Inf, max = Inf)
  options$lowerBound <- c(-Inf, 0)
  options$upperBound <- c(Inf, Inf)

  options$transformations <- c(mu = "mean", sigma2 = "sd^2", sigma = "sd", tau = "1/sd^2", kappa = "1/sd")

  options
}

### text fill functions -----
.ldGaussianParsSupportMoments <- function(jaspResults, options){
  if(options$parsSupportMoments && is.null(jaspResults[['parsSupportMoments']])){
    pars <- list()
    pars[[1]] <- gettextf("mean: &mu; %s","\u2208 \u211D")
    pars[[2]] <- switch(options[['parametrization']],
                        sigma2 = gettextf("variance: %s",                 "&sigma;<sup>2</sup> \u2208 \u211D<sup>+</sup>"),
                        sigma  = gettextf("standard deviation: %s",       "&sigma; \u2208 \u211D<sup>+</sup>"),
                        tau    = gettextf("precision: %s",                "&tau; \u2208 \u211D<sup>+</sup>"),
                        kappa  = gettextf("square root of precision: %s", "&kappa; \u2208 \u211D<sup>+</sup>"))

    support <- "x \u2208 \u211D"

    moments <- list()
    moments$expectation <- gettext("&mu;")
    moments$variance <- switch(options[['parametrization']],
                               sigma2 = "&sigma;<sup>2</sup>",
                               sigma  = "&sigma;<sup>2</sup>",
                               tau    = "1/&tau;",
                               kappa  = "1/&kappa;<sup>2</sup>")

    jaspResults[['parsSupportMoments']] <- .ldParsSupportMoments(pars, support, moments)
  }
}

.ldFormulaGaussianPDF <- function(options){
  if(options[['parametrization']] == "sigma2"){
    text <- "<MATH>
    f(x; <span style='color:red'>&mu;</span>, <span style='color:blue'>&sigma;&sup2;</span>) =
(2&pi;<span style='color:blue'>&sigma;&sup2;</span>)<sup>-&frac12;</sup>
exp[-(x-<span style='color:red'>&mu;</span>)&sup2; &frasl; 2<span style='color:blue'>&sigma;&sup2;</span>]
    </MATH>"
  } else if(options[['parametrization']] == "sigma"){
    text <- "<MATH>
    f(x; <span style='color:red'>&mu;</span>, <span style='color:blue'>&sigma;</span>) =
    (2&pi;<span style='color:blue'>&sigma;</span>&sup2;)<sup>-&frac12;</sup>
    exp[-(x-<span style='color:red'>&mu;</span>)&sup2; &frasl; 2<span style='color:blue'>&sigma;</span>&sup2;]
    </MATH>"
  } else if(options[['parametrization']] == "tau2"){
    text <- "<MATH>
    f(x; <span style='color:red'>&mu;</span>, <span style='color:blue'>&tau;&sup2;</span>) =
    (<span style='color:blue'>&tau;&sup2;</span> &frasl; 2&pi;)<sup>&frac12;</sup>
    exp[-(x-<span style='color:red'>&mu;</span>)&sup2; <span style='color:blue'>&tau;&sup2;</span> &frasl; 2]
    </MATH>"
  } else if(options[['parametrization']] == "tau"){
    text <- "<MATH>
    f(x; <span style='color:red'>&mu;</span>, <span style='color:blue'>&tau;</span>) =
    <span style='color:blue'>&tau;</span> &frasl; (2&pi;)<sup>&frac12;</sup>
    exp[-(x-<span style='color:red'>&mu;</span>)&sup2; <span style='color:blue'>&tau;</span>&sup2; &frasl; 2]
    </MATH>"
  }

  return(gsub(pattern = "\n", replacement = " ", x = text))
}

.ldFormulaGaussianCDF <- function(options){
  if(options$parametrization == "sigma2"){
    text <- "<MATH>
    F(x; <span style='color:red'>&mu;</span>, <span style='color:blue'>&sigma;&sup2;</span>)
    </MATH>"
  } else if(options$parametrization == "sigma"){
    text <- "<MATH>
    F(x; <span style='color:red'>&mu;</span>, <span style='color:blue'>&sigma;</span>)
    </MATH>"
  } else if(options$parametrization == "tau2"){
    text <- "<MATH>
    F(x; <span style='color:red'>&mu;</span>, <span style='color:blue'>&tau;&sup2;</span>)
    </MATH>"
  } else {
    text <- "<MATH>
    F(x; <span style='color:red'>&mu;</span>, <span style='color:blue'>&tau;</span>)
    </MATH>"
  }

  return(gsub(pattern = "\n", replacement = " ", x = text))
}

.ldFormulaGaussianQF <- function(options){
  if(options$parametrization == "sigma2"){
    text <- "<MATH>
    Q(p; <span style='color:red'>&mu;</span>, <span style='color:blue'>&sigma;&sup2;</span>)
    </MATH>"
  } else if(options$parametrization == "sigma"){
    text <- "<MATH>
    Q(p; <span style='color:red'>&mu;</span>, <span style='color:blue'>&sigma;</span>)
    </MATH>"
  } else if(options$parametrization == "tau2"){
    text <- "<MATH>
    Q(p; <span style='color:red'>&mu;</span>, <span style='color:blue'>&tau;&sup2;</span>)
    </MATH>"
  } else {
    text <- "<MATH>
    Q(p; <span style='color:red'>&mu;</span>, <span style='color:blue'>&tau;</span>)
    </MATH>"
  }

  return(gsub(pattern = "\n", replacement = " ", x = text))
}

#### Table functions ----

.ldFillGaussianEstimatesTable <- function(table, results, options, ready){
  if(!ready) return()
  if(is.null(results)) return()
  if(is.null(table)) return()

  if (options[["biasCorrected"]])
    table$addFootnote(gettext("Unbiased with Bessel's correction."), colNames="estimate", rowNames="scale")

  table$setData(results[["structured"]])

  return()
}


.ldMLEGaussian <- function(variable, options, ready, errors) {
  if(!ready || !isFALSE(errors)) return()
  results <- list()

  pLowerCI <- (1-options[['ciIntervalInterval']]) / 2
  pUpperCI <- 1 - pLowerCI
  pCi <- c(pLowerCI, pUpperCI)

  n <- length(variable)
  df <- n-1

  sigma2 <- var(variable)
  chiSq <- qchisq(pCi, df)

  if (options[["biasCorrected"]]){
    sigma2Ci <- sigma2 * df / rev(chiSq)
    seScale <- df
  } else {
    sigma2 <- sigma2 * df / n
    sigma2Ci <- sigma2 * n / rev(chiSq)
    seScale <- n
  }

  mu <- mean(variable)
  t <- qt(pCi, df=df)
  muSe <- sqrt(sigma2) / sqrt(n)
  muCi <- mu + t * muSe

  loc <- data.frame(
    par="mu", parName="\u03BC", estimate=mu, se=muSe, lower=muCi[1], upper=muCi[2]
  )

  scale <- switch(
    options[["parametrization"]],
    sigma2 = data.frame(par="sigma2",
                        parName="\u03C3\u00B2",
                        estimate=sigma2,
                        se=sigma2 * sqrt(2) / seScale,
                        lower=sigma2Ci[1],
                        upper=sigma2Ci[2]),
    sigma  = data.frame(par="sigma",
                        parName="\u03C3",
                        estimate=sqrt(sigma2),
                        se=sqrt(sigma2 / (2 * seScale)),
                        lower=sqrt(sigma2Ci[1]),
                        upper=sqrt(sigma2Ci[2])),
    tau    = data.frame(par="tau",
                        parName="\u03C4",
                        estimate=1/sigma2,
                        se=sqrt(2)/ (sigma2 * seScale),
                        lower=1/sigma2Ci[2],
                        upper=1/sigma2Ci[1]),
    kappa  = data.frame(par="kappa",
                        parName="\u03BA",
                        estimate=1/sqrt(sigma2),
                        se=1/sqrt(2*sigma2*seScale),
                        lower=1/sqrt(sigma2Ci[2]),
                        upper=1/sqrt(sigma2Ci[1]))
  )

  results$structured <- rbind(loc, scale)
  rownames(results$structured) <- c("loc", "scale")

  results$fitdist <- list()
  results$fitdist$convergence <- 0
  results$fitdist$estimate <- c(mean=mu, sd=sqrt(sigma2))
  results$ci.possible <- TRUE
  results$se.possible <- TRUE


  return(results)
}

# old ----
# .ldGaussianMethodMomentsResults <- function(jaspResults, options, variable, ready){
#   if(!ready || !options[['methodMoments']])
#     return()
#
#
#   if(is.null(jaspResults[['methodMoments']][['results']]$object)){
#     jaspResults[['methodMoments']][['results']] <- createJaspState()
#     jaspResults[['methodMoments']][['results']]$dependOn(c("variable", "simulateNow"))
#
#     results <- list()
#     results$par <- .computeObservedMoments(x = variable, max.moment = 2, about.mean = TRUE)
#     results$par[2] <- sqrt(results$par[2])
#     names(results$par) <- c("mean", "sd")
#
#     results$table <- c(mu = results$par[[1]],
#                        sigma = results$par[[2]],
#                        sigma2 = results$par[[2]]^2,
#                        tau = results$par[[2]],
#                        tau2 = 1/results$par[[2]]^2)
#     jaspResults[['methodMoments']][['results']]$object <- results
#   }
#
#   return()
# }
#
# .ldGaussianMethodUnbiasedResults <- function(jaspResults, options, variable, ready){
#   if(!ready || !options[['methodUnbiased']])
#     return()
#
#
#
#   if(is.null(jaspResults[['methodUnbiased']][['results']]$object)){
#     jaspResults[['methodUnbiased']][['results']] <- createJaspState()
#     jaspResults[['methodUnbiased']][['results']]$dependOn(c("variable", "simulateNow", "ciInterval"))
#
#     results <- list()
#     results$par <- c(mean = mean(variable), sd = .sdGaussianUnbiased(variable))
#     names(results$par) <- c("mean", "sd")
#
#     results$table <- c(mu = results$par[[1]],
#                        sigma = results$par[[2]],
#                        sigma2 = var(variable),
#                        tau = 1/results$par[[2]],
#                        tau2 = 1/var(variable))
#
#     if(options[['ciInterval']]){
#       res <- t.test(variable, conf.level = options[['ciIntervalInterval']])
#       resvar <- ci.GaussianVar(variable, conf.level = options[['ciIntervalInterval']])
#       ressd  <- ci.GaussianSD (variable, conf.level = options[['ciIntervalInterval']])
#
#       results$table <- c(results$table, mu.lower = res[['conf.int']][[1]], mu.upper = res[['conf.int']][[2]],
#                          sigma2.lower = resvar[1], sigma2.upper = resvar[2],
#                          sigma.lower  = ressd[1], sigma.upper = ressd[2],
#                          tau2.lower = 1/resvar[1], tau2.upper = 1/resvar[2],
#                          tau.lower = 1/ressd[1], tau.upper = 1/ressd[2])
#
#     }
#     jaspResults[['methodUnbiased']][['results']]$object <- results
#   }
#
#   return()
# }
#
# .ldFitAssessment <- function(methodContainer, options, variable, ready){
#   if(is.null(methodContainer[['fitAssessment']])){
#     methodContainer[['fitAssessment']] <- createJaspContainer(title = "Fit Assessment")
#     methodContainer[['fitAssessment']]$dependOn(c("variable", "simulateNow"))
#   }
#
#
#   estParameters <- methodContainer[['results']]$object[['par']]
#
#   .ldFillAssessmentTable(methodContainer, estParameters, options, variable, ready)
#
#
#   if(is.null(methodContainer[['fitAssessment']][['estPDF']]) && options$estPDF){
#     pdfplot <- createJaspPlot(title = "Histogram vs. Theoretical PDF")
#     pdfplot$dependOn(c("estPDF"))
#     pdfplot$position <- 2
#     methodContainer[['fitAssessment']][['estPDF']] <- pdfplot
#
#     if(ready)
#       .ldFillEstPDFPlot(pdfplot, estParameters, options, variable)
#   }
#
#   if(is.null(methodContainer[['fitAssessment']][['qqplot']]) && options$qqplot){
#     qqplot <- createJaspPlot(title = "Q-Q plot")
#     qqplot$dependOn(c("qqplot"))
#     qqplot$position <- 3
#     methodContainer[['fitAssessment']][['qqplot']] <- qqplot
#
#     if(ready)
#       .ldFillQQPlot(qqplot, estParameters, options, variable)
#   }
#
#   if(is.null(methodContainer[['fitAssessment']][['estCDF']]) && options$estCDF){
#     cdfplot <- createJaspPlot(title = "Empirical vs. Theoretical CDF")
#     cdfplot$dependOn(c("estCDF"))
#     cdfplot$position <- 4
#     methodContainer[['fitAssessment']][['estCDF']] <- cdfplot
#
#     if(ready)
#       .ldFillEstCDFPlot(cdfplot, estParameters, options, variable)
#   }
#
#   if(is.null(methodContainer[['fitAssessment']][['ppplot']]) && options$ppplot){
#     ppplot <- createJaspPlot(title = "P-P plot")
#     ppplot$dependOn(c("ppplot"))
#     ppplot$position <-5
#     methodContainer[['fitAssessment']][['ppplot']] <- ppplot
#
#     if(ready)
#       .ldFillPPPlot(ppplot, estParameters, options, variable)
#   }
#
#   return()
# }
#
#### Helper functions ----
# .sdGaussianUnbiased <- function(x){
#   # https://en.wikipedia.org/wiki/Unbiased_estimation_of_standard_deviation
#   x <- na.omit(x)
#   n <- length(x)
#   logSDBiased <- log(sd(x))
#
#   logCorrectionFactor <- 0.5*log(2) - 0.5*log(n-1) + lgamma(n/2) - lgamma((n-1)/2)
#
#   logSDUnbiased <- logSDBiased - logCorrectionFactor
#
#   return(exp(logSDUnbiased))
# }
#
#
# ci.GaussianVar <- function(x, conf.level = options[['ciIntervalInterval']]){
#   x <- na.omit(x)
#   df <- length(x) - 1
#   v <- var(x)
#
#   alpha <- 1-conf.level
#   perc <- c(1-alpha/2, alpha/2)
#   res <- v * df / qchisq(p = perc, df = df)
#
#   return(res)
# }
#
# ci.GaussianSD <- function(variable, conf.level = options[['ciIntervalInterval']]){
#   sqrt(ci.GaussianVar(variable, conf.level))
# }
