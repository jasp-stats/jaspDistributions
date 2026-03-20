testthat::context("Compare continuous distributions")

options <- jaspTools::analysisOptions("compareContinuousDistributions")
options$variable <- "contGamma"
options$distributions <- list(
  list(
    distribution    = "Normal",
    parameters      = "sigma",
    normalMu        = 0,
    normalMuFixed   = FALSE,
    normalSigma     = 1.4,
    normalSigmaFixed = TRUE
  ),
  list(
    distribution     = "Gamma",
    parameters       = "theta",
    gammaAlpha       = 2,
    gammaAlphaFixed  = FALSE,
    gammaTheta       = 1,
    gammaThetaFixed  = FALSE
  ),
  list(
    distribution = "StandardNormal"
  ),
  list(
    distribution           = "Exponential",
    parameters             = "lambda",
    exponentialLambda      = 1,
    exponentialLambdaFixed = FALSE
  )
)
options$comparisonTableOrderBy <- "bic"
options$outputLimitTo <- 2
options$goodnessOfFit <- TRUE
options$goodnessOfFitBootstrap <- TRUE
options$goodnessOfFitBootstrapSamples <- 100L
options$empiricalPlots <- TRUE
options$empiricalPlotsCi <- TRUE
options$empiricalPlotsCiLevel <- 0.95
set.seed(1)
{
  withr::local_options(jaspLegacyKind = FALSE)
  results <- jaspTools::runAnalysis("compareContinuousDistributions", "test.csv", options)
}


testthat::test_that("Main comparison table works", {
  testthat::local_edition(3)
  withr::local_options(width = 200)
  table <- results[["results"]][["comparisonTable"]][["data"]]
  table <- do.call(rbind, table)
  testthat::expect_snapshot(table)
})

testthat::test_that("Per distribution output works", {
  testthat::local_edition(3)
  table <- results[["results"]][["distributionResults1"]][["collection"]][["distributionResults1_parameterEstimates"]][["data"]]
  table <- do.call(rbind, table)
  testthat::expect_snapshot(table)

  plotName <- results[["results"]][["distributionResults1"]][["collection"]][["distributionResults1_empiricalPlots"]][["collection"]][["distributionResults1_empiricalPlots_hist"]][["data"]]
  plot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(test = plot, name = "hist-plot")

  plotName <- results[["results"]][["distributionResults1"]][["collection"]][["distributionResults1_empiricalPlots"]][["collection"]][["distributionResults1_empiricalPlots_ecdf"]][["data"]]
  plot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(test = plot, name = "ecdf-plot")

  plotName <- results[["results"]][["distributionResults1"]][["collection"]][["distributionResults1_empiricalPlots"]][["collection"]][["distributionResults1_empiricalPlots_pp"]][["data"]]
  plot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(test = plot, name = "pp-plot")

  plotName <- results[["results"]][["distributionResults1"]][["collection"]][["distributionResults1_empiricalPlots"]][["collection"]][["distributionResults1_empiricalPlots_qq"]][["data"]]
  plot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(test = plot, name = "qq-plot")

  # depends on bootstrapping - small differences possible
  testthat::skip_on_os(c("windows", "linux"))
  table <- results[["results"]][["distributionResults1"]][["collection"]][["distributionResults1_goodnessOfFit"]][["data"]]
  testthat::expect_snapshot(table)
})

