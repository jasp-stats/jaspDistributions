testthat::context("Compare continuous distributions")

options <- jaspTools::analysisOptions("compareContinuousDistributions")
options$variable <- "contGamma"
options$distributions <- list(
  list(
    distribution    = "Normal",
    parametrization = "sigma",
    parameters      = list(
      list(value = "mu",    mu = 0,      fixed = FALSE),
      list(value = "sigma", sigma = 1.4, fixed = TRUE)
    )
  ),
  list(
    distribution     = "Gamma",
    parametrization  = "theta",
    parameters = list(
      list(value = "alpha", alpha = 2, fixed = FALSE),
      list(value = "theta", theta = 1, fixed = FALSE)
    )
  ),
  list(
    distribution = "StandardNormal", parametrization = ""
  ),
  list(
    distribution     = "Exponential",
    parametrization = "lambda",
    parameters = list(
      list(value = "lambda", lambda = 1, fixed = FALSE)
    )
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

