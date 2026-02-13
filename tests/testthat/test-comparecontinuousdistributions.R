testthat::context("Compare continuous distributions")
options(jaspLegacyRngKind=FALSE)

options <- jaspTools::analysisOptions("compareContinuousDistributions")
options$variable <- "contGamma"
options$distributionSpecification <- "Normal(mu=0, sigma=fixed(1.4))\nGamma(alpha=2, theta=1)\nStandardNormal()\nExponential(lambda=1)"
options$comparisonTableOrderBy <- "bic"
options$outputLimitTo <- 2
options$goodnessOfFit <- TRUE
options$goodnessOfFitBootstrap <- TRUE
options$goodnessOfFitBootstrapSamples <- 100L
options$empiricalPlots <- TRUE
options$empiricalPlotsCi <- TRUE
options$empiricalPlotsCiLevel <- 0.95
set.seed(1)
results <- jaspTools::runAnalysis("compareContinuousDistributions", "test.csv", options)


testthat::test_that("Main comparison table works", {
  testthat::local_edition(3)
  table <- results[["results"]][["comparisonTable"]][["data"]]
  table <- do.call(rbind, table)
  testthat::expect_snapshot(table)
})

testthat::test_that("Per distribution output works", {
  testthat::local_edition(3)
  table <- results[["results"]][["distribution1"]][["collection"]][["distribution1_parameterEstimates"]][["data"]]
  table <- do.call(rbind, table)
  testthat::expect_snapshot(table)

  plotName <- results[["results"]][["distribution1"]][["collection"]][["distribution1_empiricalPlots"]][["collection"]][["distribution1_empiricalPlots_hist"]][["data"]]
  plot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(test = plot, name = "hist-plot")

  plotName <- results[["results"]][["distribution1"]][["collection"]][["distribution1_empiricalPlots"]][["collection"]][["distribution1_empiricalPlots_ecdf"]][["data"]]
  plot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(test = plot, name = "ecdf-plot")

  plotName <- results[["results"]][["distribution1"]][["collection"]][["distribution1_empiricalPlots"]][["collection"]][["distribution1_empiricalPlots_pp"]][["data"]]
  plot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(test = plot, name = "pp-plot")

  plotName <- results[["results"]][["distribution1"]][["collection"]][["distribution1_empiricalPlots"]][["collection"]][["distribution1_empiricalPlots_qq"]][["data"]]
  plot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(test = plot, name = "qq-plot")

  # depends on bootstrapping - small differences possible
  testthat::skip_on_os(c("windows", "linux"))
  table <- results[["results"]][["distribution1"]][["collection"]][["distribution1_goodnessOfFit"]][["data"]]
  testthat::expect_snapshot(table)
})

options(jaspLegacyRngKind=TRUE)
