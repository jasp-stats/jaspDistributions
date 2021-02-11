context("Discover Distributions - Hypergeometric")

options <- jaspTools::analysisOptions("LDhypergeometric")
options$explanatoryText <- TRUE
options$highlightDensity <- TRUE
options$highlightProbability <- TRUE
options$histogram <- TRUE
options$moments <- TRUE
options$newVariableName <- ""
options$parsSupportMoments <- TRUE
options$summary <- TRUE
options$plotPMF <- TRUE
options$plotCMF <- TRUE
options$ecdf <- TRUE
options$summary <- TRUE
options$max <- 3
options$max_x <- 5
options$min <- 1
options$min_x <- 0
options$variable <- "hypergeometric"
set.seed(1)

results <- jaspTools::runAnalysis("LDhypergeometric",
                                  data.frame(hypergeometric = rhyper(nn = 100, m = options[['success']], n = options[['size']] - options[['success']], k = options[['draws']])),
                                  options)

test_that("Empirical Cumulative Distribution plot matches", {
  plotName <- results[["results"]][["dataContainer"]][["collection"]][["dataContainer_ecdf"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "empirical-cumulative-distribution", dir="LDhypergeometric")
})

test_that("Bar plot matches", {
  plotName <- results[["results"]][["dataContainer"]][["collection"]][["dataContainer_histogram"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "bar-plot", dir="LDhypergeometric")
})

test_that("Observed Moments table results match", {
  table <- results[["results"]][["dataContainer"]][["collection"]][["dataContainer_moments"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(2.52, 1, 2.52, 0.5296, 2, 6.88))
})

test_that("Descriptives table results match", {
  table <- results[["results"]][["dataContainer"]][["collection"]][["dataContainer_summary"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(4, 2.52, 3, 1, 2, 3, 100, 0.731402416559786, 0.534949494949495,
                                      "hypergeometric"))
})

test_that("Cumulative Probability Plot matches", {
  plotName <- results[["results"]][["plotCMF"]][["collection"]][["plotCMF_cmfPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "cumulative-probability-plot", dir="LDhypergeometric")
})

test_that("Probability Mass Plot matches", {
  plotName <- results[["results"]][["plotPMF"]][["collection"]][["plotPMF_pmfPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "probability-mass-plot", dir="LDhypergeometric")
})
