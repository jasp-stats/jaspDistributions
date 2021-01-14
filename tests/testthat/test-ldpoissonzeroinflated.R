context("Discover Distributions - Zero-inflated Poisson")

options <- jaspTools::analysisOptions("LDpoissonZeroInflated")
options$chiSquare <- TRUE
options$ciInterval <- TRUE
options$ciIntervalInterval <- 0.95
options$ecdf <- TRUE
options$estCDF <- TRUE
options$estPMF <- TRUE
options$explanatoryText <- TRUE
options$highlightDensity <- TRUE
options$highlightProbability <- TRUE
options$histogram <- TRUE
options$max_x <- 8
options$methodMLE <- TRUE
options$min <- 2
options$moments <- TRUE
options$newVariableName <- ""
options$outputEstimates <- TRUE
options$outputSE <- TRUE
options$parsSupportMoments <- TRUE
options$plotCMF <- TRUE
options$ppplot <- TRUE
options$qqplot <- TRUE
options$summary <- TRUE
options$variable <- "ZiPois"
set.seed(1)
results <- jaspTools::runAnalysis("LDpoissonZeroInflated",
                                  data.frame(ZiPois = jaspDistributions:::rzipois(n = 100, prob = options[['prob']], lambda = options[['lambda']])),
                                  options)

test_that("Empirical Cumulative Distribution plot matches", {
  plotName <- results[["results"]][["dataContainer"]][["collection"]][["dataContainer_ecdf"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "empirical-cumulative-distribution", dir="LDpoissonZeroInflated")
})

test_that("Bar plot matches", {
  plotName <- results[["results"]][["dataContainer"]][["collection"]][["dataContainer_histogram"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "bar-plot", dir="LDpoissonZeroInflated")
})

test_that("Observed Moments table results match", {
  table <- results[["results"]][["dataContainer"]][["collection"]][["dataContainer_moments"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(2.17, 1, 2.17, 3.3611, 2, 8.07))
})

test_that("Descriptives table results match", {
  table <- results[["results"]][["dataContainer"]][["collection"]][["dataContainer_summary"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(7, 2.17, 2, 0, 0, 3, 100, 1.84256628240357, 3.3950505050505, "ZiPois"
                                 ))
})

test_that("Estimated Parameters table results match", {
  table <- results[["results"]][["mleContainer"]][["collection"]][["mleContainer_estParametersTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.248031473318565, 0.151993165594199, "<unicode>", 0.0490000369812426,
                                      0.344069781042931, 2.88575646810286, 2.46416578237765, "<unicode>",
                                      0.215101241171095, 3.30734715382807))
})

test_that("Empirical vs. Theoretical CDF plot matches", {
  plotName <- results[["results"]][["mleContainer"]][["collection"]][["mleContainer_mleFitAssessment"]][["collection"]][["mleContainer_mleFitAssessment_estCDF"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "empirical-vs-theoretical-cdf", dir="LDpoissonZeroInflated")
})

test_that("Histogram vs. Theoretical PMF plot matches", {
  plotName <- results[["results"]][["mleContainer"]][["collection"]][["mleContainer_mleFitAssessment"]][["collection"]][["mleContainer_mleFitAssessment_estPMF"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "histogram-vs-theoretical-pmf", dir="LDpoissonZeroInflated")
})

test_that("Fit Statistics table results match", {
  table <- results[["results"]][["mleContainer"]][["collection"]][["mleContainer_mleFitAssessment"]][["collection"]][["mleContainer_mleFitAssessment_fitStatisticsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.62221069905223, 5.30982187793331, "Chi-square"))
})

test_that("P-P plot matches", {
  plotName <- results[["results"]][["mleContainer"]][["collection"]][["mleContainer_mleFitAssessment"]][["collection"]][["mleContainer_mleFitAssessment_ppplot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "p-p-plot", dir="LDpoissonZeroInflated")
})

test_that("Q-Q plot matches", {
  plotName <- results[["results"]][["mleContainer"]][["collection"]][["mleContainer_mleFitAssessment"]][["collection"]][["mleContainer_mleFitAssessment_qqplot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "q-q-plot", dir="LDpoissonZeroInflated")
})

test_that("Cumulative Probability Plot matches", {
  plotName <- results[["results"]][["plotCMF"]][["collection"]][["plotCMF_cmfPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "cumulative-probability-plot", dir="LDpoissonZeroInflated")
})

test_that("Probability Mass Plot matches", {
  plotName <- results[["results"]][["plotPMF"]][["collection"]][["plotPMF_pmfPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "probability-mass-plot", dir="LDpoissonZeroInflated")
})
