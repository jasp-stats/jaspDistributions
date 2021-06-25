context("Discover Distributions - Binomial")

options <- jaspTools::analysisOptions("LDbetaBinomial")
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
options$max <- 8
options$max_x <- 10
options$methodMLE <- TRUE
options$min <- 3
options$moments <- TRUE
options$newVariableName <- ""
options$outputEstimates <- TRUE
options$outputSE <- TRUE
options$parsSupportMoments <- TRUE
options$plotCMF <- TRUE
options$ppplot <- TRUE
options$qqplot <- TRUE
options$summary <- TRUE
options$variable <- "betaBinom"
set.seed(1)
results <- jaspTools::runAnalysis("LDbetaBinomial",
                                  data.frame(betaBinom = jaspDistributions:::rbetabinom(n=100, options[['size']], 5, 5)),
                                  options)


test_that("Empirical Cumulative Distribution plot matches", {
  plotName <- results[["results"]][["dataContainer"]][["collection"]][["dataContainer_ecdf"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "empirical-cumulative-distribution")
})

test_that("Bar plot matches", {
  plotName <- results[["results"]][["dataContainer"]][["collection"]][["dataContainer_histogram"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "bar-plot")
})

test_that("Observed Moments table results match", {
  table <- results[["results"]][["dataContainer"]][["collection"]][["dataContainer_moments"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(5.34, 1, 5.34, 4.4044, 2, 32.92))
})

test_that("Descriptives table results match", {
  table <- results[["results"]][["dataContainer"]][["collection"]][["dataContainer_summary"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(9, 5.34, 6, 1, 3.75, 7, 100, 2.10923893594085, 4.44888888888889,
                                      "betaBinom"))
})

test_that("Estimated Parameters table results match", {
  table <- results[["results"]][["mleContainer"]][["collection"]][["mleContainer_estParametersTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(5.87209399540591, 2.33000495472673, "<unicode>", 1.80722149417986,
                                      9.4141830360851, 5.12743927367975, 2.04396398591842, "<unicode>",
                                      1.57323058591045, 8.21091456144109))
})

test_that("Empirical vs. Theoretical CDF plot matches", {
  plotName <- results[["results"]][["mleContainer"]][["collection"]][["mleContainer_mleFitAssessment"]][["collection"]][["mleContainer_mleFitAssessment_estCDF"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "empirical-vs-theoretical-cdf")
})

test_that("Histogram vs. Theoretical PMF plot matches", {
  plotName <- results[["results"]][["mleContainer"]][["collection"]][["mleContainer_mleFitAssessment"]][["collection"]][["mleContainer_mleFitAssessment_estPMF"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "histogram-vs-theoretical-pmf")
})

test_that("Fit Statistics table results match", {
  table <- results[["results"]][["mleContainer"]][["collection"]][["mleContainer_mleFitAssessment"]][["collection"]][["mleContainer_mleFitAssessment_fitStatisticsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.0935052657651824, 13.5761084215139, "Chi-square"))
})

test_that("P-P plot matches", {
  plotName <- results[["results"]][["mleContainer"]][["collection"]][["mleContainer_mleFitAssessment"]][["collection"]][["mleContainer_mleFitAssessment_ppplot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "p-p-plot")
})

test_that("Q-Q plot matches", {
  plotName <- results[["results"]][["mleContainer"]][["collection"]][["mleContainer_mleFitAssessment"]][["collection"]][["mleContainer_mleFitAssessment_qqplot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "q-q-plot")
})

test_that("Cumulative Probability Plot matches", {
  plotName <- results[["results"]][["plotCMF"]][["collection"]][["plotCMF_cmfPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "cumulative-probability-plot")
})

test_that("Probability Mass Plot matches", {
  plotName <- results[["results"]][["plotPMF"]][["collection"]][["plotPMF_pmfPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "probability-mass-plot")
})
