context("Discover Distributions - Zero-inflated negative Binomial")

options <- jaspTools::analysisOptions("LDnegbinomialZeroInflated")
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
options$methodMLE <- TRUE
options$min <- 2
options$moments <- TRUE
options$newVariableName <- ""
options$outputEstimates <- TRUE
options$outputSE <- TRUE
options$par <- 0.5
options$parsSupportMoments <- TRUE
options$plotCMF <- TRUE
options$ppplot <- TRUE
options$qqplot <- TRUE
options$summary <- TRUE
options$variable <- "zinb"
set.seed(1)
results <- jaspTools::runAnalysis("LDnegbinomialZeroInflated",
                                  data.frame(zinb = jaspDistributions:::rzinbinom(n = 100, pi = options[['pi']], size = options[['size']], mu = 5)),
                                  options)

test_that("Empirical Cumulative Distribution plot matches", {
  plotName <- results[["results"]][["dataContainer"]][["collection"]][["dataContainer_ecdf"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "empirical-cumulative-distribution", dir="LDnegbinomialZeroInflated")
})

test_that("Bar plot matches", {
  plotName <- results[["results"]][["dataContainer"]][["collection"]][["dataContainer_histogram"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "bar-plot", dir="LDnegbinomialZeroInflated")
})

test_that("Observed Moments table results match", {
  table <- results[["results"]][["dataContainer"]][["collection"]][["dataContainer_moments"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(3.59, 1, 3.59, 14.1819, 2, 27.07))
})

test_that("Descriptives table results match", {
  table <- results[["results"]][["dataContainer"]][["collection"]][["dataContainer_summary"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(17, 3.59, 3, 0, 0, 5.25, 100, 3.78485818956953, 14.3251515151515,
                                      "zinb"))
})

test_that("Estimated Parameters table results match", {
  table <- results[["results"]][["mleContainer"]][["collection"]][["mleContainer_estParametersTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.217029435842974, 0.105171451993197, "<unicode>", 0.0570714486246166,
                                      0.32888741969275, 2.29678654865155, 0.703782411047027, "k",
                                      0.812772147942481, 3.88979068625607, 0.333741786281988, 0.188494403467786,
                                      "p", 0.0741071692948929, 0.478989169096191))
})

test_that("Empirical vs. Theoretical CDF plot matches", {
  plotName <- results[["results"]][["mleContainer"]][["collection"]][["mleContainer_mleFitAssessment"]][["collection"]][["mleContainer_mleFitAssessment_estCDF"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "empirical-vs-theoretical-cdf", dir="LDnegbinomialZeroInflated")
})

test_that("Histogram vs. Theoretical PMF plot matches", {
  plotName <- results[["results"]][["mleContainer"]][["collection"]][["mleContainer_mleFitAssessment"]][["collection"]][["mleContainer_mleFitAssessment_estPMF"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "histogram-vs-theoretical-pmf", dir="LDnegbinomialZeroInflated")
})

test_that("Fit Statistics table results match", {
  table <- results[["results"]][["mleContainer"]][["collection"]][["mleContainer_mleFitAssessment"]][["collection"]][["mleContainer_mleFitAssessment_fitStatisticsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.700143851291686, 10.8196301014598, "Chi-square"))
})

test_that("P-P plot matches", {
  plotName <- results[["results"]][["mleContainer"]][["collection"]][["mleContainer_mleFitAssessment"]][["collection"]][["mleContainer_mleFitAssessment_ppplot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "p-p-plot", dir="LDnegbinomialZeroInflated")
})

test_that("Q-Q plot matches", {
  plotName <- results[["results"]][["mleContainer"]][["collection"]][["mleContainer_mleFitAssessment"]][["collection"]][["mleContainer_mleFitAssessment_qqplot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "q-q-plot", dir="LDnegbinomialZeroInflated")
})

test_that("Cumulative Probability Plot matches", {
  plotName <- results[["results"]][["plotCMF"]][["collection"]][["plotCMF_cmfPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "cumulative-probability-plot", dir="LDnegbinomialZeroInflated")
})

test_that("Probability Mass Plot matches", {
  plotName <- results[["results"]][["plotPMF"]][["collection"]][["plotPMF_pmfPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "probability-mass-plot", dir="LDnegbinomialZeroInflated")
})
