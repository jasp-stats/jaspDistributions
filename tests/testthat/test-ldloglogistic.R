context("Discover Distributions - Log-logistic")

options <- jaspTools::analysisOptions("LDlogLogistic")
options$parametrization <- "alphabeta"
options$par1 <- 2
options$par2 <- 3
options$andersonDarling <- TRUE
options$ciInterval <- TRUE
options$ciIntervalInterval <- 0.95
options$cramerVonMisses <- TRUE
options$estCDF <- TRUE
options$estPDF <- TRUE
options$highlightDensity <- TRUE
options$highlightProbability <- TRUE
options$histogram <- FALSE
options$kolmogorovSmirnov <- TRUE
options$methodMLE <- TRUE
options$newVariableName <- ""
options$outputEstimates <- TRUE
options$outputSE <- TRUE
options$parsSupportMoments <- TRUE
options$plotCDF <- TRUE
options$plotQF <- TRUE
options$ppplot <- TRUE
options$qqplot <- TRUE
options$qqPlotCi <- FALSE
options$qqPlotCiLevel <- 0.95
options$ppPlotCi <- FALSE
options$ppPlotCiLevel <- 0.95
options$sampleSize <- 1000
options$variable <- "loglogistic"
set.seed(1)
results <- jaspTools::runAnalysis("LDlogLogistic",
                                  data.frame(loglogistic = jaspDistributions:::rloglogis(n=100, alpha=options[["par1"]], beta=options[["par2"]])),
                                  options)


test_that("Descriptives table results match", {
  table <- results[["results"]][["dataContainer"]][["collection"]][["dataContainer_summary"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(9.93422849032264, 2.33457358449681, 1.96780491450667, 0.477066221678966,
                                      1.56298581660688, 2.97626169298452, 100, 1.29365949824446, 1.67355489739811,
                                      "loglogistic"))
})

test_that("Estimated Parameters table results match", {
  table <- results[["results"]][["mleContainer"]][["collection"]][["mleContainer_estParametersTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(2.06857907526432, 1.86971617909476, "<unicode>", 0.101462525708721,
                                      2.26744197143388, 3.55354945002815, 2.97571520189881, "<unicode>",
                                      0.294818809267527, 4.13138369815748))
})

test_that("Empirical vs. Theoretical CDF plot matches", {
  plotName <- results[["results"]][["mleContainer"]][["collection"]][["mleContainer_mleFitAssessment"]][["collection"]][["mleContainer_mleFitAssessment_estCDF"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "empirical-vs-theoretical-cdf", dir="LDlogLogistic")
})

test_that("Histogram vs. Theoretical PDF plot matches", {
  plotName <- results[["results"]][["mleContainer"]][["collection"]][["mleContainer_mleFitAssessment"]][["collection"]][["mleContainer_mleFitAssessment_estPDF"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "histogram-vs-theoretical-pdf", dir="LDlogLogistic")
})

test_that("Fit Statistics table results match", {
  table <- results[["results"]][["mleContainer"]][["collection"]][["mleContainer_mleFitAssessment"]][["collection"]][["mleContainer_mleFitAssessment_fitStatisticsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.944590834507008, 0.0526306225652022, "Kolmogorov-Smirnov", 0,
                                      0, 0, 0.295374163882449, 0.514790576613683, "Cram<unicode>r-von Mises",
                                      0, 0, 0, 0.720135115059263, 1.80075753129102, "Anderson-Darling"
                                 ))
})

test_that("P-P plot matches", {
  plotName <- results[["results"]][["mleContainer"]][["collection"]][["mleContainer_mleFitAssessment"]][["collection"]][["mleContainer_mleFitAssessment_ppplot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "p-p-plot", dir="LDlogLogistic")
})

test_that("Q-Q plot matches", {
  plotName <- results[["results"]][["mleContainer"]][["collection"]][["mleContainer_mleFitAssessment"]][["collection"]][["mleContainer_mleFitAssessment_qqplot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "q-q-plot", dir="LDlogLogistic")
})

test_that("Cumulative Probability Plot matches", {
  plotName <- results[["results"]][["plotCDF"]][["collection"]][["plotCDF_cdfPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "cumulative-probability-plot", dir="LDlogLogistic")
})

test_that("Density Plot matches", {
  plotName <- results[["results"]][["plotPDF"]][["collection"]][["plotPDF_pdfPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "density-plot", dir="LDlogLogistic")
})

test_that("Quantile Plot matches", {
  plotName <- results[["results"]][["plotQF"]][["collection"]][["plotQF_qfPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "quantile-plot", dir="LDlogLogistic")
})
