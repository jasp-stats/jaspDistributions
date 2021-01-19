context("Discover Distributions - Mixture of two normal distributions")

options <- jaspTools::analysisOptions("LDmixtureNormalNormal")
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
options$sampleSize <- 1000
options$variable <- "mixnorm"
set.seed(1)
results <- jaspTools::runAnalysis("LDmixtureNormalNormal",
                                  data.frame(mixnorm = jaspDistributions:::rmixnormnorm(n=100, pi=0.5, mu1=-3, sigma1=1, mu2=3, sigma2=1)),
                                  options)


test_that("Descriptives table results match", {
  table <- results[["results"]][["dataContainer"]][["collection"]][["dataContainer_summary"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(5.40161776050478, 0.102420507069452, 1.46999168801633, -4.80495862889104,
                                      -2.93371503860019, 2.873661361786, 100, 3.10092106843918, 9.61571147268996,
                                      "mixnorm"))
})

test_that("Estimated Parameters table results match", {
  table <- results[["results"]][["mleContainer"]][["collection"]][["mleContainer_estParametersTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.479958842506049, 0.382028044993936, "<unicode>", 0.0499656107380433,
                                      0.577889640018162, -2.95855211503701, -3.2249419959426, "<unicode><unicode>",
                                      0.135915702026589, -2.69216223413141, 0.941209494326871, 0.752580452025413,
                                      "<unicode><unicode>", 0.0962410757490136, 1.12983853662833,
                                      2.92744821586415, 2.67524524062692, "<unicode><unicode>", 0.128677351842465,
                                      3.17965119110137, 0.92713779631031, 0.747825490680175, "<unicode><unicode>",
                                      0.0914875513246815, 1.10645010194044))
})

test_that("Empirical vs. Theoretical CDF plot matches", {
  plotName <- results[["results"]][["mleContainer"]][["collection"]][["mleContainer_mleFitAssessment"]][["collection"]][["mleContainer_mleFitAssessment_estCDF"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "empirical-vs-theoretical-cdf", dir="LDmixtureNormalNormal")
})

test_that("Histogram vs. Theoretical PDF plot matches", {
  plotName <- results[["results"]][["mleContainer"]][["collection"]][["mleContainer_mleFitAssessment"]][["collection"]][["mleContainer_mleFitAssessment_estPDF"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "histogram-vs-theoretical-pdf", dir="LDmixtureNormalNormal")
})

test_that("Fit Statistics table results match", {
  table <- results[["results"]][["mleContainer"]][["collection"]][["mleContainer_mleFitAssessment"]][["collection"]][["mleContainer_mleFitAssessment_fitStatisticsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.83237588193818, 0.06230471399249, "Kolmogorov-Smirnov", 0.861431524147999,
                                      0.0525996267277474, "Cram<unicode>r-von Mises", 0.828213765725574,
                                      0.4201235894528, "Anderson-Darling"))
})

test_that("P-P plot matches", {
  plotName <- results[["results"]][["mleContainer"]][["collection"]][["mleContainer_mleFitAssessment"]][["collection"]][["mleContainer_mleFitAssessment_ppplot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "p-p-plot", dir="LDmixtureNormalNormal")
})

test_that("Q-Q plot matches", {
  plotName <- results[["results"]][["mleContainer"]][["collection"]][["mleContainer_mleFitAssessment"]][["collection"]][["mleContainer_mleFitAssessment_qqplot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "q-q-plot", dir="LDmixtureNormalNormal")
})

test_that("Cumulative Probability Plot matches", {
  plotName <- results[["results"]][["plotCDF"]][["collection"]][["plotCDF_cdfPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "cumulative-probability-plot", dir="LDmixtureNormalNormal")
})

test_that("Density Plot matches", {
  plotName <- results[["results"]][["plotPDF"]][["collection"]][["plotPDF_pdfPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "density-plot", dir="LDmixtureNormalNormal")
})

test_that("Quantile Plot matches", {
  plotName <- results[["results"]][["plotQF"]][["collection"]][["plotQF_qfPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "quantile-plot", dir="LDmixtureNormalNormal")
})
