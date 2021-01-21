context("Discover Distributions - Amoroso")

options <- jaspTools::analysisOptions("LDamoroso")
options[["alpha"]] <- 3
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
options$variable <- "amoroso"
set.seed(1)
results <- jaspTools::runAnalysis("LDamoroso",
                                  data.frame(amoroso = jaspDistributions:::ramoroso(n = 100, a = options[["a"]], theta = options[["theta"]], alpha = options[["alpha"]], beta = options[["beta"]])),
                                  options)

test_that("Descriptives table results match", {
  table <- results[["results"]][["dataContainer"]][["collection"]][["dataContainer_summary"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(7.11526104225213, 2.91599104494971, 2.72776768324661, 0.343938829435207,
                                      1.85908514447057, 3.81583005623678, 100, 1.42042966062756, 2.01762042079053,
                                      "amoroso"))
})

test_that("Estimated Parameters table results match", {
  table <- results[["results"]][["mleContainer"]][["collection"]][["mleContainer_estParametersTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.240803290783608, -0.112208459813637, "a", 0.180111345607244,
                                      0.593815041380853, 3.54003094456327, 1.57429229016312, "<unicode>",
                                      1.00294631427192, 5.50576959896341, 0.75104336642473, -0.192167505680985,
                                      "<unicode>", 0.481238879665974, 1.69425423853044, 2.34306497378787,
                                      0.7049020582066, "<unicode>", 0.835812764164488, 3.98122788936913
                                 ))
})

test_that("Empirical vs. Theoretical CDF plot matches", {
  plotName <- results[["results"]][["mleContainer"]][["collection"]][["mleContainer_mleFitAssessment"]][["collection"]][["mleContainer_mleFitAssessment_estCDF"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "empirical-vs-theoretical-cdf", dir="LDamoroso")
})

test_that("Histogram vs. Theoretical PDF plot matches", {
  plotName <- results[["results"]][["mleContainer"]][["collection"]][["mleContainer_mleFitAssessment"]][["collection"]][["mleContainer_mleFitAssessment_estPDF"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "histogram-vs-theoretical-pdf", dir="LDamoroso")
})

test_that("Fit Statistics table results match", {
  table <- results[["results"]][["mleContainer"]][["collection"]][["mleContainer_mleFitAssessment"]][["collection"]][["mleContainer_mleFitAssessment_fitStatisticsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.996348236696634, 0.0407506337143581, "Kolmogorov-Smirnov", 0.996424292800046,
                                      0.020739043517987, "Cram<unicode>r-von Mises", 0.999623186741392,
                                      0.128661088698408, "Anderson-Darling"))
})

test_that("P-P plot matches", {
  plotName <- results[["results"]][["mleContainer"]][["collection"]][["mleContainer_mleFitAssessment"]][["collection"]][["mleContainer_mleFitAssessment_ppplot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "p-p-plot", dir="LDamoroso")
})

test_that("Q-Q plot matches", {
  plotName <- results[["results"]][["mleContainer"]][["collection"]][["mleContainer_mleFitAssessment"]][["collection"]][["mleContainer_mleFitAssessment_qqplot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "q-q-plot", dir="LDamoroso")
})

test_that("Cumulative Probability Plot matches", {
  plotName <- results[["results"]][["plotCDF"]][["collection"]][["plotCDF_cdfPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "cumulative-probability-plot", dir="LDamoroso")
})

test_that("Density Plot matches", {
  plotName <- results[["results"]][["plotPDF"]][["collection"]][["plotPDF_pdfPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "density-plot", dir="LDamoroso")
})

test_that("Quantile Plot matches", {
  plotName <- results[["results"]][["plotQF"]][["collection"]][["plotQF_qfPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "quantile-plot", dir="LDamoroso")
})
