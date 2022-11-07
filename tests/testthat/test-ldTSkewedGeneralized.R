context("Discover Distributions - Skewed generalized t")

options <- jaspTools::analysisOptions("LDtSkewedGeneralized")
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
options$variable <- "tSkewedGeneralized"
set.seed(1)
# results <- jaspTools::runAnalysis("LDtSkewedGeneralized",
#                                   data.frame(tSkewedGeneralized = sgt::rsgt(n = 100,
#                                                                             mu = options[["mu"]],
#                                                                             sigma = options[["sigma"]],
#                                                                             lambda = options[["lambda"]],
#                                                                             p = options[["p"]],
#                                                                             q = options[["q"]],
#                                                                             mean.cent = FALSE, var.adj = FALSE)),
#                                   options, makeTests = TRUE)

dataset <- structure(list(tSkewedGeneralized = c(-0.530999504363556, -0.264550523911044,
                                                 0.147278508386009, 1.41379279437116, -0.74346081616329, 1.31858098287033,
                                                 1.94510050294935, 0.339638141315525, 0.267293623933724, -1.82007421721905,
                                                 -0.727044300854682, -0.848279724161682, 0.403322782970501, -0.238282120510073,
                                                 0.64105367162361, -0.00460156454646508, 0.48342699629454, 5.48994341651525,
                                                 -0.247148788085101, 0.666997011158828, 1.75961775404142, -0.704108882684084,
                                                 0.318348097827974, -1.13006786861688, -0.526044867193596, -0.233920524101726,
                                                 -4.23362471405238, -0.242014737290538, 1.09816799945679, -0.336939790241051,
                                                 -0.0358628091517533, 0.20320121287073, -0.0129184637272747, -0.806053871379784,
                                                 0.866240658648983, 0.357857985514134, 0.727855292160052, -1.26343780425603,
                                                 0.500291154183374, -0.180312774455815, 0.837111551631799, 0.307731866396145,
                                                 0.686315593483786, 0.106674439358563, 0.0595444391277278, 0.709613732228063,
                                                 -3.15772647035729, -0.0455871656108646, 0.524702655395383, 0.417745098190272,
                                                 -0.0448056628718868, 1.04477994994355, -0.124765674193871, -0.593540337049658,
                                                 -1.67515067657215, -1.33829396411252, -0.395097327892868, 0.0372944353096321,
                                                 0.342486040544603, -0.189661472111422, 1.46401184418907, -0.453208208849788,
                                                 -0.0821442946898215, -0.355795859681975, 0.316492671404112, -0.553049790787438,
                                                 -0.0429490611187619, 0.629312981738016, -1.49681898912917, 1.13611792133336,
                                                 -0.339942724693089, 0.924592444434426, -0.322151776558366, -0.352500047505187,
                                                 -0.0473505023318533, 1.26462659147451, 1.06398938788051, -0.225547930074685,
                                                 0.666564599224363, 2.36819055095534, -0.131811383687545, 0.46955253272672,
                                                 -0.204136118648377, -0.372775900061905, 0.599490331271819, -0.739562111989198,
                                                 0.465802969953763, -1.15715438049187, -0.591369429950265, -1.01801639528437,
                                                 -0.609971018601087, -1.87287548793547, 0.296850288151095, 1.14272313474796,
                                                 0.672119459395738, 0.73956627191343, -0.0898111261822395, -0.18281215529423,
                                                 0.793822744859701, 0.214646756637512)), class = "data.frame", row.names = c(NA, -100L))
results <- runAnalysis("LDtSkewedGeneralized", dataset, options)


test_that("Descriptives table results match", {
  table <- results[["results"]][["dataContainer"]][["collection"]][["dataContainer_summary"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(5.48994341651525, 0.0531434975970818, -0.024390636439514, -4.23362471405238,
                                      -0.378356257019646, 0.632248154209414, 100, 1.09955226496343,
                                      1.20901518338622, "tSkewedGeneralized"))
})

test_that("Estimated Parameters table results match", {
  table <- results[["results"]][["mleContainer"]][["collection"]][["mleContainer_estParametersTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.134509937065477, -0.204173378194206, "<unicode>", 0.172800785081345,
                                      0.473193252325159, 0.932984744870689, 0.693529016965292, "<unicode>",
                                      0.122173534715022, 1.17244047277609, -0.0513410108552863, -0.347885138840433,
                                      "<unicode>", 0.151300804670009, 0.24520311712986, 3.39529248759638,
                                      -0.641651906406987, "p", 2.05970335467706, 7.43223688159975,
                                      0.636658364848752, -0.460712506294413, "q", 0.559893385694373,
                                      1.73402923599192))
})

test_that("Empirical vs. Theoretical CDF plot matches", {
  plotName <- results[["results"]][["mleContainer"]][["collection"]][["mleContainer_mleFitAssessment"]][["collection"]][["mleContainer_mleFitAssessment_estCDF"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "empirical-vs-theoretical-cdf")
})

test_that("Histogram vs. Theoretical PDF plot matches", {
  plotName <- results[["results"]][["mleContainer"]][["collection"]][["mleContainer_mleFitAssessment"]][["collection"]][["mleContainer_mleFitAssessment_estPDF"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "histogram-vs-theoretical-pdf")
})

test_that("Fit Statistics table results match", {
  table <- results[["results"]][["mleContainer"]][["collection"]][["mleContainer_mleFitAssessment"]][["collection"]][["mleContainer_mleFitAssessment_fitStatisticsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.799738252547794, 0.0644983595337972, "Kolmogorov-Smirnov", 0.956660678378229,
                                      0.0352474047421152, "Cram<unicode>r-von Mises", 0.995606400069538,
                                      0.17613070186222, "Anderson-Darling"))
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
  plotName <- results[["results"]][["plotCDF"]][["collection"]][["plotCDF_cdfPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "cumulative-probability-plot")
})

test_that("Density Plot matches", {
  plotName <- results[["results"]][["plotPDF"]][["collection"]][["plotPDF_pdfPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "density-plot")
})

test_that("Quantile Plot matches", {
  plotName <- results[["results"]][["plotQF"]][["collection"]][["plotQF_qfPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "quantile-plot")
})
