context("Discover Distributions - Skew normal")

options <- jaspTools::analysisOptions("LDnormalSkew")
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
options$variable <- "normalSkew"
set.seed(1)
# results <- jaspTools::runAnalysis("LDnormalSkew",
#                                   data.frame(normalSkew = sn::rsn(n = 100, xi = options[["xi"]], omega = options[["omega"]], alpha = options[["alpha"]])),
#                                   options, makeTests = TRUE)

dataset <- structure(list(normalSkew = structure(c(0.183643324222082, 1.59528080213779,
                                                   -0.820468384118015, 0.738324705129217, -0.305388387156356, 0.389843236411431,
                                                   -2.2146998871775, -0.0449336090152309, 0.943836210685299, 0.593901321217509,
                                                   0.782136300731067, -1.98935169586337, -0.0561287395290008, -1.47075238389927,
                                                   0.417941560199702, -0.102787727342996, -0.0538050405829051, -0.41499456329968,
                                                   -0.0593133967111857, 0.763175748457544, -0.253361680136508, 0.556663198673657,
                                                   -0.70749515696212, 0.768532924515416, 0.881107726454215, -0.612026393250771,
                                                   -1.12936309608079, 1.98039989850586, -1.04413462631653, -0.135054603880824,
                                                   -0.0392400027331692, 0.0280021587806661, 0.188792299514343, 1.46555486156289,
                                                   2.17261167036215, -0.709946430921815, -0.934097631644252, 0.291446235517463,
                                                   0.00110535163162413, -0.589520946188072, -0.135178615123832,
                                                   -1.52356680042976, 0.332950371213518, -0.304183923634301, 0.267098790772231,
                                                   1.20786780598317, 0.700213649514998, 0.558486425565304, -0.573265414236886,
                                                   -0.473400636439312, 0.0421158731442352, 0.158028772404075, 1.76728726937265,
                                                   0.910174229495227, 1.68217608051942, -0.461644730360566, -0.650696353310367,
                                                   -0.392807929441984, -0.279113302976559, -0.177330482269606, 1.34303882517041,
                                                   -0.179556530043387, 0.712666307051405, -0.0376341714670479, -0.324270272246319,
                                                   -0.588894486259664, -1.51839408178679, -1.53644982353759, -0.528279904445006,
                                                   -0.0568967778473925, 1.17658331201856, -0.463530401472386, -0.750819001193448,
                                                   0.0173956196932517, -1.64060553441858, -0.018559832714638, -0.929362147453702,
                                                   -1.07519229661568, -0.621266694796823, 1.86929062242358, -0.238647100913033,
                                                   0.886422651374936, 2.20610246454047, -1.42449465021281, 0.207538339232345,
                                                   0.105802367893711, -0.077152935356531, -0.0347260283112762, 2.07524500865228,
                                                   1.2079083983867, 0.983895570053379, -1.46725002909224, -0.158754604716016,
                                                   -0.766081999604665, -0.926109497377437, 0.402011779486338, 0.830373167981674,
                                                   -1.04798441280774, -1.01584746530465, -0.38107605110892),
                                                 family = "SN", parameters = c(0,1, 0, 0))),
                     class = "data.frame", row.names = c(NA, -100L))
results <- runAnalysis("LDnormalSkew", dataset, options)


test_that("Descriptives table results match", {
  table <- results[["results"]][["dataContainer"]][["collection"]][["dataContainer_summary"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(2.20610246454047, -0.000729160654835123, -0.0565127586881966,
                                      -2.2146998871775, -0.595147307953747, 0.620479403291881, 100,
                                      0.945120907503157, 0.893253529799591, "normalSkew"))
})

test_that("Estimated Parameters table results match", {
  table <- results[["results"]][["mleContainer"]][["collection"]][["mleContainer_estParametersTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(-0.000369367284250824, -29.0766213778385, "<unicode>", 14.8350950527173,
                                      29.07588264327, 0.940384420667224, 0.809582693968186, "<unicode>",
                                      0.0667368011508302, 1.07118614736626, -0.000479500992353865,
                                      -38.7515976630037, "<unicode>", 19.7713419571356, 38.750638661019
                                 ))
})

test_that("Empirical vs. Theoretical CDF plot matches", {
  plotName <- results[["results"]][["mleContainer"]][["collection"]][["mleContainer_mleFitAssessment"]][["collection"]][["mleContainer_mleFitAssessment_estCDF"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "empirical-vs-theoretical-cdf", dir="LDnormalSkew")
})

test_that("Histogram vs. Theoretical PDF plot matches", {
  plotName <- results[["results"]][["mleContainer"]][["collection"]][["mleContainer_mleFitAssessment"]][["collection"]][["mleContainer_mleFitAssessment_estPDF"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "histogram-vs-theoretical-pdf", dir="LDnormalSkew")
})

test_that("Fit Statistics table results match", {
  table <- results[["results"]][["mleContainer"]][["collection"]][["mleContainer_mleFitAssessment"]][["collection"]][["mleContainer_mleFitAssessment_fitStatisticsTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.367969485606479, 0.0918300063001897, "Kolmogorov-Smirnov", 0.621668969197197,
                                      0.0929222597572388, "Cram<unicode>r-von Mises", 0.719814750407416,
                                      0.526203868010313, "Anderson-Darling"))
})

test_that("P-P plot matches", {
  plotName <- results[["results"]][["mleContainer"]][["collection"]][["mleContainer_mleFitAssessment"]][["collection"]][["mleContainer_mleFitAssessment_ppplot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "p-p-plot", dir="LDnormalSkew")
})

test_that("Q-Q plot matches", {
  plotName <- results[["results"]][["mleContainer"]][["collection"]][["mleContainer_mleFitAssessment"]][["collection"]][["mleContainer_mleFitAssessment_qqplot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "q-q-plot", dir="LDnormalSkew")
})

test_that("Cumulative Probability Plot matches", {
  plotName <- results[["results"]][["plotCDF"]][["collection"]][["plotCDF_cdfPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "cumulative-probability-plot", dir="LDnormalSkew")
})

test_that("Density Plot matches", {
  plotName <- results[["results"]][["plotPDF"]][["collection"]][["plotPDF_pdfPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "density-plot", dir="LDnormalSkew")
})

test_that("Quantile Plot matches", {
  plotName <- results[["results"]][["plotQF"]][["collection"]][["plotQF_qfPlot"]][["data"]]
  testPlot <- results[["state"]][["figures"]][[plotName]][["obj"]]
  jaspTools::expect_equal_plots(testPlot, "quantile-plot", dir="LDnormalSkew")
})
