context("Discover Distributions - Skew t")

options <- jaspTools::analysisOptions("LDtSkew")
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
options$variable <- "tSkew"
set.seed(1)
# results <- jaspTools::runAnalysis("LDtSkew",
#                                   data.frame(tSkew = sn::rst(n = 100, xi = options[["xi"]], omega = options[["omega"]], alpha = options[["alpha"]], nu = options[['df']])),
#                                   options, makeTests = TRUE)

dataset <- structure(list(tSkew = structure(c(0.166466101020332, 0.756172175570806,
                                              -1.32311205634697, 39.5500541995772, -0.258398488481512, 0.351175898432495,
                                              -3.83969705490781, -0.314573149613621, 0.486436645301382, 3.37654912366599,
                                              0.631562650873659, -4.49628203921757, -0.0252479580622532, -16.2096030494882,
                                              0.525065753326231, -0.085760002920956, -0.0674838591868019, -2.1054518148248,
                                              -0.071153119944488, 0.599497525874982, -0.176395994658089, 1.7518530266234,
                                              -18.5808271567366, 1.15009411120618, 0.619820991705097, -1.95027056368796,
                                              -1.24453272181207, 1.85417091939191, -0.879591181331059, -0.628555747471371,
                                              -0.0208466218023847, 0.569294561748724, 0.406096606646876, 2.59827098339106,
                                              2.08691838285492, -1.28980343834726, -4.83680948152579, 0.304220573605017,
                                              0.000636939499616895, -0.920487642860568, -1.62597102944069,
                                              -6.85058862835663, 0.30872851311932, -0.141842711515541, 0.381328256296774,
                                              7.64271383983639, 1.02430728202479, 0.452397897640655, -9.70532462086745,
                                              -0.680774833935516, 0.103531499099507, 0.14477158220437, 1.62749049905605,
                                              1.20669965811502, 0.891564601334524, -0.222472755771092, -0.543749780351781,
                                              -1.53548652496422, -0.266015868176243, -0.19632604854068, 2.4691124343649,
                                              -0.418935116023503, 1.35466817434142, -0.0497452526407398, -1.98009388203763,
                                              -0.341467262920325, -0.791901458135598, -1.15808642131038, -0.871842220858178,
                                              -0.0598596027166855, 3.62626901135741, -4.74742243899268, -0.421182496621616,
                                              0.0281494905477873, -1.95284613177263, -0.030595897148655, -0.713279265111321,
                                              -1.26947978627331, -0.870810956808085, 1.02732604721211, -0.280920315501317,
                                              0.884724383504778, 7.85612737536557, -1.31290171706486, 0.103832969477425,
                                              0.0872203968019154, -0.105166571749719, -0.049077904285436, 260.034019286389,
                                              0.697856625707604, 2.23827598180649, -2.02487980535134, -0.12621572093331,
                                              -0.639948684144935, -1.95119017114498, 2.77909693121603, 0.813734811782649,
                                              -3.04977230921607, -1.28389257269143, -1.87366820006087),
                                            family = "ST", parameters = c(0, 1, 0, 1))),
                     class = "data.frame", row.names = c(NA, -100L))
results <- runAnalysis("LDtSkew", dataset, options)

test_that("Descriptives table results match", {
  table <- results[["results"]][["dataContainer"]][["collection"]][["dataContainer_summary"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(260.034019286389, 2.46099686612255, -0.069318489565645, -18.5808271567366,
                                      -0.979887337473021, 0.622756406497238, 100, 26.5266651327452,
                                      703.663963064801, "tSkew"))
})

test_that("Estimated Parameters table results match", {
  table <- results[["results"]][["mleContainer"]][["collection"]][["mleContainer_estParametersTable"]][["data"]]
  jaspTools::expect_equal_tables(table,
                                 list(0.0075267265754242, -0.37126457192011, "<unicode>", 0.193264417858385,
                                      0.386318025070958, 0.775019476729775, 0.506624207772372, "<unicode>",
                                      0.136938877996979, 1.04341474568718, -0.139540003131136, -0.64910788184568,
                                      "<unicode>", 0.25998838893671, 0.370027875583407, 0.995513863080443,
                                      0.64001827539064, "df", 0.181378632716676, 1.35100945077025
                                 ))
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
                                 list(0.997230674722692, 0.0399619121929585, "Kolmogorov-Smirnov", 0.981318212834332,
                                      0.0284962035821812, "Cram<unicode>r-von Mises", 0.99280729787226,
                                      0.190487716577849, "Anderson-Darling"))
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
