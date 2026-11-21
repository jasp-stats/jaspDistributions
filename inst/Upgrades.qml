import QtQuick
import JASP.Module

Upgrades
{
	Upgrade
	{
		functionName:	"compareContinuousDistributions"
		fromVersion:	"0.96.5"
		toVersion:		"0.97.2"

		// The distribution specification changed shape over time. We normalize every old shape to the
		// current one, in which each specification looks like:
		//   { distribution: "Gamma", value: ["#"], parametrization: "theta",
		//     parameters: [ { value: ["alpha"], alpha: 2, fixed: false }, { value: ["theta"], theta: 1, fixed: false } ] }
		// i.e. the specification has a "value" (a one-element array holding the row id, e.g. ["#"], ["#2"], ...),
		// and each parameter row carries "value" (a one-element array with the parameter name), a key named after
		// that parameter (its number) and "fixed".
		//
		// Two old shapes are handled:
		//  1. Flat: parameters were distribution-prefixed options inside the specification (e.g. normalMu,
		//     normalMuFixed, ...) and the parametrization choice was stored under "parameters".
		//  2. Nested but with the parameter name stored as an array (e.g. "value": ["alpha"]) and an extra
		//     specification-level "value": [""]. Because that specification-level key was identical ("") for
		//     every distribution, the distributions collided in the list and all but one were dropped.
		//
		// This restructuring cannot be done with ChangeRename, so we rebuild the whole "distributions" array.
		ChangeJS
		{
			name: "distributions"
			jsFunction: function(options)
			{
				// Parameter names per distribution. Multi-parametrization distributions map each
				// parametrization (the old "parameters" value) to its ordered list of parameter names;
				// single-parametrization distributions just list their parameter names. Used for the flat shape.
				var parametersByDistribution = {
					"Normal":						{ "sigma": ["mu", "sigma"], "sigma2": ["mu", "sigma2"], "tau": ["mu", "tau"] },
					"StandardNormal":				[],
					"StandardT":					["nu"],
					"StudentT":						["nu", "mu", "sigma"],
					"NoncentralT":					["nu", "kappa"],
					"NoncentralStudentT":			["nu", "kappa", "mu", "sigma"],
					"Cauchy":						["mu", "sigma"],
					"Gumbel":						["mu", "beta"],
					"Laplace":						["mu", "beta"],
					"Logistic":						["mu", "sigma"],
					"SkewedGeneralizedT":			["mu", "sigma", "lambda", "p", "q"],
					"SymmetricGeneralizedNormal":	["mu", "alpha", "beta"],
					"SkewNormal":					["xi", "omega", "alpha"],
					"SkewCauchy":					["xi", "omega", "alpha"],
					"SkewT":						["xi", "omega", "alpha", "nu"],
					"Beta":							["alpha", "beta"],
					"BetaPrime":					["alpha", "beta"],
					"CentralF":						["nu1", "nu2"],
					"NoncentralF":					["nu1", "nu2", "kappa"],
					"ChiSquared":					["nu"],
					"NoncentralChiSquared":			["nu", "kappa"],
					"Exponential":					{ "lambda": ["lambda"], "beta": ["beta"] },
					"Gamma":						{ "theta": ["alpha", "theta"], "lambda": ["alpha", "lambda"], "mu": ["alpha", "mu"] },
					"InverseGamma":					{ "theta": ["alpha", "theta"], "lambda": ["alpha", "lambda"], "mu": ["alpha", "mu"] },
					"Gompertz":						["eta", "beta"],
					"LogLogistic":					{ "mu": ["mu", "sigma"], "alpha": ["alpha", "beta"] },
					"LogNormal":					["mu", "sigma"],
					"Wald":							{ "mu": ["mu", "lambda"], "nu": ["nu", "alpha", "sigma"] },
					"Weibull":						["shape", "scale"],
					"Amoroso":						["a", "theta", "alpha", "beta"],
					"StretchedBeta":				["alpha", "beta", "min", "max"],
					"Frechet":						["alpha", "sigma", "theta"],
					"Pareto":						["alpha", "beta"],
					"Triangular":					["a", "b", "c"],
					"Uniform":						["min", "max"],
					"ShiftedExponential":			{ "lambda": ["lambda", "shift"], "beta": ["beta", "shift"] },
					"ShiftedLogNormal":				["mu", "sigma", "shift"],
					"ShiftedGamma":					{ "theta": ["alpha", "theta", "shift"], "lambda": ["alpha", "lambda", "shift"], "mu": ["alpha", "mu", "shift"] },
					"ShiftedInverseGamma":			{ "theta": ["alpha", "theta", "shift"], "lambda": ["alpha", "lambda", "shift"], "mu": ["alpha", "mu", "shift"] },
					"ShiftedLogLogistic":			{ "mu": ["mu", "sigma", "shift"], "alpha": ["alpha", "beta", "shift"] },
					"ShiftedWald":					{ "mu": ["mu", "lambda", "shift"], "nu": ["nu", "alpha", "sigma", "shift"] },
					"ShiftedWeibull":				["shape", "scale", "shift"]
				};

				function lowerFirst(s) { return s.charAt(0).toLowerCase() + s.slice(1); }
				function upperFirst(s) { return s.charAt(0).toUpperCase() + s.slice(1); }

				// Some old controls stored a name as a single-element array (e.g. ["alpha"]) instead of a string.
				function toName(v)
				{
					if (Array.isArray(v))	return v.length > 0 ? v[0] : "";
					return v === undefined || v === null ? "" : v;
				}

				var distributions = options["distributions"] || [];

				return distributions.map(function(specification, specIndex)
				{
					var distribution		= toName(specification["distribution"]);
					var newSpecification	= { "distribution": distribution, "parametrization": "", "parameters": [] };

					// The specification-level "value" is the (auto-generated) row id and must be an array.
					// Keep the old one if it already was a real, non-empty id (old flat shape had "#", "#2", ...);
					// otherwise regenerate a unique one as JASP does for unnamed rows (this fixes the old nested
					// shape where every specification-level "value" was [""] and the distributions collided).
					var oldValue	= specification["value"];
					var hasValue	= (Array.isArray(oldValue) && oldValue.length > 0 && oldValue[0] !== "")
									|| (typeof oldValue === "string" && oldValue !== "");
					newSpecification["value"] = hasValue
						? (Array.isArray(oldValue) ? oldValue : [oldValue])
						: [ specIndex === 0 ? "#" : "#" + (specIndex + 1) ];

					if (!distribution)
						return newSpecification;	// empty row

					// Shape 2: already nested (parameters is a list). Just normalize each row and drop the
					// specification-level "value" key that caused the distributions to collide.
					if (Array.isArray(specification["parameters"]))
					{
						newSpecification["parametrization"] = toName(specification["parametrization"]);
						newSpecification["parameters"] = specification["parameters"].map(function(row)
						{
							var parameterName	= toName(row["value"]);
							var newRow			= { "value": [parameterName] };
							if (parameterName !== "")
								newRow[parameterName] = row[parameterName];
							newRow["fixed"] = (row["fixed"] === undefined ? false : row["fixed"]);
							return newRow;
						});
						return newSpecification;
					}

					// Shape 1: flat. Rebuild the parameter list from the distribution-prefixed options.
					if (!parametersByDistribution.hasOwnProperty(distribution))
						return newSpecification;

					var entry = parametersByDistribution[distribution];
					var parameterNames;

					if (Array.isArray(entry))
					{
						parameterNames = entry;	// single parametrization
					}
					else
					{
						// multi-parametrization: the old parametrization choice was stored under "parameters"
						var parametrization = toName(specification["parameters"]);
						newSpecification["parametrization"] = parametrization;
						parameterNames = entry[parametrization] || [];
					}

					var prefix = lowerFirst(distribution);

					newSpecification["parameters"] = parameterNames.map(function(parameterName)
					{
						var valueKey	= prefix + upperFirst(parameterName);	// e.g. "normalMu"
						var fixedKey	= valueKey + "Fixed";					// e.g. "normalMuFixed"

						var newRow = { "value": [parameterName] };
						newRow[parameterName] = specification[valueKey];

						if (specification.hasOwnProperty(fixedKey))
							newRow["fixed"] = specification[fixedKey];
						else
							// the only parameters without an explicit "fixed" option were the bounds of the stretched beta, which were always fixed
							newRow["fixed"] = (distribution === "StretchedBeta" && (parameterName === "min" || parameterName === "max"));

						return newRow;
					});

					return newSpecification;
				});
			}
		}
	}

	// ---------------------------------------------------------------------------------------------------------------
	// The separate analyses of continuous distributions (LDgaussianunivariate, LDgamma, ...) were replaced by the single
	// ContinuousDistribution analysis ("Single Distribution" in the menu). The options of the old analyses are translated
	// to the options of the new one. The mixture analyses (LDmixtureNormalNormal, LDmixtureNormalUniform) have no counterpart
	// and remain separate analyses.
	//
	// The upgrade is an inline component, of which there is an instance for each old analysis: the function name must be a
	// literal of each instance, because an Upgrade registers itself before bindings (e.g., of a Repeater) are evaluated.
	//
	// Every new option is first created by copying an option that all the old analyses have ("explanatoryText", or
	// "variable" for the variable options, whose .meta is then copied as well) and then set by a ChangeJS. The ChangeJS
	// functions read the old options, which are still there; options that might be missing in old files are skipped
	// (by a condition), so that they get their default value in the new analysis.
	// ---------------------------------------------------------------------------------------------------------------

	component ContinuousDistributionUpgrade: Upgrade
	{
		id:					step

		newFunctionName:	"ContinuousDistribution"
		fromVersion:		"0.97.2"
		toVersion:			"0.97.3"
		msg:				qsTr("This analysis was made with an older version of JASP and was converted to the Single Distribution analysis. Options without an equivalent were dropped, so the output may differ.")

		// the value of an old option, or a default if the option is missing
		function oldOption(options, name, defaultValue)
		{
			var value = options[name];
			return value === undefined || value === null ? defaultValue : value;
		}

		function hasOldOption(options, name) { return options[name] !== undefined && options[name] !== null; }

		// the name of the (single) variable of the old analyses
		function oldVariableName(options)
		{
			var variable = options["variable"];
			if (variable === undefined || variable === null)	return "";
			if (typeof variable === "string")					return variable;
			var value = variable["value"];
			if (Array.isArray(value))							return value.length > 0 ? value[0] : "";
			return value === undefined || value === null ? "" : value;
		}

		// a variable option of the new analysis (a DropDown), with the variable of the old analysis or empty
		function variableOption(options, keepVariable)
		{
			var name = keepVariable ? oldVariableName(options) : "";
			var variable = options["variable"];
			var types = (name !== "" && variable && Array.isArray(variable["types"])) ? variable["types"].slice(0, 1) : [];
			return { "value": name, "types": types };
		}

		// the old analyses fitted the distribution if maximum likelihood or any assessment of the fit was requested
		function oldAssessesFit(options)
		{
			return ["kolmogorovSmirnov", "cramerVonMisses", "andersonDarling", "lillienfors", "shapiroWilk", "shapiroFrancia",
					"estPDF", "estCDF", "qqplot", "ppplot"].some(function(name) { return options[name] === true; });
		}

		function oldFitsDistribution(options)
		{
			return oldVariableName(options) !== "" && (oldOption(options, "methodMLE", false) || oldAssessesFit(options));
		}

		// the distribution specification of the new analysis: rows are [name, value] or [name, value, fixed]
		function makeSpecification(distribution, parametrization, rows)
		{
			return {
				"distribution":		distribution,
				"parametrization":	parametrization,
				"parameters":		rows.map(function(row)
				{
					var parameter = { "value": [row[0]] };
					parameter[row[0]] = row[1];
					parameter["fixed"] = row.length > 2 ? row[2] : false;
					return parameter;
				})
			};
		}

		// the parameters of the old analyses, translated to the distributions and parametrizations of DistributionS7
		function oldSpecification(analysis, options)
		{
			function o(name, defaultValue) { return oldOption(options, name, defaultValue); }

			switch (analysis)
			{
			case "LDgaussianunivariate":
			{
				var parametrization	= o("parametrization", "sigma2");
				var second			= o("varValue", 1);
				// the square root of the precision is not available anymore: it becomes the precision
				if (parametrization === "kappa")
				{
					parametrization	= "tau";
					second			= second * second;
				}
				return makeSpecification("Normal", parametrization, [["mu", o("mu", 0)], [parametrization, second]]);
			}
			case "LDnormalSkew":			return makeSpecification("SkewNormal", "", [["xi", o("xi", 0)], ["omega", o("omega", 1)], ["alpha", o("alpha", 0)]]);
			case "LDnormalGeneralized":		return makeSpecification("SymmetricGeneralizedNormal", "", [["mu", o("mu", 0)], ["alpha", o("alpha", 1)], ["beta", o("beta", 2)]]);
			case "LDcauchy":				return makeSpecification("Cauchy", "", [["mu", o("mu", 0)], ["sigma", o("sigma", 1)]]);
			case "LDtStudent":				return makeSpecification("StudentT", "", [["nu", o("df", 1)], ["mu", o("location", 0)], ["sigma", o("scale", 1)]]);
			case "LDtNoncentral":			return makeSpecification("NoncentralT", "", [["nu", o("df", 1)], ["kappa", o("ncp", 0)]]);
			case "LDtSkew":					return makeSpecification("SkewT", "", [["xi", o("xi", 0)], ["omega", o("omega", 1)], ["alpha", o("alpha", 0)], ["nu", o("df", 1)]]);
			case "LDtSkewedGeneralized":	return makeSpecification("SkewedGeneralizedT", "", [["mu", o("mu", 0)], ["sigma", o("sigma", 1)], ["lambda", o("lambda", 0)], ["p", o("p", 2)], ["q", o("q", 1)]]);
			// without noncentrality, the central distribution (the noncentrality would be estimated on the boundary of its support)
			case "LDf":
				return o("ncp", 0) === 0
					? makeSpecification("CentralF",		"", [["nu1", o("df1", 5)], ["nu2", o("df2", 2)]])
					: makeSpecification("NoncentralF",	"", [["nu1", o("df1", 5)], ["nu2", o("df2", 2)], ["kappa", o("ncp", 0)]]);
			case "LDchisq":
				return o("ncp", 0) === 0
					? makeSpecification("ChiSquared",			"", [["nu", o("df", 5)]])
					: makeSpecification("NoncentralChiSquared",	"", [["nu", o("df", 5)], ["kappa", o("ncp", 0)]]);
			case "LDuniform":				return makeSpecification("Uniform", "", [["min", o("lowerBoundPar", 0)], ["max", o("upperBoundPar", 1)]]);
			case "LDbeta":					return makeSpecification("Beta", "", [["alpha", o("alpha", 1)], ["beta", o("beta", 1)]]);
			// the bounds were never estimated
			case "LDbetaStretched":			return makeSpecification("StretchedBeta", "", [["alpha", o("alpha", 1)], ["beta", o("beta", 1)], ["min", o("lowerBoundPar", 0), true], ["max", o("upperBoundPar", 1), true]]);
			case "LDbetaPrime":				return makeSpecification("BetaPrime", "", [["alpha", o("alpha", 1)], ["beta", o("beta", 1)]]);
			// the old "mean" is the shape divided by the rate, as in DistributionS7 (for the inverse gamma, the mean of 1/x)
			case "LDgamma":
			case "LDgammaInverse":
			{
				var gammaParameter	= { "scale": "theta", "rate": "lambda", "mean": "mu" }[o("parametrization", "scale")] || "theta";
				return makeSpecification(analysis === "LDgamma" ? "Gamma" : "InverseGamma", gammaParameter, [["alpha", o("shape", 1)], [gammaParameter, o("par2", 1)]]);
			}
			case "LDexponential":
			{
				var exponentialParameter = o("parametrization", "rate") === "scale" ? "beta" : "lambda";
				return makeSpecification("Exponential", exponentialParameter, [[exponentialParameter, o("par", 1)]]);
			}
			case "LDlaplace":				return makeSpecification("Laplace", "", [["mu", o("mu", 0)], ["beta", o("beta", 1)]]);
			case "LDlognormal":				return makeSpecification("LogNormal", "", [["mu", o("mu", 0)], ["sigma", o("sigma", 1)]]);
			case "LDlogistic":				return makeSpecification("Logistic", "", [["mu", o("mu", 0)], ["sigma", o("sigma", 1)]]);
			case "LDlogLogistic":
				return o("parametrization", "musigma") === "alphabeta"
					? makeSpecification("LogLogistic", "alpha",	[["alpha", o("par1", 1)], ["beta", o("par2", 1)]])
					: makeSpecification("LogLogistic", "mu",	[["mu", o("par1", 0)], ["sigma", o("par2", 1)]]);
			case "LDpareto":				return makeSpecification("Pareto", "", [["alpha", o("shape", 1)], ["beta", o("scale", 1)]]);
			case "LDamoroso":				return makeSpecification("Amoroso", "", [["a", o("a", 0)], ["theta", o("theta", 1)], ["alpha", o("alpha", 1)], ["beta", o("beta", 1)]]);
			case "LDfrechet":				return makeSpecification("Frechet", "", [["alpha", o("shape", 1)], ["sigma", o("scale", 1)], ["theta", o("location", 0)]]);
			case "LDgumbel":				return makeSpecification("Gumbel", "", [["mu", o("mu", 0)], ["beta", o("beta", 1)]]);
			case "LDgompertz":				return makeSpecification("Gompertz", "", [["eta", o("shape", 1)], ["beta", o("scale", 1)]]);
			case "LDtriangular":			return makeSpecification("Triangular", "", [["a", o("a", -1)], ["b", o("b", 1)], ["c", o("c", 0)]]);
			case "LDweibull":				return makeSpecification("Weibull", "", [["shape", o("shape", 1)], ["scale", o("scale", 1)]]);
			// the old parametrizations with two of the drift rate, threshold, and noise had the third one fixed at 1
			case "LDwald":
				switch (o("parametrization", "mulambda"))
				{
				case "alphanu":		return makeSpecification("Wald", "nu", [["nu", o("par2", 1)], ["alpha", o("par1", 1)], ["sigma", 1, true]]);
				case "alphasigma":	return makeSpecification("Wald", "nu", [["nu", 1, true], ["alpha", o("par1", 1)], ["sigma", o("par2", 1)]]);
				case "nusigma":		return makeSpecification("Wald", "nu", [["nu", o("par1", 1)], ["alpha", 1, true], ["sigma", o("par2", 1)]]);
				default:			return makeSpecification("Wald", "mu", [["mu", o("par1", 1)], ["lambda", o("par2", 1)]]);
				}
			}

			return makeSpecification("", "", []);
		}

		// rows of a ComponentsList without source are identified by "#", "#2", ...
		function componentsListRows(rows)
		{
			return rows.map(function(row, index)
			{
				row["value"] = [index === 0 ? "#" : "#" + (index + 1)];
				return row;
			});
		}

		// the old analyses highlighted the density at the bounds of an interval, and the probability of that interval
		function oldHighlightInterval(options)
		{
			switch (oldOption(options, "highlightType", "minmax"))
			{
			case "lower":	return { "min": "-Inf",								"max": oldOption(options, "lower_max", 0),	"points": [oldOption(options, "lower_max", 0)] };
			case "upper":	return { "min": oldOption(options, "upper_min", 0),	"max": "Inf",								"points": [oldOption(options, "upper_min", 0)] };
			default:
				var min = oldOption(options, "min", 0), max = oldOption(options, "max", 1);
				return { "min": min, "max": max, "points": [min, max] };
			}
		}

		// Specify distribution (parameters first: the parametrization of the old analysis is still needed)
		ChangeCopy	{ from: "explanatoryText"; to: "distribution" }
		ChangeJS	{ name: "distribution";		jsFunction: function(options) { return step.oldSpecification(step.functionName, options)["distribution"]; } }
		ChangeCopy	{ from: "explanatoryText"; to: "parameters" }
		ChangeJS	{ name: "parameters";		jsFunction: function(options) { return step.oldSpecification(step.functionName, options)["parameters"]; } }
		ChangeCopy	{ from: "explanatoryText"; to: "parametrization"; condition: function(options) { return !step.hasOldOption(options, "parametrization"); } }
		ChangeJS	{ name: "parametrization";	jsFunction: function(options) { return step.oldSpecification(step.functionName, options)["parametrization"]; } }

		// the old analyses showed a table of parameters only for the estimates
		ChangeCopy	{ from: "explanatoryText"; to: "parameterTable" }
		ChangeJS	{ name: "parameterTable";	jsFunction: function(options) { return step.oldFitsDistribution(options) && step.oldOption(options, "outputEstimates", true); } }

		// Show distribution
		ChangeCopy	{ from: "explanatoryText"; to: "parametersSupportMoments" }
		ChangeJS	{ name: "parametersSupportMoments";	jsFunction: function(options) { return step.oldOption(options, "parsSupportMoments", false); } }
		ChangeCopy	{ from: "explanatoryText"; to: "pdfPlot" }
		ChangeJS	{ name: "pdfPlot";			jsFunction: function(options) { return step.oldOption(options, "plotPDF", true); } }
		ChangeCopy	{ from: "explanatoryText"; to: "cdfPlot" }
		ChangeJS	{ name: "cdfPlot";			jsFunction: function(options) { return step.oldOption(options, "plotCDF", false); } }
		ChangeCopy	{ from: "explanatoryText"; to: "qfPlot" }
		ChangeJS	{ name: "qfPlot";			jsFunction: function(options) { return step.oldOption(options, "plotQF", false); } }

		ChangeCopy	{ id: plotRange; from: "explanatoryText"; to: "distributionPlotRangeBasedOn"; condition: function(options) { return step.hasOldOption(options, "min_x") && step.hasOldOption(options, "max_x"); } }
		ChangeJS	{ name: "distributionPlotRangeBasedOn";	condition: plotRange.condition; jsFunction: function(options) { return "quantile"; } }
		ChangeCopy	{ from: "explanatoryText"; to: "distributionPlotQuantileMin";	condition: plotRange.condition }
		ChangeJS	{ name: "distributionPlotQuantileMin";	condition: plotRange.condition; jsFunction: function(options) { return options["min_x"]; } }
		ChangeCopy	{ from: "explanatoryText"; to: "distributionPlotQuantileMax";	condition: plotRange.condition }
		ChangeJS	{ name: "distributionPlotQuantileMax";	condition: plotRange.condition; jsFunction: function(options) { return options["max_x"]; } }

		ChangeCopy	{ id: highlightDensity; from: "explanatoryText"; to: "distributionPlotHighlightDensity"; condition: function(options) { return step.oldOption(options, "highlightDensity", false) === true; } }
		ChangeJS
		{
			name:		"distributionPlotHighlightDensity"
			condition:	highlightDensity.condition
			jsFunction:	function(options)
			{
				return step.componentsListRows(step.oldHighlightInterval(options)["points"].map(function(point) { return { "point": point }; }));
			}
		}
		ChangeCopy	{ id: highlightProbability; from: "explanatoryText"; to: "distributionPlotHighlightProbability"; condition: function(options) { return step.oldOption(options, "highlightProbability", false) === true; } }
		ChangeJS
		{
			name:		"distributionPlotHighlightProbability"
			condition:	highlightProbability.condition
			jsFunction:	function(options)
			{
				var interval = step.oldHighlightInterval(options);
				return step.componentsListRows([{ "min": interval["min"], "max": interval["max"] }]);
			}
		}

		// Simulate and display data
		ChangeCopy	{ id: simulatedData; from: "explanatoryText"; to: "simulatedDataSavedToData"; condition: function(options) { return step.oldOption(options, "newVariableName", "") !== ""; } }
		ChangeJS	{ name: "simulatedDataSavedToData";			condition: simulatedData.condition; jsFunction: function(options) { return true; } }
		ChangeCopy	{ from: "explanatoryText"; to: "simulatedDataSavedToDataColumn";	condition: simulatedData.condition }
		ChangeJS	{ name: "simulatedDataSavedToDataColumn";	condition: simulatedData.condition; jsFunction: function(options) { return options["newVariableName"]; } }
		ChangeCopy	{ id: sampleSize; from: "explanatoryText"; to: "simulatedDataSampleSize"; condition: function(options) { return step.hasOldOption(options, "sampleSize") && options["sampleSize"] > 0; } }
		ChangeJS	{ name: "simulatedDataSampleSize";			condition: sampleSize.condition; jsFunction: function(options) { return options["sampleSize"]; } }

		ChangeCopy	{ from: "explanatoryText"; to: "descriptivesTable" }
		ChangeJS	{ name: "descriptivesTable";	jsFunction: function(options) { return step.oldOption(options, "summary", true); } }

		// Estimate parameters: the variable is used for estimation only if the old analysis fitted the distribution,
		// otherwise it is only displayed (with the specified distribution)
		ChangeCopy	{ from: "variable"; to: "estimationVariable" }
		ChangeJS	{ name: "estimationVariable";		jsFunction: function(options) { return step.variableOption(options, step.oldFitsDistribution(options)); } }
		ChangeCopy	{ from: "variable"; to: "fitAssessmentVariable" }
		ChangeJS	{ name: "fitAssessmentVariable";	jsFunction: function(options) { return step.variableOption(options, !step.oldFitsDistribution(options)); } }

		// the bias-corrected estimator was only available for the normal distribution, maximum likelihood otherwise
		ChangeCopy	{ from: "explanatoryText"; to: "parameterEstimator" }
		ChangeJS	{ name: "parameterEstimator";	jsFunction: function(options) { return step.oldOption(options, "biasCorrected", false) ? "default" : "maximumLikelihood"; } }
		ChangeCopy	{ from: "explanatoryText"; to: "parameterUncertainty" }
		ChangeJS	{ name: "parameterUncertainty";	jsFunction: function(options) { return step.oldOption(options, "outputSE", false) || step.oldOption(options, "ciInterval", false); } }
		ChangeCopy	{ id: ciLevel; from: "explanatoryText"; to: "parameterUncertaintyCiLevel"; condition: function(options) { return step.hasOldOption(options, "ciIntervalInterval"); } }
		ChangeJS	{ name: "parameterUncertaintyCiLevel";	condition: ciLevel.condition; jsFunction: function(options) { return options["ciIntervalInterval"]; } }

		// Assess fit (the plots of only the data are shown together with the specified distribution)
		ChangeCopy	{ from: "explanatoryText"; to: "histogramPlot" }
		ChangeJS	{ name: "histogramPlot";	jsFunction: function(options) { return step.oldOption(options, "estPDF", false) || step.oldOption(options, "histogram", false); } }
		ChangeCopy	{ from: "explanatoryText"; to: "ecdfPlot" }
		ChangeJS	{ name: "ecdfPlot";			jsFunction: function(options) { return step.oldOption(options, "estCDF", false) || step.oldOption(options, "ecdf", false); } }
		ChangeCopy	{ from: "explanatoryText"; to: "qqPlot" }
		ChangeJS	{ name: "qqPlot";			jsFunction: function(options) { return step.oldOption(options, "qqplot", false); } }
		ChangeCopy	{ from: "explanatoryText"; to: "ppPlot" }
		ChangeJS	{ name: "ppPlot";			jsFunction: function(options) { return step.oldOption(options, "ppplot", false); } }
		ChangeCopy	{ from: "explanatoryText"; to: "goodnessOfFitTest" }
		ChangeJS
		{
			name:		"goodnessOfFitTest"
			jsFunction:	function(options)
			{
				return ["kolmogorovSmirnov", "cramerVonMisses", "andersonDarling", "lillienfors", "shapiroWilk", "shapiroFrancia"]
					.some(function(name) { return options[name] === true; });
			}
		}
	}

	ContinuousDistributionUpgrade { functionName: "LDgaussianunivariate" }
	ContinuousDistributionUpgrade { functionName: "LDnormalSkew" }
	ContinuousDistributionUpgrade { functionName: "LDnormalGeneralized" }
	ContinuousDistributionUpgrade { functionName: "LDcauchy" }
	ContinuousDistributionUpgrade { functionName: "LDtStudent" }
	ContinuousDistributionUpgrade { functionName: "LDtNoncentral" }
	ContinuousDistributionUpgrade { functionName: "LDtSkew" }
	ContinuousDistributionUpgrade { functionName: "LDtSkewedGeneralized" }
	ContinuousDistributionUpgrade { functionName: "LDf" }
	ContinuousDistributionUpgrade { functionName: "LDchisq" }
	ContinuousDistributionUpgrade { functionName: "LDuniform" }
	ContinuousDistributionUpgrade { functionName: "LDbeta" }
	ContinuousDistributionUpgrade { functionName: "LDbetaStretched" }
	ContinuousDistributionUpgrade { functionName: "LDbetaPrime" }
	ContinuousDistributionUpgrade { functionName: "LDgamma" }
	ContinuousDistributionUpgrade { functionName: "LDgammaInverse" }
	ContinuousDistributionUpgrade { functionName: "LDexponential" }
	ContinuousDistributionUpgrade { functionName: "LDlaplace" }
	ContinuousDistributionUpgrade { functionName: "LDlognormal" }
	ContinuousDistributionUpgrade { functionName: "LDlogistic" }
	ContinuousDistributionUpgrade { functionName: "LDlogLogistic" }
	ContinuousDistributionUpgrade { functionName: "LDpareto" }
	ContinuousDistributionUpgrade { functionName: "LDamoroso" }
	ContinuousDistributionUpgrade { functionName: "LDfrechet" }
	ContinuousDistributionUpgrade { functionName: "LDgumbel" }
	ContinuousDistributionUpgrade { functionName: "LDgompertz" }
	ContinuousDistributionUpgrade { functionName: "LDtriangular" }
	ContinuousDistributionUpgrade { functionName: "LDwald" }
	ContinuousDistributionUpgrade { functionName: "LDweibull" }
}
