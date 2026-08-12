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
					"StudentT":						["nu"],
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
}
