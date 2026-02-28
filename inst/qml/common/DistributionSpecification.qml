import QtQuick
import JASP
import JASP.Controls
import "./parameter-specification" as PS

Column
{
	id: root
	spacing: 5
	leftPadding: 10

	property var parametersValues: {
		// Multi-parametrization distributions
		if (distribution.currentValue === "Normal")
			return [
				{ label: qsTr("μ (mean), σ (std.deviation)"),	value: "sigma"	},
				{ label: qsTr("μ (mean), σ² (variance)"),		value: "sigma2"	},
				{ label: qsTr("μ (mean), τ (precision)"),		value: "tau"	}
			]
		if (distribution.currentValue === "Exponential")
			return [
				{ label: qsTr("λ (rate)"), 	value: "lambda"	},
				{ label: qsTr("β (scale)"),	value: "beta" 	}
			]
		if (distribution.currentValue === "Gamma")
			return [
				{ label: qsTr("α (shape), θ (scale)"),	value: "theta"	},
				{ label: qsTr("α (shape), λ (rate)"),	value: "lambda"	},
				{ label: qsTr("α (shape), μ (mean)"),	value: "mu"		}
			]
		if (distribution.currentValue === "InverseGamma")
			return [
				{ label: qsTr("α (shape), θ (scale)"),	value: "theta"	},
				{ label: qsTr("α (shape), λ (rate)"),	value: "lambda"	},
				{ label: qsTr("α (shape), μ (mean)"),	value: "mu"		}
			]
		if (distribution.currentValue === "LogLogistic")
			return [
				{ label: qsTr("μ (log-location), σ (log-scale)"),	value: "nu"	},
				{ label: qsTr("α (scale), β (shape)"),				value: "alpha"	}
			]
		if (distribution.currentValue === "Wald")
			return [
				{ label: qsTr("μ (mean), λ (shape)"),					value: "mu"		},
				{ label: qsTr("ν (drift), α (threshold), σ (noise)"),	value: "nu"	}
			]
		if (distribution.currentValue === "ShiftedExponential")
			return [
				{ label: qsTr("λ (rate), shift"),	value: "lambda"	},
				{ label: qsTr("β (scale), shift"),	value: "beta"	}
			]
		if (distribution.currentValue === "ShiftedGamma")
			return [
				{ label: qsTr("α (shape), θ (scale), shift"),	value: "theta"	},
				{ label: qsTr("α (shape), λ (rate), shift"),	value: "lambda"	},
				{ label: qsTr("α (shape), μ (mean), shift"),	value: "mu"		}
			]
		if (distribution.currentValue === "ShiftedInverseGamma")
			return [
				{ label: qsTr("α (shape), θ (scale), shift"),	value: "theta"	},
				{ label: qsTr("α (shape), λ (rate), shift"),	value: "lambda"	},
				{ label: qsTr("α (shape), μ (mean), shift"),	value: "mu"		}
			]
		if (distribution.currentValue === "ShiftedLogLogistic")
			return [
				{ label: qsTr("μ (log-location), σ (log-scale), shift"),	value: "mu"	},
				{ label: qsTr("α (scale), β (shape), shift"),				value: "alpha"	}
			]
		if (distribution.currentValue === "ShiftedWald")
			return [
				{ label: qsTr("μ (mean), λ (shape), shift"),					value: "mu"		},
				{ label: qsTr("ν (drift), α (threshold), σ (noise), shift"),	value: "nu"	}
			]
		// Single-parametrization distributions
		return [{ label: qsTr("default"), value: "default" }]
	}

	Row
	{
		DropDown
		{
			id: distribution
			name: "distribution"
			values: [
				// Unbounded
				{ label: qsTr("Normal"),						value: "Normal"						},
				{ label: qsTr("Standard Normal"),				value: "StandardNormal"				},
				{ label: qsTr("Standard t"),					value: "StandardT"					},
				{ label: qsTr("Student t"),						value: "StudentT"					},
				{ label: qsTr("Noncentral t"),					value: "NoncentralT"				},
				{ label: qsTr("Noncentral Student t"),			value: "NoncentralStudentT"			},
				{ label: qsTr("Cauchy"),						value: "Cauchy"						},
				{ label: qsTr("Gumbel"),						value: "Gumbel"						},
				{ label: qsTr("Laplace"),						value: "Laplace"					},
				{ label: qsTr("Logistic"),						value: "Logistic"					},
				{ label: qsTr("Skewed Generalized t"),			value: "SkewedGeneralizedT"			},
				{ label: qsTr("Symmetric Generalized Normal"),	value: "SymmetricGeneralizedNormal"	},
				{ label: qsTr("Skew Normal"),					value: "SkewNormal"					},
				{ label: qsTr("Skew Cauchy"),					value: "SkewCauchy"					},
				{ label: qsTr("Skew t"),						value: "SkewT"						},
				// Bounded — fixed support
				{ label: qsTr("Beta"),							value: "Beta"						},
				{ label: qsTr("Beta Prime"),					value: "BetaPrime"					},
				{ label: qsTr("F"),								value: "CentralF"					},
				{ label: qsTr("Noncentral F"),					value: "NoncentralF"				},
				{ label: qsTr("Chi-Squared"),					value: "ChiSquared"					},
				{ label: qsTr("Noncentral Chi-Squared"),		value: "NoncentralChiSquared"		},
				{ label: qsTr("Exponential"),					value: "Exponential"				},
				{ label: qsTr("Gamma"),							value: "Gamma"						},
				{ label: qsTr("Inverse Gamma"),					value: "InverseGamma"				},
				{ label: qsTr("Gompertz"),						value: "Gompertz"					},
				{ label: qsTr("Log-Logistic"),					value: "LogLogistic"				},
				{ label: qsTr("Log-Normal"),					value: "LogNormal"					},
				{ label: qsTr("Wald"),							value: "Wald"						},
				{ label: qsTr("Weibull"),						value: "Weibull"					},
				// Bounded — dynamic support
				{ label: qsTr("Amoroso"),						value: "Amoroso"					},
				{ label: qsTr("Stretched Beta"),				value: "StretchedBeta"				},
				{ label: qsTr("Fréchet"),						value: "Frechet"					},
				{ label: qsTr("Pareto"),						value: "Pareto"						},
				{ label: qsTr("Triangular"),					value: "Triangular"					},
				{ label: qsTr("Uniform"),						value: "Uniform"					},
				// Shifted
				{ label: qsTr("Shifted Exponential"),			value: "ShiftedExponential"			},
				{ label: qsTr("Shifted Log-Normal"),			value: "ShiftedLogNormal"			},
				{ label: qsTr("Shifted Gamma"),					value: "ShiftedGamma"				},
				{ label: qsTr("Shifted Inverse Gamma"),			value: "ShiftedInverseGamma"		},
				{ label: qsTr("Shifted Log-Logistic"),			value: "ShiftedLogLogistic"			},
				{ label: qsTr("Shifted Wald"),					value: "ShiftedWald"				},
				{ label: qsTr("Shifted Weibull"),				value: "ShiftedWeibull"				}
			]
		}
		CheckBox
		{
			id: settings
			name: "settings"
			label: qsTr("Show parameter settings")
			checked: false
			visible: !["StandardNormal", "StandardT"].includes(distribution.currentValue)
		}
	}

	Column
	{
		spacing: 5
		leftPadding: 10
		visible: settings.checked

		Row
		{
			visible: root.parametersValues.length > 1
			DropDown
			{
				id: parameters
				name: "parameters"
				label: qsTr("Parameters")
				values: root.parametersValues
			}
		}

		// Unbounded
		PS.Normal					{ visible: distribution.currentValue === "Normal";					parameters: parameters.currentValue }
		PS.StandardNormal			{ visible: distribution.currentValue === "StandardNormal";			parameters: parameters.currentValue }
		PS.StandardT				{ visible: distribution.currentValue === "StandardT";				parameters: parameters.currentValue }
		PS.StudentT					{ visible: distribution.currentValue === "StudentT";				parameters: parameters.currentValue }
		PS.NoncentralT				{ visible: distribution.currentValue === "NoncentralT";				parameters: parameters.currentValue }
		PS.NoncentralStudentT		{ visible: distribution.currentValue === "NoncentralStudentT";		parameters: parameters.currentValue }
		PS.Cauchy					{ visible: distribution.currentValue === "Cauchy";					parameters: parameters.currentValue }
		PS.Gumbel					{ visible: distribution.currentValue === "Gumbel";					parameters: parameters.currentValue }
		PS.Laplace					{ visible: distribution.currentValue === "Laplace";					parameters: parameters.currentValue }
		PS.Logistic					{ visible: distribution.currentValue === "Logistic";				parameters: parameters.currentValue }
		PS.SkewedGeneralizedT		{ visible: distribution.currentValue === "SkewedGeneralizedT";		parameters: parameters.currentValue }
		PS.SymmetricGeneralizedNormal { visible: distribution.currentValue === "SymmetricGeneralizedNormal"; parameters: parameters.currentValue }
		PS.SkewNormal				{ visible: distribution.currentValue === "SkewNormal";				parameters: parameters.currentValue }
		PS.SkewCauchy				{ visible: distribution.currentValue === "SkewCauchy";				parameters: parameters.currentValue }
		PS.SkewT					{ visible: distribution.currentValue === "SkewT";					parameters: parameters.currentValue }
		// Bounded — fixed support
		PS.Beta						{ visible: distribution.currentValue === "Beta";					parameters: parameters.currentValue }
		PS.BetaPrime				{ visible: distribution.currentValue === "BetaPrime";				parameters: parameters.currentValue }
		PS.CentralF					{ visible: distribution.currentValue === "CentralF";				parameters: parameters.currentValue }
		PS.NoncentralF				{ visible: distribution.currentValue === "NoncentralF";				parameters: parameters.currentValue }
		PS.ChiSquared				{ visible: distribution.currentValue === "ChiSquared";				parameters: parameters.currentValue }
		PS.NoncentralChiSquared		{ visible: distribution.currentValue === "NoncentralChiSquared";	parameters: parameters.currentValue }
		PS.Exponential				{ visible: distribution.currentValue === "Exponential";				parameters: parameters.currentValue }
		PS.Gamma					{ visible: distribution.currentValue === "Gamma";					parameters: parameters.currentValue }
		PS.InverseGamma				{ visible: distribution.currentValue === "InverseGamma";			parameters: parameters.currentValue }
		PS.Gompertz					{ visible: distribution.currentValue === "Gompertz";				parameters: parameters.currentValue }
		PS.LogLogistic				{ visible: distribution.currentValue === "LogLogistic";				parameters: parameters.currentValue }
		PS.LogNormal				{ visible: distribution.currentValue === "LogNormal";				parameters: parameters.currentValue }
		PS.Wald						{ visible: distribution.currentValue === "Wald";					parameters: parameters.currentValue }
		PS.Weibull					{ visible: distribution.currentValue === "Weibull";					parameters: parameters.currentValue }
		// Bounded — dynamic support
		PS.Amoroso					{ visible: distribution.currentValue === "Amoroso";					parameters: parameters.currentValue }
		PS.StretchedBeta			{ visible: distribution.currentValue === "StretchedBeta";			parameters: parameters.currentValue }
		PS.Frechet					{ visible: distribution.currentValue === "Frechet";					parameters: parameters.currentValue }
		PS.Pareto					{ visible: distribution.currentValue === "Pareto";					parameters: parameters.currentValue }
		PS.Triangular				{ visible: distribution.currentValue === "Triangular";				parameters: parameters.currentValue }
		PS.Uniform					{ visible: distribution.currentValue === "Uniform";					parameters: parameters.currentValue }
		// Shifted
		PS.ShiftedExponential		{ visible: distribution.currentValue === "ShiftedExponential";		parameters: parameters.currentValue }
		PS.ShiftedLogNormal			{ visible: distribution.currentValue === "ShiftedLogNormal";		parameters: parameters.currentValue }
		PS.ShiftedGamma				{ visible: distribution.currentValue === "ShiftedGamma";			parameters: parameters.currentValue }
		PS.ShiftedInverseGamma		{ visible: distribution.currentValue === "ShiftedInverseGamma";		parameters: parameters.currentValue }
		PS.ShiftedLogLogistic		{ visible: distribution.currentValue === "ShiftedLogLogistic";		parameters: parameters.currentValue }
		PS.ShiftedWald				{ visible: distribution.currentValue === "ShiftedWald";				parameters: parameters.currentValue }
		PS.ShiftedWeibull			{ visible: distribution.currentValue === "ShiftedWeibull";			parameters: parameters.currentValue }
	}
}
