import QtQuick
import JASP
import JASP.Controls

Column
{
	id:				root
	spacing:		5
	leftPadding:	10

	property var allDistributionParameters: {
		"Normal"					: {
			"parameterChoice": [
				{ label: qsTr("μ (mean), σ (std.deviation)"),	value: "sigma"	},
				{ label: qsTr("μ (mean), σ² (variance)"),		value: "sigma2"	},
				{ label: qsTr("μ (mean), τ (precision)"),		value: "tau"	}
			],
			"sigma": [
				{ value: "mu",		label: qsTr("μ (mean)"),			min: -Infinity,		defaultValue: 0 },
				{ value: "sigma",	label: qsTr("σ (std. deviation)"),	min: 0,				defaultValue: 1 }
			],
			"sigma2": [
				{ value: "mu",		label: qsTr("μ (mean)"),			min: -Infinity,		defaultValue: 0 },
				{ value: "sigma2",	label: qsTr("σ² (variance)"),		min: 0,				defaultValue: 1 }
			],
			"tau": [
				{ value: "mu",		label: qsTr("μ (mean)"),			min: -Infinity,		defaultValue: 0 },
				{ value: "tau",		label: qsTr("τ (precision)"),		min: 0,				defaultValue: 1 }
			]
		},
		"StandardNormal"			: {},
		"StandardT"					: {
			"default": [
				{ value: "nu",		label: qsTr("ν (df)"),				min: 0,				defaultValue: 5 }
			]
		},
		"StudentT"					: {
			"default": [
				{ value: "nu",		label: qsTr("ν (df)"),				min: 0,				defaultValue: 5 }
			]
		},
		"NoncentralT"				: {
			"default": [
				{ value: "nu",		label: qsTr("ν (df)"),				min: 0,				defaultValue: 5 },
				{ value: "kappa",	label: qsTr("κ (noncentrality)"),	min: -Infinity,		defaultValue: 0 }
			]
		},
		"NoncentralStudentT"		: {
			"default": [
				{ value: "nu",		label: qsTr("ν (df)"),				min: 0,				defaultValue: 5 },
				{ value: "kappa",	label: qsTr("κ (noncentrality)"),	min: -Infinity,		defaultValue: 0 },
				{ value: "mu",		label: qsTr("μ (location)"),		min: -Infinity,		defaultValue: 0 },
				{ value: "sigma",	label: qsTr("σ (scale)"),			min: 0,				defaultValue: 1 }
			]
		},
		"Cauchy"					: {
			"default": [
				{ value: "mu",		label: qsTr("μ (location)"),	min: -Infinity,	defaultValue: 0 },
				{ value: "sigma",	label: qsTr("σ (scale)"),		min: 0,			defaultValue: 1 }
			]
		},
		"Gumbel"					: {
			"default": [
				{ value: "mu",		label: qsTr("μ (location)"),	min: -Infinity,	defaultValue: 0 },
				{ value: "beta",	label: qsTr("β (scale)"),		min: 0,			defaultValue: 1 }
			]
		},
		"Laplace"					: {
			"default": [
				{ value: "mu",		label: qsTr("μ (location)"),	min: -Infinity,	defaultValue: 0 },
				{ value: "beta",	label: qsTr("β (scale)"),		min: 0,			defaultValue: 1 }
			]
		},
		"Logistic"					: {
			"default": [
				{ value: "mu",		label: qsTr("μ (location)"),	min: -Infinity,	defaultValue: 0 },
				{ value: "sigma",	label: qsTr("σ (scale)"),		min: 0,			defaultValue: 1 }
			]
		},
		"SkewedGeneralizedT"		: {
			"default": [
				{ value: "mu",		label: qsTr("μ (location)"),	min: -Infinity,	defaultValue: 0 },
				{ value: "sigma",	label: qsTr("σ (scale)"),		min: 0,			defaultValue: 1 },
				{ value: "lambda",	label: qsTr("λ (skew)"),		min: -1,		defaultValue: 0 },
				{ value: "p",		label: qsTr("p (kurtosis)"),	min: 0,			defaultValue: 2 },
				{ value: "q",		label: qsTr("q (kurtosis)"),	min: 0,			defaultValue: 2 }
			]
		},
		"SymmetricGeneralizedNormal": {
			"default": [
				{ value: "mu",		label: qsTr("μ (location)"),	min: -Infinity,	defaultValue: 0 },
				{ value: "alpha",	label: qsTr("α (scale)"),		min: 0,			defaultValue: 1 },
				{ value: "beta",	label: qsTr("β (shape)"),		min: 0,			defaultValue: 2 }
			]
		},
		"SkewNormal"				: {
			"default": [
				{ value: "xi",		label: qsTr("ξ (location)"),	min: -Infinity,	defaultValue: 0 },
				{ value: "omega",	label: qsTr("ω (scale)"),		min: 0,			defaultValue: 1 },
				{ value: "alpha",	label: qsTr("α (slant)"),		min: -Infinity,	defaultValue: 0 }
			]
		},
		"SkewCauchy"				: {
			"default": [
				{ value: "xi",		label: qsTr("ξ (location)"),	min: -Infinity,	defaultValue: 0 },
				{ value: "omega",	label: qsTr("ω (scale)"),		min: 0,			defaultValue: 1 },
				{ value: "alpha",	label: qsTr("α (slant)"),		min: -Infinity,	defaultValue: 0 }
			]
		},
		"SkewT"						: {
			"default": [
				{ value: "xi",		label: qsTr("ξ (location)"),	min: -Infinity,	defaultValue: 0 },
				{ value: "omega",	label: qsTr("ω (scale)"),		min: 0,			defaultValue: 1 },
				{ value: "alpha",	label: qsTr("α (slant)"),		min: -Infinity,	defaultValue: 0 },
				{ value: "nu",		label: qsTr("ν (df)"),			min: 0,			defaultValue: 5 }
			]
		},
		"Beta"						: {
			"default": [
				{ value: "alpha",	label: qsTr("α (shape)"),	min: 0,	defaultValue: 2 },
				{ value: "beta",	label: qsTr("β (shape)"),	min: 0,	defaultValue: 2 }
			]
		},
		"BetaPrime"					: {
			"default": [
				{ value: "alpha",	label: qsTr("α (shape)"),	min: 0,	defaultValue: 2 },
				{ value: "beta",	label: qsTr("β (shape)"),	min: 0,	defaultValue: 2 }
			]
		},
		"CentralF"					: {
			"default": [
				{ value: "nu1",		label: qsTr("ν₁ (df)"),	min: 0,	defaultValue: 5 },
				{ value: "nu2",		label: qsTr("ν₂ (df)"),	min: 0,	defaultValue: 5 }
			]
		},
		"NoncentralF"				: {
			"default": [
				{ value: "nu1",		label: qsTr("ν₁ (df)"),			min: 0,	defaultValue: 5 },
				{ value: "nu2",		label: qsTr("ν₂ (df)"),			min: 0,	defaultValue: 5 },
				{ value: "kappa",	label: qsTr("κ (noncentrality)"),	min: 0,	defaultValue: 1 }
			]
		},
		"ChiSquared"				: {
			"default": [
				{ value: "nu",	label: qsTr("ν (df)"),	min: 0,	defaultValue: 5 }
			]
		},
		"NoncentralChiSquared"		: {
			"default": [
				{ value: "nu",		label: qsTr("ν (df)"),				min: 0,	defaultValue: 5 },
				{ value: "kappa",	label: qsTr("κ (noncentrality)"),	min: 0,	defaultValue: 1 }
			]
		},
		"Exponential"				: {
			"parameterChoice": [
				{ label: qsTr("λ (rate)"),	value: "lambda"	},
				{ label: qsTr("β (scale)"),	value: "beta"	}
			],
			"lambda": [
				{ value: "lambda",	label: qsTr("λ (rate)"),	min: 0,	defaultValue: 1 }
			],
			"beta": [
				{ value: "beta",	label: qsTr("β (scale)"),	min: 0,	defaultValue: 1 }
			]
		},
		"Gamma"						: {
			"parameterChoice": [
				{ label: qsTr("α (shape), θ (scale)"),	value: "theta"	},
				{ label: qsTr("α (shape), λ (rate)"),	value: "lambda"	},
				{ label: qsTr("α (shape), μ (mean)"),	value: "mu"		}
			],
			"theta": [
				{ value: "alpha",	label: qsTr("α (shape)"),	min: 0,	defaultValue: 2 },
				{ value: "theta",	label: qsTr("θ (scale)"),	min: 0,	defaultValue: 1 }
			],
			"lambda": [
				{ value: "alpha",	label: qsTr("α (shape)"),	min: 0,	defaultValue: 2 },
				{ value: "lambda",	label: qsTr("λ (rate)"),	min: 0,	defaultValue: 1 }
			],
			"mu": [
				{ value: "alpha",	label: qsTr("α (shape)"),	min: 0,	defaultValue: 2 },
				{ value: "mu",		label: qsTr("μ (mean)"),	min: 0,	defaultValue: 1 }
			]
		},
		"InverseGamma"				: {
			"parameterChoice": [
				{ label: qsTr("α (shape), θ (scale)"),	value: "theta"	},
				{ label: qsTr("α (shape), λ (rate)"),	value: "lambda"	},
				{ label: qsTr("α (shape), μ (mean)"),	value: "mu"		}
			],
			"theta": [
				{ value: "alpha",	label: qsTr("α (shape)"),	min: 0,	defaultValue: 2 },
				{ value: "theta",	label: qsTr("θ (scale)"),	min: 0,	defaultValue: 1 }
			],
			"lambda": [
				{ value: "alpha",	label: qsTr("α (shape)"),	min: 0,	defaultValue: 2 },
				{ value: "lambda",	label: qsTr("λ (rate)"),	min: 0,	defaultValue: 1 }
			],
			"mu": [
				{ value: "alpha",	label: qsTr("α (shape)"),	min: 0,	defaultValue: 2 },
				{ value: "mu",		label: qsTr("μ (mean)"),	min: 0,	defaultValue: 1 }
			]
		},
		"Gompertz"					: {
			"default": [
				{ value: "eta",		label: qsTr("η (shape)"),	min: 0,	defaultValue: 1 },
				{ value: "beta",	label: qsTr("β (scale)"),	min: 0,	defaultValue: 1 }
			]
		},
		"LogLogistic"				: {
			"parameterChoice": [
				{ label: qsTr("μ (log location), σ (log scale)"),	value: "mu"		},
				{ label: qsTr("α (scale), β (shape)"),				value: "alpha"	}
			],
			"mu": [
				{ value: "mu",		label: qsTr("μ (log location)"),	min: -Infinity,	defaultValue: 0 },
				{ value: "sigma",	label: qsTr("σ (log scale)"),		min: 0,			defaultValue: 1 }
			],
			"alpha": [
				{ value: "alpha",	label: qsTr("α (scale)"),	min: 0,	defaultValue: 1 },
				{ value: "beta",	label: qsTr("β (shape)"),	min: 0,	defaultValue: 2 }
			]
		},
		"LogNormal"					: {
			"default": [
				{ value: "mu",		label: qsTr("μ (log mean)"),			min: -Infinity,	defaultValue: 0 },
				{ value: "sigma",	label: qsTr("σ (log std. deviation)"),	min: 0,			defaultValue: 1 }
			]
		},
		"Wald"						: {
			"parameterChoice": [
				{ label: qsTr("μ (mean), λ (shape)"),					value: "mu"	},
				{ label: qsTr("ν (drift), α (threshold), σ (noise)"),	value: "nu"	}
			],
			"mu": [
				{ value: "mu",		label: qsTr("μ (mean)"),		min: 0,	defaultValue: 1 },
				{ value: "lambda",	label: qsTr("λ (shape)"),		min: 0,	defaultValue: 1 }
			],
			"nu": [
				{ value: "nu",		label: qsTr("ν (drift)"),		min: 0,	defaultValue: 1 },
				{ value: "alpha",	label: qsTr("α (threshold)"),	min: 0,	defaultValue: 1 },
				{ value: "sigma",	label: qsTr("σ (noise)"),		min: 0,	defaultValue: 1 }
			]
		},
		"Weibull"					: {
			"default": [
				{ value: "shape",	label: qsTr("shape"),	min: 0,	defaultValue: 2 },
				{ value: "scale",	label: qsTr("scale"),	min: 0,	defaultValue: 1 }
			]
		},
		"Amoroso"					: {
			"default": [
				{ value: "a",		label: qsTr("a (location)"),	min: -Infinity,	defaultValue: 0 },
				{ value: "theta",	label: qsTr("θ (scale)"),		min: -Infinity,	defaultValue: 1 },
				{ value: "alpha",	label: qsTr("α (shape)"),		min: 0,			defaultValue: 2 },
				{ value: "beta",	label: qsTr("β (shape)"),		min: -Infinity,	defaultValue: 1 }
			]
		},
		"StretchedBeta"				: {
			"default": [
				{ value: "alpha",	label: qsTr("α (shape)"),	min: 0,			defaultValue: 2 },
				{ value: "beta",	label: qsTr("β (shape)"),	min: 0,			defaultValue: 2 },
				{ value: "min",		label: qsTr("min"),			min: -Infinity,	defaultValue: 0 },
				{ value: "max",		label: qsTr("max"),			min: -Infinity,	defaultValue: 1 }
			]
		},
		"Frechet"					: {
			"default": [
				{ value: "alpha",	label: qsTr("α (shape)"),		min: 0,			defaultValue: 2 },
				{ value: "sigma",	label: qsTr("σ (scale)"),		min: 0,			defaultValue: 1 },
				{ value: "theta",	label: qsTr("θ (location)"),	min: -Infinity,	defaultValue: 0 }
			]
		},
		"Pareto"					: {
			"default": [
				{ value: "alpha",	label: qsTr("α (shape)"),	min: 0,	defaultValue: 2 },
				{ value: "beta",	label: qsTr("β (scale)"),	min: 0,	defaultValue: 1 }
			]
		},
		"Triangular"				: {
			"default": [
				{ value: "a",	label: qsTr("a (min)"),		min: -Infinity,	defaultValue: 0   },
				{ value: "b",	label: qsTr("b (max)"),		min: -Infinity,	defaultValue: 1   },
				{ value: "c",	label: qsTr("c (mode)"),	min: -Infinity,	defaultValue: 0.5 }
			]
		},
		"Uniform"					: {
			"default": [
				{ value: "min",	label: qsTr("min"),	min: -Infinity,	defaultValue: 0 },
				{ value: "max",	label: qsTr("max"),	min: -Infinity,	defaultValue: 1 }
			]
		},
		"ShiftedExponential"		: {
			"parameterChoice": [
				{ label: qsTr("λ (rate), shift"),	value: "lambda"	},
				{ label: qsTr("β (scale), shift"),	value: "beta"	}
			],
			"lambda": [
				{ value: "lambda",	label: qsTr("λ (rate)"),	min: 0,			defaultValue: 1 },
				{ value: "shift",	label: qsTr("shift"),		min: -Infinity,	defaultValue: 0 }
			],
			"beta": [
				{ value: "beta",	label: qsTr("β (scale)"),	min: 0,			defaultValue: 1 },
				{ value: "shift",	label: qsTr("shift"),		min: -Infinity,	defaultValue: 0 }
			]
		},
		"ShiftedLogNormal"			: {
			"default": [
				{ value: "mu",		label: qsTr("μ (log mean)"),			min: -Infinity,	defaultValue: 0 },
				{ value: "sigma",	label: qsTr("σ (log std. deviation)"),	min: 0,			defaultValue: 1 },
				{ value: "shift",	label: qsTr("shift"),					min: -Infinity,	defaultValue: 0 }
			]
		},
		"ShiftedGamma"				: {
			"parameterChoice": [
				{ label: qsTr("α (shape), θ (scale), shift"),	value: "theta"	},
				{ label: qsTr("α (shape), λ (rate), shift"),	value: "lambda"	},
				{ label: qsTr("α (shape), μ (mean), shift"),	value: "mu"		}
			],
			"theta": [
				{ value: "alpha",	label: qsTr("α (shape)"),	min: 0,			defaultValue: 2 },
				{ value: "theta",	label: qsTr("θ (scale)"),	min: 0,			defaultValue: 1 },
				{ value: "shift",	label: qsTr("shift"),		min: -Infinity,	defaultValue: 0 }
			],
			"lambda": [
				{ value: "alpha",	label: qsTr("α (shape)"),	min: 0,			defaultValue: 2 },
				{ value: "lambda",	label: qsTr("λ (rate)"),	min: 0,			defaultValue: 1 },
				{ value: "shift",	label: qsTr("shift"),		min: -Infinity,	defaultValue: 0 }
			],
			"mu": [
				{ value: "alpha",	label: qsTr("α (shape)"),	min: 0,			defaultValue: 2 },
				{ value: "mu",		label: qsTr("μ (mean)"),	min: 0,			defaultValue: 1 },
				{ value: "shift",	label: qsTr("shift"),		min: -Infinity,	defaultValue: 0 }
			]
		},
		"ShiftedInverseGamma"		: {
			"parameterChoice": [
				{ label: qsTr("α (shape), θ (scale), shift"),	value: "theta"	},
				{ label: qsTr("α (shape), λ (rate), shift"),	value: "lambda"	},
				{ label: qsTr("α (shape), μ (mean), shift"),	value: "mu"		}
			],
			"theta": [
				{ value: "alpha",	label: qsTr("α (shape)"),	min: 0,			defaultValue: 2 },
				{ value: "theta",	label: qsTr("θ (scale)"),	min: 0,			defaultValue: 1 },
				{ value: "shift",	label: qsTr("shift"),		min: -Infinity,	defaultValue: 0 }
			],
			"lambda": [
				{ value: "alpha",	label: qsTr("α (shape)"),	min: 0,			defaultValue: 2 },
				{ value: "lambda",	label: qsTr("λ (rate)"),	min: 0,			defaultValue: 1 },
				{ value: "shift",	label: qsTr("shift"),		min: -Infinity,	defaultValue: 0 }
			],
			"mu": [
				{ value: "alpha",	label: qsTr("α (shape)"),	min: 0,			defaultValue: 2 },
				{ value: "mu",		label: qsTr("μ (mean)"),	min: 0,			defaultValue: 1 },
				{ value: "shift",	label: qsTr("shift"),		min: -Infinity,	defaultValue: 0 }
			]
		},
		"ShiftedLogLogistic"		: {
			"parameterChoice": [
				{ label: qsTr("μ (log location), σ (log scale), shift"),	value: "mu"		},
				{ label: qsTr("α (scale), β (shape), shift"),				value: "alpha"	}
			],
			"mu": [
				{ value: "mu",		label: qsTr("μ (log location)"),	min: -Infinity,	defaultValue: 0 },
				{ value: "sigma",	label: qsTr("σ (log scale)"),		min: 0,			defaultValue: 1 },
				{ value: "shift",	label: qsTr("shift"),				min: -Infinity,	defaultValue: 0 }
			],
			"alpha": [
				{ value: "alpha",	label: qsTr("α (scale)"),	min: 0,			defaultValue: 1 },
				{ value: "beta",	label: qsTr("β (shape)"),	min: 0,			defaultValue: 2 },
				{ value: "shift",	label: qsTr("shift"),		min: -Infinity,	defaultValue: 0 }
			]
		},
		"ShiftedWald"				: {
			"parameterChoice": [
				{ label: qsTr("μ (mean), λ (shape), shift"),					value: "mu"	},
				{ label: qsTr("ν (drift), α (threshold), σ (noise), shift"),	value: "nu"	}
			],
			"mu": [
				{ value: "mu",		label: qsTr("μ (mean)"),		min: 0,			defaultValue: 1 },
				{ value: "lambda",	label: qsTr("λ (shape)"),		min: 0,			defaultValue: 1 },
				{ value: "shift",	label: qsTr("shift"),			min: -Infinity,	defaultValue: 0 }
			],
			"nu": [
				{ value: "nu",		label: qsTr("ν (drift)"),		min: 0,			defaultValue: 1 },
				{ value: "alpha",	label: qsTr("α (threshold)"),	min: 0,			defaultValue: 1 },
				{ value: "sigma",	label: qsTr("σ (noise)"),		min: 0,			defaultValue: 1 },
				{ value: "shift",	label: qsTr("shift"),			min: -Infinity,	defaultValue: 0 }
			]
		},
		"ShiftedWeibull"			: {
			"default": [
				{ value: "shape",	label: qsTr("shape"),	min: 0,			defaultValue: 2 },
				{ value: "scale",	label: qsTr("scale"),	min: 0,			defaultValue: 1 },
				{ value: "shift",	label: qsTr("shift"),	min: -Infinity,	defaultValue: 0 }
			]
		}

	}

	Row
	{
		spacing: 5 * jaspTheme.uiScale
		DropDown
		{
			id: distribution
			name: "distribution"
			addEmptyValue: true
			placeholderText: qsTr("<Select a distribution>")
			values: [
				// Unbounded
				{ label: qsTr("Normal"),						value: "Normal"						},
				{ label: qsTr("Standard normal"),				value: "StandardNormal"				},
				{ label: qsTr("Standard t"),					value: "StandardT"					},
				{ label: qsTr("Student t"),						value: "StudentT"					},
				{ label: qsTr("Noncentral t"),					value: "NoncentralT"				},
				{ label: qsTr("Noncentral Student t"),			value: "NoncentralStudentT"			},
				{ label: qsTr("Cauchy"),						value: "Cauchy"						},
				{ label: qsTr("Gumbel"),						value: "Gumbel"						},
				{ label: qsTr("Laplace"),						value: "Laplace"					},
				{ label: qsTr("Logistic"),						value: "Logistic"					},
				{ label: qsTr("Skewed generalized t"),			value: "SkewedGeneralizedT"			},
				{ label: qsTr("Symmetric generalized normal"),	value: "SymmetricGeneralizedNormal"	},
				{ label: qsTr("Skew normal"),					value: "SkewNormal"					},
				{ label: qsTr("Skew Cauchy"),					value: "SkewCauchy"					},
				{ label: qsTr("Skew t"),						value: "SkewT"						},
				// Bounded — fixed support
				{ label: qsTr("Beta"),							value: "Beta"						},
				{ label: qsTr("Beta prime"),					value: "BetaPrime"					},
				{ label: qsTr("F"),								value: "CentralF"					},
				{ label: qsTr("Noncentral F"),					value: "NoncentralF"				},
				{ label: qsTr("Chi-squared"),					value: "ChiSquared"					},
				{ label: qsTr("Noncentral chi-squared"),		value: "NoncentralChiSquared"		},
				{ label: qsTr("Exponential"),					value: "Exponential"				},
				{ label: qsTr("Gamma"),							value: "Gamma"						},
				{ label: qsTr("Inverse gamma"),					value: "InverseGamma"				},
				{ label: qsTr("Gompertz"),						value: "Gompertz"					},
				{ label: qsTr("Log-logistic"),					value: "LogLogistic"				},
				{ label: qsTr("Log-normal"),					value: "LogNormal"					},
				{ label: qsTr("Wald"),							value: "Wald"						},
				{ label: qsTr("Weibull"),						value: "Weibull"					},
				// Bounded — dynamic support
				{ label: qsTr("Amoroso"),						value: "Amoroso"					},
				{ label: qsTr("Stretched beta"),				value: "StretchedBeta"				},
				{ label: qsTr("Fréchet"),						value: "Frechet"					},
				{ label: qsTr("Pareto"),						value: "Pareto"						},
				{ label: qsTr("Triangular"),					value: "Triangular"					},
				{ label: qsTr("Uniform"),						value: "Uniform"					},
				// Shifted
				{ label: qsTr("Shifted Exponential"),			value: "ShiftedExponential"			},
				{ label: qsTr("Shifted Log-normal"),			value: "ShiftedLogNormal"			},
				{ label: qsTr("Shifted Gamma"),					value: "ShiftedGamma"				},
				{ label: qsTr("Shifted Inverse gamma"),			value: "ShiftedInverseGamma"		},
				{ label: qsTr("Shifted Log-logistic"),			value: "ShiftedLogLogistic"			},
				{ label: qsTr("Shifted Wald"),					value: "ShiftedWald"				},
				{ label: qsTr("Shifted Weibull"),				value: "ShiftedWeibull"				}
			]

			property var parameters: distribution.currentValue ? root.allDistributionParameters[distribution.currentValue] : {}

		}
		CheckBox
		{
			id: settings
			name: "settings"
			label: qsTr("Show parameter settings")
			checked: false
			visible: !["", "StandardNormal"].includes(distribution.currentValue)
			isBound: false
		}
	}

	Column
	{
		spacing:		5 * jaspTheme.uiScale
		leftPadding:	10 * jaspTheme.uiScale
		visible:		settings.checked

		DropDown
		{
			id:			parameterChoice
			name:		"parameters"
			label:		qsTr("Parameters")
			visible:	distribution.parameters.hasOwnProperty("parameterChoice")
			values:		(visible && distribution.parameters) ? distribution.parameters["parameterChoice"] : []

			property var parametersSettings: currentValue ? distribution.parameters[parameterChoice.currentValue]
														  : (distribution.parameters.hasOwnProperty("default") ? distribution.parameters["default"] : [])

		}

		ComponentsList
		{
			id:					parametersSettingsId
			values:				parameterChoice.parametersSettings
			name:				"parametersSettings"
			addItemManually:	false
			visible:			values.length > 0


			rowComponent: Row
			{
				spacing:		10 * jaspTheme.uiScale
				Label			{ width: 100 * jaspTheme.uiScale;	text: rowLabel }
				DoubleField		{ name: rowValue;					min: parameterChoice.parametersSettings[rowIndex].min;    defaultValue: parameterChoice.parametersSettings[rowIndex].defaultValue }
				CheckBox		{ name: rowValue + "Fixed";			label: qsTr("Fixed") }
			}

		}
	}
}
