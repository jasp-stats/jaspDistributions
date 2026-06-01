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
				{ value: "normalMu",				label: qsTr("μ (mean)"),			min: -Infinity,		defaultValue: 0 },
				{ value: "normalSigma",				label: qsTr("σ (std. deviation)"),	min: 0,				defaultValue: 1 }
			],
			"sigma2": [
				{ value: "normalMu",				label: qsTr("μ (mean)"),			min: -Infinity,		defaultValue: 0 },
				{ value: "normalSigma2",			label: qsTr("σ² (variance)"),		min: 0,				defaultValue: 1 }
			],
			"tau": [
				{ value: "normalMu",				label: qsTr("μ (mean)"),			min: -Infinity,		defaultValue: 0 },
				{ value: "normalTau",				label: qsTr("τ (precision)"),		min: 0,				defaultValue: 1 }
			]
		},
		"StandardNormal"			: {},
		"StandardT"					: {
			"default": [
				{ value: "standardTNu",				label: qsTr("ν (df)"),				min: 0,				defaultValue: 5 }
			]
		},
		"StudentT"					: {
			"default": [
				{ value: "studentTNu",				label: qsTr("ν (df)"),				min: 0,				defaultValue: 5 }
			]
		},
		"NoncentralT"				: {
			"default": [
				{ value: "noncentralTNu",			label: qsTr("ν (df)"),				min: 0,				defaultValue: 5 },
				{ value: "noncentralTKappa",		label: qsTr("κ (noncentrality)"),	min: -Infinity,		defaultValue: 0 }
			]
		},
		"NoncentralStudentT"		: {
			"default": [
				{ value: "noncentralStudentTNu",	label: qsTr("ν (df)"),				min: 0,				defaultValue: 5 },
				{ value: "noncentralStudentTKappa",	label: qsTr("κ (noncentrality)"),	min: -Infinity,		defaultValue: 0 },
				{ value: "noncentralStudentTMu",	label: qsTr("μ (location)"),		min: -Infinity,		defaultValue: 0 },
				{ value: "noncentralStudentTSigma",	label: qsTr("σ (scale)"),			min: 0,				defaultValue: 1 }
			]
		},
		"Cauchy"					: {
			"default": [
				{ value: "cauchyMu",		label: qsTr("μ (location)"),	min: -Infinity,	defaultValue: 0 },
				{ value: "cauchySigma",		label: qsTr("σ (scale)"),		min: 0,			defaultValue: 1 }
			]
		},
		"Gumbel"					: {
			"default": [
				{ value: "gumbelMu",		label: qsTr("μ (location)"),	min: -Infinity,	defaultValue: 0 },
				{ value: "gumbelBeta",		label: qsTr("β (scale)"),		min: 0,			defaultValue: 1 }
			]
		},
		"Laplace"					: {
			"default": [
				{ value: "laplaceMu",		label: qsTr("μ (location)"),	min: -Infinity,	defaultValue: 0 },
				{ value: "laplaceBeta",		label: qsTr("β (scale)"),		min: 0,			defaultValue: 1 }
			]
		},
		"Logistic"					: {
			"default": [
				{ value: "logisticMu",		label: qsTr("μ (location)"),	min: -Infinity,	defaultValue: 0 },
				{ value: "logisticSigma",	label: qsTr("σ (scale)"),		min: 0,			defaultValue: 1 }
			]
		},
		"SkewedGeneralizedT"		: {
			"default": [
				{ value: "skewedGeneralizedTMu",		label: qsTr("μ (location)"),	min: -Infinity,	defaultValue: 0 },
				{ value: "skewedGeneralizedTSigma",		label: qsTr("σ (scale)"),		min: 0,			defaultValue: 1 },
				{ value: "skewedGeneralizedTLambda",	label: qsTr("λ (skew)"),		min: -1,		defaultValue: 0 },
				{ value: "skewedGeneralizedTP",			label: qsTr("p (kurtosis)"),	min: 0,			defaultValue: 2 },
				{ value: "skewedGeneralizedTQ",			label: qsTr("q (kurtosis)"),	min: 0,			defaultValue: 2 }
			]
		},
		"SymmetricGeneralizedNormal": {
			"default": [
				{ value: "symmetricGeneralizedNormalMu",	label: qsTr("μ (location)"),	min: -Infinity,	defaultValue: 0 },
				{ value: "symmetricGeneralizedNormalAlpha",	label: qsTr("α (scale)"),		min: 0,			defaultValue: 1 },
				{ value: "symmetricGeneralizedNormalBeta",	label: qsTr("β (shape)"),		min: 0,			defaultValue: 2 }
			]
		},
		"SkewNormal"				: {
			"default": [
				{ value: "skewNormalXi",	label: qsTr("ξ (location)"),	min: -Infinity,	defaultValue: 0 },
				{ value: "skewNormalOmega",	label: qsTr("ω (scale)"),		min: 0,			defaultValue: 1 },
				{ value: "skewNormalAlpha",	label: qsTr("α (slant)"),		min: -Infinity,	defaultValue: 0 }
			]
		},
		"SkewCauchy"				: {
			"default": [
				{ value: "skewCauchyXi",	label: qsTr("ξ (location)"),	min: -Infinity,	defaultValue: 0 },
				{ value: "skewCauchyOmega",	label: qsTr("ω (scale)"),		min: 0,			defaultValue: 1 },
				{ value: "skewCauchyAlpha",	label: qsTr("α (slant)"),		min: -Infinity,	defaultValue: 0 }
			]
		},
		"SkewT"						: {
			"default": [
				{ value: "skewTXi",		label: qsTr("ξ (location)"),	min: -Infinity,	defaultValue: 0 },
				{ value: "skewTOmega",	label: qsTr("ω (scale)"),		min: 0,			defaultValue: 1 },
				{ value: "skewTAlpha",	label: qsTr("α (slant)"),		min: -Infinity,	defaultValue: 0 },
				{ value: "skewTNu",		label: qsTr("ν (df)"),			min: 0,			defaultValue: 5 }
			]
		},
		"Beta"						: {
			"default": [
				{ value: "betaAlpha",	label: qsTr("α (shape)"),	min: 0,	defaultValue: 2 },
				{ value: "betaBeta",	label: qsTr("β (shape)"),	min: 0,	defaultValue: 2 }
			]
		},
		"BetaPrime"					: {
			"default": [
				{ value: "betaPrimeAlpha",	label: qsTr("α (shape)"),	min: 0,	defaultValue: 2 },
				{ value: "betaPrimeBeta",	label: qsTr("β (shape)"),	min: 0,	defaultValue: 2 }
			]
		},
		"CentralF"					: {
			"default": [
				{ value: "centralFNu1",	label: qsTr("ν₁ (df)"),	min: 0,	defaultValue: 5 },
				{ value: "centralFNu2",	label: qsTr("ν₂ (df)"),	min: 0,	defaultValue: 5 }
			]
		},
		"NoncentralF"				: {
			"default": [
				{ value: "noncentralFNu1",		label: qsTr("ν₁ (df)"),			min: 0,	defaultValue: 5 },
				{ value: "noncentralFNu2",		label: qsTr("ν₂ (df)"),			min: 0,	defaultValue: 5 },
				{ value: "noncentralFKappa",	label: qsTr("κ (noncentrality)"),	min: 0,	defaultValue: 1 }
			]
		},
		"ChiSquared"				: {
			"default": [
				{ value: "chiSquaredNu",	label: qsTr("ν (df)"),	min: 0,	defaultValue: 5 }
			]
		},
		"NoncentralChiSquared"		: {
			"default": [
				{ value: "noncentralChiSquaredNu",		label: qsTr("ν (df)"),				min: 0,	defaultValue: 5 },
				{ value: "noncentralChiSquaredKappa",	label: qsTr("κ (noncentrality)"),	min: 0,	defaultValue: 1 }
			]
		},
		"Exponential"				: {
			"parameterChoice": [
				{ label: qsTr("λ (rate)"),	value: "lambda"	},
				{ label: qsTr("β (scale)"),	value: "beta"	}
			],
			"lambda": [
				{ value: "exponentialLambda",	label: qsTr("λ (rate)"),	min: 0,	defaultValue: 1 }
			],
			"beta": [
				{ value: "exponentialBeta",		label: qsTr("β (scale)"),	min: 0,	defaultValue: 1 }
			]
		},
		"Gamma"						: {
			"parameterChoice": [
				{ label: qsTr("α (shape), θ (scale)"),	value: "theta"	},
				{ label: qsTr("α (shape), λ (rate)"),	value: "lambda"	},
				{ label: qsTr("α (shape), μ (mean)"),	value: "mu"		}
			],
			"theta": [
				{ value: "gammaAlpha",	label: qsTr("α (shape)"),	min: 0,	defaultValue: 2 },
				{ value: "gammaTheta",	label: qsTr("θ (scale)"),	min: 0,	defaultValue: 1 }
			],
			"lambda": [
				{ value: "gammaAlpha",	label: qsTr("α (shape)"),	min: 0,	defaultValue: 2 },
				{ value: "gammaLambda",	label: qsTr("λ (rate)"),	min: 0,	defaultValue: 1 }
			],
			"mu": [
				{ value: "gammaAlpha",	label: qsTr("α (shape)"),	min: 0,	defaultValue: 2 },
				{ value: "gammaMu",		label: qsTr("μ (mean)"),	min: 0,	defaultValue: 1 }
			]
		},
		"InverseGamma"				: {
			"parameterChoice": [
				{ label: qsTr("α (shape), θ (scale)"),	value: "theta"	},
				{ label: qsTr("α (shape), λ (rate)"),	value: "lambda"	},
				{ label: qsTr("α (shape), μ (mean)"),	value: "mu"		}
			],
			"theta": [
				{ value: "inverseGammaAlpha",	label: qsTr("α (shape)"),	min: 0,	defaultValue: 2 },
				{ value: "inverseGammaTheta",	label: qsTr("θ (scale)"),	min: 0,	defaultValue: 1 }
			],
			"lambda": [
				{ value: "inverseGammaAlpha",	label: qsTr("α (shape)"),	min: 0,	defaultValue: 2 },
				{ value: "inverseGammaLambda",	label: qsTr("λ (rate)"),	min: 0,	defaultValue: 1 }
			],
			"mu": [
				{ value: "inverseGammaAlpha",	label: qsTr("α (shape)"),	min: 0,	defaultValue: 2 },
				{ value: "inverseGammaMu",		label: qsTr("μ (mean)"),	min: 0,	defaultValue: 1 }
			]
		},
		"Gompertz"					: {
			"default": [
				{ value: "gompertzEta",		label: qsTr("η (shape)"),	min: 0,	defaultValue: 1 },
				{ value: "gompertzBeta",	label: qsTr("β (scale)"),	min: 0,	defaultValue: 1 }
			]
		},
		"LogLogistic"				: {
			"parameterChoice": [
				{ label: qsTr("μ (log location), σ (log scale)"),	value: "mu"		},
				{ label: qsTr("α (scale), β (shape)"),				value: "alpha"	}
			],
			"mu": [
				{ value: "logLogisticMu",		label: qsTr("μ (log location)"),	min: -Infinity,	defaultValue: 0 },
				{ value: "logLogisticSigma",	label: qsTr("σ (log scale)"),		min: 0,			defaultValue: 1 }
			],
			"alpha": [
				{ value: "logLogisticAlpha",	label: qsTr("α (scale)"),	min: 0,	defaultValue: 1 },
				{ value: "logLogisticBeta",		label: qsTr("β (shape)"),	min: 0,	defaultValue: 2 }
			]
		},
		"LogNormal"					: {
			"default": [
				{ value: "logNormalMu",		label: qsTr("μ (log mean)"),			min: -Infinity,	defaultValue: 0 },
				{ value: "logNormalSigma",	label: qsTr("σ (log std. deviation)"),	min: 0,			defaultValue: 1 }
			]
		},
		"Wald"						: {
			"parameterChoice": [
				{ label: qsTr("μ (mean), λ (shape)"),					value: "mu"	},
				{ label: qsTr("ν (drift), α (threshold), σ (noise)"),	value: "nu"	}
			],
			"mu": [
				{ value: "waldMu",		label: qsTr("μ (mean)"),		min: 0,	defaultValue: 1 },
				{ value: "waldLambda",	label: qsTr("λ (shape)"),		min: 0,	defaultValue: 1 }
			],
			"nu": [
				{ value: "waldNu",		label: qsTr("ν (drift)"),		min: 0,	defaultValue: 1 },
				{ value: "waldAlpha",	label: qsTr("α (threshold)"),	min: 0,	defaultValue: 1 },
				{ value: "waldSigma",	label: qsTr("σ (noise)"),		min: 0,	defaultValue: 1 }
			]
		},
		"Weibull"					: {
			"default": [
				{ value: "weibullShape",	label: qsTr("shape"),	min: 0,	defaultValue: 2 },
				{ value: "weibullScale",	label: qsTr("scale"),	min: 0,	defaultValue: 1 }
			]
		},
		"Amoroso"					: {
			"default": [
				{ value: "amorosoA",		label: qsTr("a (location)"),	min: -Infinity,	defaultValue: 0 },
				{ value: "amorosoTheta",	label: qsTr("θ (scale)"),		min: -Infinity,	defaultValue: 1 },
				{ value: "amorosoAlpha",	label: qsTr("α (shape)"),		min: 0,			defaultValue: 2 },
				{ value: "amorosoBeta",		label: qsTr("β (shape)"),		min: -Infinity,	defaultValue: 1 }
			]
		},
		"StretchedBeta"				: {
			"default": [
				{ value: "stretchedBetaAlpha",	label: qsTr("α (shape)"),	min: 0,			defaultValue: 2 },
				{ value: "stretchedBetaBeta",	label: qsTr("β (shape)"),	min: 0,			defaultValue: 2 },
				{ value: "stretchedBetaMin",	label: qsTr("min"),			min: -Infinity,	defaultValue: 0 },
				{ value: "stretchedBetaMax",	label: qsTr("max"),			min: -Infinity,	defaultValue: 1 }
			]
		},
		"Frechet"					: {
			"default": [
				{ value: "frechetAlpha",	label: qsTr("α (shape)"),		min: 0,			defaultValue: 2 },
				{ value: "frechetSigma",	label: qsTr("σ (scale)"),		min: 0,			defaultValue: 1 },
				{ value: "frechetTheta",	label: qsTr("θ (location)"),	min: -Infinity,	defaultValue: 0 }
			]
		},
		"Pareto"					: {
			"default": [
				{ value: "paretoAlpha",	label: qsTr("α (shape)"),	min: 0,	defaultValue: 2 },
				{ value: "paretoBeta",	label: qsTr("β (scale)"),	min: 0,	defaultValue: 1 }
			]
		},
		"Triangular"				: {
			"default": [
				{ value: "triangularA",	label: qsTr("a (min)"),		min: -Infinity,	defaultValue: 0   },
				{ value: "triangularB",	label: qsTr("b (max)"),		min: -Infinity,	defaultValue: 1   },
				{ value: "triangularC",	label: qsTr("c (mode)"),	min: -Infinity,	defaultValue: 0.5 }
			]
		},
		"Uniform"					: {
			"default": [
				{ value: "uniformMin",	label: qsTr("min"),	min: -Infinity,	defaultValue: 0 },
				{ value: "uniformMax",	label: qsTr("max"),	min: -Infinity,	defaultValue: 1 }
			]
		},
		"ShiftedExponential"		: {
			"parameterChoice": [
				{ label: qsTr("λ (rate), shift"),	value: "lambda"	},
				{ label: qsTr("β (scale), shift"),	value: "beta"	}
			],
			"lambda": [
				{ value: "shiftedExponentialLambda",	label: qsTr("λ (rate)"),	min: 0,			defaultValue: 1 },
				{ value: "shiftedExponentialShift",		label: qsTr("shift"),		min: -Infinity,	defaultValue: 0 }
			],
			"beta": [
				{ value: "shiftedExponentialBeta",	label: qsTr("β (scale)"),	min: 0,			defaultValue: 1 },
				{ value: "shiftedExponentialShift",	label: qsTr("shift"),		min: -Infinity,	defaultValue: 0 }
			]
		},
		"ShiftedLogNormal"			: {
			"default": [
				{ value: "shiftedLogNormalMu",		label: qsTr("μ (log mean)"),			min: -Infinity,	defaultValue: 0 },
				{ value: "shiftedLogNormalSigma",	label: qsTr("σ (log std. deviation)"),	min: 0,			defaultValue: 1 },
				{ value: "shiftedLogNormalShift",	label: qsTr("shift"),					min: -Infinity,	defaultValue: 0 }
			]
		},
		"ShiftedGamma"				: {
			"parameterChoice": [
				{ label: qsTr("α (shape), θ (scale), shift"),	value: "theta"	},
				{ label: qsTr("α (shape), λ (rate), shift"),	value: "lambda"	},
				{ label: qsTr("α (shape), μ (mean), shift"),	value: "mu"		}
			],
			"theta": [
				{ value: "shiftedGammaAlpha",	label: qsTr("α (shape)"),	min: 0,			defaultValue: 2 },
				{ value: "shiftedGammaTheta",	label: qsTr("θ (scale)"),	min: 0,			defaultValue: 1 },
				{ value: "shiftedGammaShift",	label: qsTr("shift"),		min: -Infinity,	defaultValue: 0 }
			],
			"lambda": [
				{ value: "shiftedGammaAlpha",	label: qsTr("α (shape)"),	min: 0,			defaultValue: 2 },
				{ value: "shiftedGammaLambda",	label: qsTr("λ (rate)"),	min: 0,			defaultValue: 1 },
				{ value: "shiftedGammaShift",	label: qsTr("shift"),		min: -Infinity,	defaultValue: 0 }
			],
			"mu": [
				{ value: "shiftedGammaAlpha",	label: qsTr("α (shape)"),	min: 0,			defaultValue: 2 },
				{ value: "shiftedGammaMu",		label: qsTr("μ (mean)"),	min: 0,			defaultValue: 1 },
				{ value: "shiftedGammaShift",	label: qsTr("shift"),		min: -Infinity,	defaultValue: 0 }
			]
		},
		"ShiftedInverseGamma"		: {
			"parameterChoice": [
				{ label: qsTr("α (shape), θ (scale), shift"),	value: "theta"	},
				{ label: qsTr("α (shape), λ (rate), shift"),	value: "lambda"	},
				{ label: qsTr("α (shape), μ (mean), shift"),	value: "mu"		}
			],
			"theta": [
				{ value: "shiftedInverseGammaAlpha",	label: qsTr("α (shape)"),	min: 0,			defaultValue: 2 },
				{ value: "shiftedInverseGammaTheta",	label: qsTr("θ (scale)"),	min: 0,			defaultValue: 1 },
				{ value: "shiftedInverseGammaShift",	label: qsTr("shift"),		min: -Infinity,	defaultValue: 0 }
			],
			"lambda": [
				{ value: "shiftedInverseGammaAlpha",	label: qsTr("α (shape)"),	min: 0,			defaultValue: 2 },
				{ value: "shiftedInverseGammaLambda",	label: qsTr("λ (rate)"),	min: 0,			defaultValue: 1 },
				{ value: "shiftedInverseGammaShift",	label: qsTr("shift"),		min: -Infinity,	defaultValue: 0 }
			],
			"mu": [
				{ value: "shiftedInverseGammaAlpha",	label: qsTr("α (shape)"),	min: 0,			defaultValue: 2 },
				{ value: "shiftedInverseGammaMu",		label: qsTr("μ (mean)"),	min: 0,			defaultValue: 1 },
				{ value: "shiftedInverseGammaShift",	label: qsTr("shift"),		min: -Infinity,	defaultValue: 0 }
			]
		},
		"ShiftedLogLogistic"		: {
			"parameterChoice": [
				{ label: qsTr("μ (log location), σ (log scale), shift"),	value: "mu"		},
				{ label: qsTr("α (scale), β (shape), shift"),				value: "alpha"	}
			],
			"mu": [
				{ value: "shiftedLogLogisticMu",	label: qsTr("μ (log location)"),	min: -Infinity,	defaultValue: 0 },
				{ value: "shiftedLogLogisticSigma",	label: qsTr("σ (log scale)"),		min: 0,			defaultValue: 1 },
				{ value: "shiftedLogLogisticShift",	label: qsTr("shift"),				min: -Infinity,	defaultValue: 0 }
			],
			"alpha": [
				{ value: "shiftedLogLogisticAlpha",	label: qsTr("α (scale)"),	min: 0,			defaultValue: 1 },
				{ value: "shiftedLogLogisticBeta",	label: qsTr("β (shape)"),	min: 0,			defaultValue: 2 },
				{ value: "shiftedLogLogisticShift",	label: qsTr("shift"),		min: -Infinity,	defaultValue: 0 }
			]
		},
		"ShiftedWald"				: {
			"parameterChoice": [
				{ label: qsTr("μ (mean), λ (shape), shift"),					value: "mu"	},
				{ label: qsTr("ν (drift), α (threshold), σ (noise), shift"),	value: "nu"	}
			],
			"mu": [
				{ value: "shiftedWaldMu",		label: qsTr("μ (mean)"),		min: 0,			defaultValue: 1 },
				{ value: "shiftedWaldLambda",	label: qsTr("λ (shape)"),		min: 0,			defaultValue: 1 },
				{ value: "shiftedWaldShift",	label: qsTr("shift"),			min: -Infinity,	defaultValue: 0 }
			],
			"nu": [
				{ value: "shiftedWaldNu",		label: qsTr("ν (drift)"),		min: 0,			defaultValue: 1 },
				{ value: "shiftedWaldAlpha",	label: qsTr("α (threshold)"),	min: 0,			defaultValue: 1 },
				{ value: "shiftedWaldSigma",	label: qsTr("σ (noise)"),		min: 0,			defaultValue: 1 },
				{ value: "shiftedWaldShift",	label: qsTr("shift"),			min: -Infinity,	defaultValue: 0 }
			]
		},
		"ShiftedWeibull"			: {
			"default": [
				{ value: "shiftedWeibullShape",	label: qsTr("shape"),	min: 0,			defaultValue: 2 },
				{ value: "shiftedWeibullScale",	label: qsTr("scale"),	min: 0,			defaultValue: 1 },
				{ value: "shiftedWeibullShift",	label: qsTr("shift"),	min: -Infinity,	defaultValue: 0 }
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
