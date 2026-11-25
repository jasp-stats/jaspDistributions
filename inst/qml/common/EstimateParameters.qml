//
// Copyright (C) 2013-2026 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//
import QtQuick
import QtQuick.Layouts
import JASP
import JASP.Controls

Section
{
	// Were (some of) the parameters estimated from data?
	property bool hasFitVariable: estimationVariable.currentValue !== ""

	title:		dataSetInfo.dataAvailable ? qsTr("Estimate Parameters") : qsTr("Estimate Parameters") + " - " + qsTr("[requires a loaded data set]")
	enabled:	dataSetInfo.dataAvailable
	info:		qsTr("Estimate the free (not fixed) parameters of the distribution from data, and quantify the uncertainty about them.")
	columns:	2

	DropDown
	{
		id:						estimationVariable
		name:					"estimationVariable"
		label:					qsTr("Estimate parameters from")
		Layout.columnSpan:		2
		addEmptyValue:			true
		placeholderText:		qsTr("<Do not estimate>")
		showVariableTypeIcon:	true
		allowedColumns:			["scale"]
		source:					[{ isDataSetVariables: true }]
		info:					qsTr("Variable used to estimate the free (not fixed) parameters of the distribution. Leave empty to use the specified parameter values as they are.")
	}

	Group
	{
		title:		qsTr("Parameter estimation")
		enabled:	hasFitVariable

		DropDown
		{
			name:		"parameterEstimator"
			label:		qsTr("Method")
			info:		qsTr("How the parameters are estimated. Default uses maximum spacing for shifted distributions, and otherwise the bias-corrected estimator if available (e.g., normal, exponential), and maximum likelihood if not.")
			values:
			[
				{ label: qsTr("Default"),				value: "default"			},
				{ label: qsTr("Maximum likelihood"),	value: "maximumLikelihood"	},
				{ label: qsTr("Maximum spacing"),		value: "maximumSpacing"		}
			]
		}

		DropDown
		{
			name:		"parameterEstimatorOptimizationSpace"
			label:		qsTr("Optimize on")
			info:		qsTr("Parameter space on which the estimates are optimized if they cannot be calculated analytically: unconstrained (e.g., positive parameters on the log scale; 'BFGS') or constrained ('L-BFGS-B').")
			values:
			[
				{ label: qsTr("unconstrained space"),	value: "unconstrained"	},
				{ label: qsTr("constrained space"),		value: "constrained"	}
			]
		}
	}

	// Each inference method is a value of a DropDown with its own settings shown below it,
	// so that further methods can be added without changing the layout.
	Group
	{
		title:		qsTr("Parameter uncertainty")
		enabled:	hasFitVariable

		CheckBox
		{
			name:		"parameterUncertainty"
			label:		qsTr("Quantify uncertainty")
			info:		qsTr("Displays the uncertainty about the estimated parameters (e.g., standard errors and confidence intervals) in the parameter table.")

			CIField { name: "parameterUncertaintyCiLevel"; label: qsTr("Confidence level") }

			DropDown
			{
				id:		parameterUncertaintyMethod
				name:	"parameterUncertaintyMethod"
				label:	qsTr("Method")
				info:	qsTr("How the standard errors and confidence intervals are calculated. Default uses exact intervals if available (e.g., normal, uniform), and normal theory otherwise. Normal theory assumes maximum likelihood estimates. Bootstrap is unreliable for parameters that bound the support (e.g., of the uniform).")
				values:
				[
					{ label: qsTr("Default"),		value: "default"		},
					{ label: qsTr("Normal theory"),	value: "normalTheory"	},
					{ label: qsTr("Bootstrap"),		value: "bootstrap"		}
				]
			}

			DropDown
			{
				name:		"parameterUncertaintyNormalTheorySpace"
				label:		qsTr("Compute intervals on")
				visible:	parameterUncertaintyMethod.value === "normalTheory"
				info:		qsTr("Compute the confidence intervals on the unconstrained parameter space and transform their bounds back, or directly on the constrained parameter space. The latter produces intervals that are symmetric around the estimate, but that may extend beyond the support of the parameter.")
				values:
				[
					{ label: qsTr("unconstrained space"),	value: "unconstrained"	},
					{ label: qsTr("constrained space"),		value: "constrained"	}
				]
			}

			Group
			{
				visible: parameterUncertaintyMethod.value === "bootstrap"

				IntegerField
				{
					name:			"parameterUncertaintyBootstrapSamples"
					label:			qsTr("Samples")
					defaultValue:	1000
					min:			100
					info:			qsTr("Number of bootstrap samples on which the parameters are re-estimated.")
				}

				DropDown
				{
					name:	"parameterUncertaintyBootstrapType"
					label:	qsTr("Type")
					info:	qsTr("Draw the bootstrap samples from the fitted distribution (parametric) or resample the data (nonparametric).")
					values:
					[
						{ label: qsTr("parametric"),	value: "parametric"		},
						{ label: qsTr("nonparametric"),	value: "nonparametric"	}
					]
				}

				DropDown
				{
					name:	"parameterUncertaintyBootstrapCiMethod"
					label:	qsTr("Confidence interval")
					info:	qsTr("Percentile: quantiles of the bootstrap estimates. Basic: bootstrap estimates reflected around the estimate.")
					values:
					[
						{ label: qsTr("percentile"),	value: "percentile"	},
						{ label: qsTr("basic"),			value: "basic"		}
					]
				}
			}
		}
	}
}
