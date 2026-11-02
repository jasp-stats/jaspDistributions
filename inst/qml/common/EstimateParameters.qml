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
	property bool hasFitVariable: fitVariable.currentValue !== ""

	title:		dataSetInfo.dataAvailable ? qsTr("Estimate Parameters") : qsTr("Estimate Parameters") + " - " + qsTr("[requires a loaded data set]")
	enabled:	dataSetInfo.dataAvailable
	info:		qsTr("Estimate the free (not fixed) parameters of the distribution from data, and quantify the uncertainty about them.")
	columns:	2

	DropDown
	{
		id:						fitVariable
		name:					"fitVariable"
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

		CheckBox
		{
			name:		"parameterEstimatorBiasCorrected"
			label:		qsTr("Bias-corrected (if available)")
			checked:	true
			info:		qsTr("Use the bias-corrected estimator if it is available for the distribution (e.g., normal, log-normal, exponential, uniform). Otherwise, the parameters are estimated by maximum likelihood.")
		}

		DropDown
		{
			name:		"parameterEstimatorMleSpace"
			label:		qsTr("Optimize on")
			info:		qsTr("If the maximum likelihood estimates cannot be calculated analytically, the log-likelihood is optimized numerically: on the unconstrained parameter space (e.g., a parameter that is always positive is optimized on the log scale), using the 'BFGS' method, or on the constrained parameter space, using the 'L-BFGS-B' method.")
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
			name:		"parameterTableUncertainty"
			label:		qsTr("Quantify uncertainty")
			info:		qsTr("Displays the uncertainty about the estimated parameters (e.g., standard errors and confidence intervals) in the parameter table.")

			CIField { name: "parameterTableCiLevel"; label: qsTr("Confidence level") }

			DropDown
			{
				id:		parameterInferenceMethod
				name:	"parameterInferenceMethod"
				label:	qsTr("Method")
				info:	qsTr("How the standard errors and confidence intervals of the estimated parameters are calculated. Default uses exact intervals if they are available for the distribution (e.g., normal, uniform), and normal theory otherwise. Normal theory uses the curvature of the log-likelihood at the estimates, and is therefore valid only for maximum likelihood estimates. Bootstrap re-estimates the parameters on simulated or resampled data; it is unreliable for parameters that determine the bounds of the support (e.g., of the uniform distribution).")
				values:
				[
					{ label: qsTr("Default"),		value: "default"		},
					{ label: qsTr("Normal theory"),	value: "normalTheory"	},
					{ label: qsTr("Bootstrap"),		value: "bootstrap"		}
				]
			}

			DropDown
			{
				name:		"parameterInferenceNormalTheorySpace"
				label:		qsTr("Compute intervals on")
				visible:	parameterInferenceMethod.value === "normalTheory"
				info:		qsTr("Compute the confidence intervals on the unconstrained parameter space and transform their bounds back, or directly on the constrained parameter space. The latter produces intervals that are symmetric around the estimate, but that may extend beyond the support of the parameter.")
				values:
				[
					{ label: qsTr("unconstrained space"),	value: "unconstrained"	},
					{ label: qsTr("constrained space"),		value: "constrained"	}
				]
			}

			Group
			{
				visible: parameterInferenceMethod.value === "bootstrap"

				IntegerField
				{
					name:			"parameterInferenceBootstrapSamples"
					label:			qsTr("Samples")
					defaultValue:	1000
					min:			100
					info:			qsTr("Number of bootstrap samples on which the parameters are re-estimated.")
				}

				DropDown
				{
					name:	"parameterInferenceBootstrapType"
					label:	qsTr("Type")
					info:	qsTr("Draw the bootstrap samples from the fitted distribution (parametric), or by resampling the data with replacement (nonparametric).")
					values:
					[
						{ label: qsTr("parametric"),	value: "parametric"		},
						{ label: qsTr("nonparametric"),	value: "nonparametric"	}
					]
				}

				DropDown
				{
					name:	"parameterInferenceBootstrapCiMethod"
					label:	qsTr("Confidence interval")
					info:	qsTr("Percentile intervals are the quantiles of the bootstrap estimates. Basic intervals reflect the bootstrap estimates around the estimate. Both respect the support of the parameters.")
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
