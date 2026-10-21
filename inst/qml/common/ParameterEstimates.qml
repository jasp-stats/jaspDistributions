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
	property bool estimationEnabled: true

	title: qsTr("Advanced options")
	info: qsTr("Options for estimating the free (not fixed) parameters of the distribution, and for repeatability of random results.")
	columns: 2

	RadioButtonGroup
	{
		id:			parameterEstimationMethod
		name:		"parameterEstimationMethod"
		enabled:	estimationEnabled
		title:	qsTr("Parameter estimation method")
		info:	qsTr("If available, parameter estimates are calculated analytically. If not, then parameters are estimated using numerical optimization of the log-likelihood function.")
		RadioButton
		{
			label:		qsTr("Unconstrained")
			value:		"unconstrained"
			checked:	true
			info:		qsTr("Optimize the log-likelihood on the unconstrained parameter space. For example, a parameter that is always positive is optimized on the log scale. The optimization method used is 'BFGS'.")
		}
		RadioButton
		{
			label:	qsTr("Constrained")
			value:	"constrained"
			info:	qsTr("Optimize the log-likelihood on the constrained parameter space. The optimization method used is 'L-BFGS-B'.")
		}
	}

	RadioButtonGroup
	{
		name:		"parameterUncertaintyMethod"
		enabled:	estimationEnabled
		title:	qsTr("Parameter uncertainty quantification method")
		info:	qsTr("How to quantify uncertainty about the parameters.")
		RadioButton
		{
			label:		qsTr("Default")
			value:		"default"
			checked:	true
			info:		qsTr("If available, analytic SE and CIs are calculated, otherwise, they are calculated using normal theory based on numerical estimate of the Hessian.")

			CheckBox
			{
				name:		"parameterUncertaintySymmetricCi"
				label:		qsTr("Symmetric CI")
				enabled:	parameterEstimationMethod.value === "unconstrained"
				info:		qsTr("Should the CI be computed on the unconstrained space and then CI bounds transformed into the original parameter space, or should the CIs be computed on the constrained space using transformed Hessian. The latter produces CIs that are symmetric around the point estimate but may, in some extreme cases, produce CIs extending beyond support of the parameter.")
			}
		}
		RadioButton
		{
			label:				qsTr("Bootstrap")
			value:				"bootstrap"
			info:				qsTr("SE and CIs are calculated using parametric bootstrap.")
			childrenOnSameRow:	true
			IntegerField { name: "parameterUncertaintyBootstrapSamples"; label: qsTr("with"); afterLabel: qsTr("bootstrap samples"); defaultValue: 1000; min: 100 }
		}
	}

	SetSeed { }
}
