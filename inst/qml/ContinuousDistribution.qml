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
import "./common" as Common

Form
{
	columns: 1
	info: qsTr("Specify a continuous distribution, optionally estimate its parameters from data, display it, and assess its fit to data.")

	Group
	{
		title: qsTr("Specify distribution")
		Common.DistributionSpecification { }
	}

	DropDown
	{
		id:						fitVariable
		name:					"fitVariable"
		label:					qsTr("Estimate parameters from")
		addEmptyValue:			true
		placeholderText:		qsTr("<Do not estimate>")
		showVariableTypeIcon:	true
		allowedColumns:			["scale"]
		// the column simulated from the fitted distribution depends on the fit, so it cannot be used for fitting
		source:					[{ isDataSetVariables: true, discard: { values: [simulateData.fittedColumn] } }]
		info:					qsTr("Variable used to estimate the free (not fixed) parameters of the distribution. Leave empty to use the specified parameter values as they are.")
	}

	CheckBox
	{
		name:		"parameterTable"
		label:		qsTr("Parameter table")
		checked:	true
		info:		qsTr("Displays a table with the parameter values.")

		CheckBox
		{
			name:		"parameterTableUncertainty"
			label:		qsTr("Quantify uncertainty")
			enabled:	fitVariable.currentValue !== ""
			info:		qsTr("Displays the uncertainty about the estimated parameters (e.g., standard errors and confidence intervals). The method is set in the advanced options.")
			CIField { name: "parameterTableCiLevel"; label: qsTr("Confidence level") }
		}
	}

	Common.ShowDistribution { }

	Common.AssessFit
	{
		hasFitVariable: fitVariable.currentValue !== ""
	}

	Common.SimulateData
	{
		id:				simulateData
		hasFitVariable:	fitVariable.currentValue !== ""
	}

	Common.ParameterEstimates
	{
		estimationEnabled: fitVariable.currentValue !== ""
	}
}
