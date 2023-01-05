//
// Copyright (C) 2013-2018 University of Amsterdam
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

import "./common" as LD

Form
{
	Section
	{
		expanded: true
		title: qsTr("Show Distribution")
		Group
		{
			Layout.columnSpan: 2
			DropDown
			{
				name: "parametrization"
				id:   parametrization
				indexDefaultValue: 0
				label: qsTr("Parameters")
				values: [
					{ label: "μ, λ",  value: "mulambda"		},
					{ label: "α, ν",  value: "alphanu"		},
					{ label: "α, σ",  value: "alphasigma"	},
					{ label: "ν, σ",  value: "nusigma"		}
				]
				visible: true
			}

			Group
			{
				columns: 2
				Text { text: [qsTr("Mean:"), qsTr("Threshold:"), qsTr("Threshold:"), qsTr("Drift rate:")][parametrization.currentIndex] }
				DoubleField
				{
					name: "par1"
					label: ["μ", "α", "α", "ν"][parametrization.currentIndex]
					id: par1
					negativeValues: false
					defaultValue: 1
				}
				Text { text: [qsTr("Shape:"), qsTr("Drift rate:"), qsTr("Noise:"), qsTr("Noise:")][parametrization.currentIndex]}
				DoubleField
				{
					name: "par2"
					label: ["λ", "ν", "σ", "σ"][parametrization.currentIndex]
					id: par2
					negativeValues: false
					defaultValue: 1
				}
			}
		}

		Group
		{
			title: qsTr("Display")
			CheckBox{ label: qsTr("Explanatory text"); name: "explanatoryText"}
			CheckBox{ label: qsTr("Parameters, support, and moments"); name: "parsSupportMoments" }
			CheckBox{ label: qsTr("Formulas"); name: "formulas"; visible: false}
			CheckBox{ label: qsTr("Probability density function"); id: plotPDF; name: "plotPDF"; checked: true }
			CheckBox{ label: qsTr("Cumulative distribution function"); id: plotCDF; name: "plotCDF"; checked: false }
			CheckBox{ label: qsTr("Quantile function"); id: plotQF; name: "plotQF"; checked: false }
		}

		LD.LDOptions
		{
			enabled				: plotPDF.checked || plotCDF.checked
			negativeValues		: false
			intervalMinmaxMin	: 1
			intervalMinmaxMax	: 2
			intervalLowerMax	: 1
			intervalUpperMin	: 1
		}
	}

	LD.LDGenerateDisplayData
	{
		distributionName		: "Gamma"
		formula					: par1.label + " = " + par1.value + ", " + par2.label + " = " + par2.value
		enabled					: mainWindow.dataAvailable
	}

	LD.LDEstimateParameters { enabled: mainWindow.dataAvailable }

	LD.LDAssessFit { enabled: mainWindow.dataAvailable }
}
