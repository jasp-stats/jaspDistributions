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
	info: qsTr("Demonstration of the Normal (Gaussian) distribution")
	Section
	{
		expanded: true
		title: qsTr("Show Distribution")
		info: qsTr("Displays theoretical Normal distribution, given specified parameter values.")
		Group
		{
			Layout.columnSpan: 2
			DropDown
			{
				name: "parametrization"
				id:   parametrization
				indexDefaultValue: 0
				label: qsTr("Parameters")
				infoLabel: "<h3>" + label + "</h3>"
				values: [
					{ label: "μ, σ²", value: "sigma2",	info: qsTr("The Normal distribution is specified using the mean and variance parameters") },
					{ label: "μ, σ",  value: "sigma",	info: qsTr("The Normal distribution is specified using the mean and standard deviation parameters") },
					{ label: "μ, τ",  value: "tau",		info: qsTr("The Normal distribution is specified using the mean and precision parameters") },
					{ label: "μ, κ",  value: "kappa",	info: qsTr("The Normal distribution is specified using the mean and square root of precision parameters") }
				]
			}

			Group
			{
				columns: 2
				Text { text: qsTr("Mean:") }
				DoubleField{ name:  "mu"; label: qsTr("μ"); id: mu; negativeValues: true }

				Text { text: [qsTr("Variance:"), qsTr("Std. deviation:"), qsTr("Precision:"), qsTr("Square root of precision:")][parametrization.currentIndex] }
				DoubleField
				{
					name: "varValue"
					id:    varValue
					label: ["σ²", "σ ", "τ", "κ "][parametrization.currentIndex]
					negativeValues: false
					defaultValue: 1
				}
			}

		}

		Group
		{
			title: qsTr("Display")
			CheckBox{ label: qsTr("Explanatory text"); name: "explanatoryText"; info: qsTr("Displays explanatory text")}
			CheckBox{ label: qsTr("Parameters, support, and moments"); name: "parsSupportMoments"; info: qsTr("Displays the definition of parameters, the support of the random variable, and the moments of the theoretical distribution") }
			CheckBox{ label: qsTr("Formulas"); name: "formulas"; visible: false}
			CheckBox
			{
				label: qsTr("Probability density function")
				id: plotPDF
				name: "plotPDF"
				checked: true
				info: qsTr("Displays the probability density plot")
			}

			CheckBox{
				label: qsTr("Cumulative distribution function")
				id: plotCDF
				name: "plotCDF"
				info: qsTr("Displays the cumulative distribution plot")
			}
			CheckBox{
				label: qsTr("Quantile function")
				name: "plotQF"
				info: qsTr("Displays the quantile plot")
			}
		}

		LD.LDOptions { enabled: plotPDF.checked || plotCDF.checked }
	}

	LD.LDGenerateDisplayData
	{
		distributionName		: "Normal"
		formula					: mu.label + " = " + mu.value + ", " + varValue.label + " = " + varValue.value
		enabled					: dataSetInfo.dataAvailable
	}

	LD.LDEstimateParameters { enabled: dataSetInfo.dataAvailable; includeUnbiased: true }

	LD.LDAssessFit{ enabled: dataSetInfo.dataAvailable; includeShapiroWilk: true }
}
