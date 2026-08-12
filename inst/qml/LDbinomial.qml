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
	info: qsTr("Demonstration of the Binomial distribution.")
	Section
	{
		expanded: true
		title: qsTr("Show Distribution")
		info: qsTr("Displays theoretical Binomial distribution, given specified parameter values.")
		Group
		{
			title: qsTr("Free parameter")
			columns: 2
			Text { text: qsTr("Probability of success:") }
			DoubleField
			{
				name: "prob"; label: qsTr("p")
				info: qsTr("probability of \"success\". This parameter can be freely estimated from the data.")
				id: prob; min: 0; max: 1; defaultValue: 0.5
			}
		}

		Group
		{
			title: qsTr("Fixed parameter")
			columns: 2
			Text { text: qsTr("Number of trials:") }
			IntegerField
			{
				name: "size"; label: qsTr("n")
				info: qsTr("number of trials. This parameter is fixed when fitting the data.")
				id: size; defaultValue: 10
			}
		}

		Group
		{
			title: qsTr("Display")
			CheckBox{ label: qsTr("Explanatory text"); name: "explanatoryText"; info: qsTr("Displays explanatory text") }
			CheckBox{ label: qsTr("Parameters, support, and moments"); name: "parsSupportMoments"; info: qsTr("Displays the definition of parameters, the support of the random variable, and the moments of the theoretical distribution") }
			CheckBox{ label: qsTr("Formulas"); name: "formulas"; visible: false}
			CheckBox{ label: qsTr("Probability mass function"); id: plotPMF; name: "plotPMF"; checked: true; info: qsTr("Displays the probability mass plot") }
			CheckBox{ label: qsTr("Cumulative distribution function"); id: plotCMF; name: "plotCMF"; checked: false; info: qsTr("Displays the cumulative distribution plot") }
		}

		Group
		{
			title: qsTr("Options")
			enabled: plotPMF.checked || plotCMF.checked

			Row
			{
				spacing: jaspTheme.columnGroupSpacing
				DoubleField
				{
					name: "min_x"; label: qsTr("Range of x from"); id: min_x;
					defaultValue: 0; min: 0; max: parseFloat(max_x.value)
					info: qsTr("Defines the limits of the x-axis of the probability density plot and cumulative distribution plot.")
				}
				DoubleField
				{
					name: "max_x"; label: qsTr("to"); id: max_x;
					defaultValue: parseFloat(size.value); min: parseFloat(min_x.value); max: parseFloat(size.value)
				}
			}

			Group
			{
				title: qsTr("Highlight")
				Group
				{
					columns: 2
					CheckBox{ name: "highlightDensity"; label: qsTr("Mass"); id: highlightDensity; info: qsTr("Highlights the probability density on the probability density plot and cumulative distribution plot at specified values of x") }
					CheckBox{ name: "highlightProbability"; label: qsTr("Cumulative Probability"); id: highlightProbability; info: qsTr("Highlights the probability in between the specified values of x in the density plot (area under the curve), and highlights the cumulative probability at the specified values in the cumulative distribution plot") }
				}
				Row
				{
					spacing: jaspTheme.columnGroupSpacing
					IntegerField
					{
						name: "min"; label: qsTr("Interval"); afterLabel: qsTr("≤ X ≤"); id: min;
						info: qsTr("Select the bounds of the ordered set to display: Density is highlighted at the lower and upper bounds, the probability is displayed for the specified interval.")
						negativeValues: false; defaultValue: 0; max: parseInt(max.value)
					}
					IntegerField
					{
						name: "max"; label: ""; id: max;
						min: parseInt(min.value); defaultValue: parseInt(size.value); max: parseInt(size.value)
					}
				}
			}
		}
	}

	LD.LDGenerateDisplayData
	{
		distributionName		: "Binomial"
		formula					: "p = " + prob.value + ", n = " + size.value
		histogramIsBarPlot		: true
		allowOnlyScaleColumns	: true
		suggestScaleColumns		: true
		enabled					: dataSetInfo.dataAvailable
	}

	LD.LDEstimateParameters { enabled: dataSetInfo.dataAvailable }

	LD.LDAssessFit { enabled: dataSetInfo.dataAvailable; distributionType: "counts" }
}
