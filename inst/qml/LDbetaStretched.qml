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
import QtQuick			2.8
import QtQuick.Layouts	1.3
import JASP.Controls	1.0
import JASP.Widgets		1.0

import "./common" as LD

Form
{
	Section
	{
		expanded: true
		title: qsTr("Show Distribution")
		Group
		{
			title: qsTr("Free parameters")
			columns: 2
			Text { text: qsTr("Shape:") }
			DoubleField{ name: "alpha"; label: qsTr("α"); id: alpha; negativeValues: false; defaultValue: 1 }
			Text { text: qsTr("Shape:") }
			DoubleField{ name: "beta";  label: qsTr("β"); id: beta; negativeValues: false; defaultValue: 1 }
		}

		Group
		{
			title: qsTr("Fixed parameters")
			columns: 2
			Text { text: qsTr("Lower bound:") }
			DoubleField{ name: "lowerBoundPar"; label: qsTr("a"); id: lowerBound; negativeValues: true; max: parseFloat(upperBound.value); defaultValue: 0 }
			Text { text: qsTr("Upper bound:") }
			DoubleField{ name: "upperBoundPar";  label: qsTr("b"); id: upperBound; min: parseFloat(lowerBound.value); defaultValue: 1 }
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
			rangeMinX			: 0
			rangeMaxX			: 1
			intervalLowerMax	: .5
			intervalUpperMin	: .5
		}
	}

	LD.LDGenerateDisplayData
	{
		distributionName	: "Stretched Beta"
		formula				: "α = " + alpha.value + ", β = " + beta.value + ", a = " + lowerBound.value + ", b = " + upperBound.value
		enabled				: dataSetInfo.dataAvailable
	}

	LD.LDEstimateParameters { enabled: dataSetInfo.dataAvailable }

	LD.LDAssessFit { enabled: dataSetInfo.dataAvailable }
}
