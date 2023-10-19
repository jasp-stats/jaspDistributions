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
			title: "Parameters"
			Layout.columnSpan: 2
			columns: 2
			Text { text: qsTr("Minimum:") }
			DoubleField{ name: "a";	label: qsTr("a"); id: a; negativeValues: true; max: b.value; defaultValue: -1 }

			Text { text: qsTr("Maximum:") }
			DoubleField{ name: "b"; label: qsTr("b"); id: b; negativeValues: true; min: a.value; defaultValue: 1 }

			Text { text: qsTr("Mode:") }
			DoubleField{ name: "c"; label: qsTr("c"); id: c; negativeValues: true; min: a.value; max: b.value; defaultValue: 0 }

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

		LD.LDOptions { enabled: plotPDF.checked || plotCDF.checked; rangeMinX: -1; rangeMaxX: 1 }
	}

	LD.LDGenerateDisplayData
	{
		distributionName		: "Triangular"
		formula					: a.label + " = " + a.value + ", " + b.label + " = " + b.value + ", " + c.label + " = " + c.value
		enabled					: dataSetInfo.dataAvailable
	}

	LD.LDEstimateParameters { enabled: dataSetInfo.dataAvailable }

	LD.LDAssessFit { enabled: dataSetInfo.dataAvailable }
}
