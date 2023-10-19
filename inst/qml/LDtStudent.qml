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
			title: qsTr("Parameters")
			Group
			{
				columns: 2
				Text { text: qsTr("Degrees of freedom:") }
				DoubleField{ name: "df"; label: qsTr("df"); id: df; min: 1; defaultValue: 1 }

				Text { text: qsTr("Location:") }
				DoubleField{ name: "location";	label: qsTr("μ"); id: location;	 negativeValues: true;  defaultValue: 0 }

				Text { text: qsTr("Scale:") }
				DoubleField{ name: "scale"; label: qsTr("σ"); id: scale; negativeValues: false; defaultValue: 1 }
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

		LD.LDOptions { enabled: plotPDF.checked || plotCDF.checked }

	}

	LD.LDGenerateDisplayData
	{
		distributionName		: "Student's t"
		formula					: df.label + " = " + df.value + ", " + location.label + " = " + location.value + ", " + scale.label + " = " + scale.value
		enabled					: dataSetInfo.dataAvailable
	}

	LD.LDEstimateParameters { enabled: dataSetInfo.dataAvailable }

	LD.LDAssessFit { enabled: dataSetInfo.dataAvailable }
}
