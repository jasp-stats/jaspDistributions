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
import QtQuick 2.8
import QtQuick.Layouts 1.3
import JASP.Controls 1.0
import JASP.Widgets 1.0

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
			columns: 4
			Text { text: qsTr("Location:") }
			DoubleField{ name: "a";		label: qsTr("a"); id: a; negativeValues: true; }
			Text { text: qsTr("Scale:") }
			DoubleField{ name: "theta"; label: qsTr("θ"); id: theta; negativeValues: true; defaultValue: 1 }
			Text { text: qsTr("Shape:") }
			DoubleField{ name: "alpha"; label: qsTr("α"); id: alpha; negativeValues: false; defaultValue: 1 }
			Text { text: qsTr("Shape:") }
			DoubleField{ name: "beta";  label: qsTr("β"); id: beta; negativeValues: false; defaultValue: 1 }

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

		LD.LDOptions { enabled: plotPDF.checked || plotCDF.checked; rangeMinX: -3; rangeMaxX: 3 }
	}

	LD.LDGenerateDisplayData
	{
		distributionName		: "Amoroso"
		formula					: a.label + " = " + a.value + ", " + theta.label + " = " + theta.value + ", " + alpha.label + " = " + alpha.value + ", " + beta.label + " = " + beta.value
		enabled					: mainWindow.dataAvailable
	}

	LD.LDEstimateParameters { enabled: mainWindow.dataAvailable }

	LD.LDAssessFit { enabled: mainWindow.dataAvailable }
}
