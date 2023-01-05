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
			columns: 4
			Text { text: qsTr("Location:") }
			DoubleField{ name: "mu";	 label: qsTr("μ");		id: mu; negativeValues: true; defaultValue: 0 }

			Text { text: qsTr("Kurtosis:") }
			DoubleField{ name: "p";		 label: qsTr("p");		id: p; negativeValues: false; defaultValue: 2}

			Text { text: qsTr("Scale:") }
			DoubleField{ name: "sigma";  label: qsTr("σ");		id: sigma; negativeValues: false; defaultValue: 1}

			Text { text: qsTr("Kurtosis:") }
			DoubleField{ name: "q";		 label: qsTr("q");		id: q; negativeValues: false; defaultValue: 1}

			Text { text: qsTr("Skewness:") }
			DoubleField{ name: "lambda"; label: qsTr("λ"); id: lambda; min: -1; max: 1; defaultValue: 0}
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
		distributionName		: "Skewed generalized t"
		formula					: "μ = " + mu.value + ", σ = " + sigma.value + ", λ = " + lambda.value + ", p = " + p.value + ", q = " + q.value
		enabled					: mainWindow.dataAvailable
	}

	LD.LDEstimateParameters { enabled: mainWindow.dataAvailable }

	LD.LDAssessFit { enabled: mainWindow.dataAvailable }
}
