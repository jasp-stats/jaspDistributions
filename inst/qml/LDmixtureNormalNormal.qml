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
			columns: 4

			Text { text: qsTr("Mean 1:") }
			DoubleField{ name: "mu1";	label: qsTr("μ₁");	id: mu1;		negativeValues: true;	defaultValue: -3	}

			Text { text: qsTr("Std. deviation 1:") }
			DoubleField{ name: "sigma1"; label: qsTr("σ₁"); id: sigma1;		negativeValues: false;	defaultValue: 1		}

			Text { text: qsTr("Mean 2:") }
			DoubleField{ name: "mu2";	label: qsTr("μ₂");	id: mu2;		negativeValues: true;	defaultValue: 3	}

			Text { text: qsTr("Std. deviation 2:") }
			DoubleField{ name: "sigma2"; label: qsTr("σ₂"); id: sigma2;		negativeValues: false;	defaultValue: 1		}

			Text { text: qsTr("Probability of component:") }
			DoubleField{ name: "pi";	label: qsTr("π");	id: pi;			min: 0; max: 1;			defaultValue: 0.5	}

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

		LD.LDOptions { enabled: plotPDF.checked || plotCDF.checked; rangeMinX: -5; rangeMaxX: 5; intervalMinmaxMin: 0; intervalMinmaxMax: 3 }
	}

	LD.LDGenerateDisplayData
	{
		distributionName		: "Mixture Normal and Normal"
		formula					: pi.label + " = " + pi.value + ", " + mu1.label + " = " + mu1.value + ", " + sigma1.label + " = " + sigma1.value + ", " + mu2.label + " = " + mu2.value + ", " + sigma2.label + " = " + sigma2.value
		enabled					: mainWindow.dataAvailable
	}

	LD.LDEstimateParameters { enabled: mainWindow.dataAvailable }

	LD.LDAssessFit { enabled: mainWindow.dataAvailable }
}
