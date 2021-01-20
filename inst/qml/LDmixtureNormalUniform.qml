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
			title: qsTr("Free parameters")
			columns: 2

			Text { text: qsTr("Mean :") }
			DoubleField{ name: "mu";	label: qsTr("μ");			id: mu;					negativeValues: true;		defaultValue: 0		}

			Text { text: qsTr("Std. deviation :") }
			DoubleField{ name: "sigma"; label: qsTr("σ");			id: sigma;				negativeValues: false;		defaultValue: 1		}

			Text { text: qsTr("Probability of normal component:") }
			DoubleField{ name: "pi";	label: qsTr("π");			id: pi;					min: 0; max: 1;				defaultValue: 0.5	}

		}

		Group
		{
			title: qsTr("Fixed parameters")
			columns: 2
			Text { text: qsTr("Minimum of uniform:") }
			DoubleField{ name: "lowerBoundPar";	label: qsTr("a");	id: lowerBoundPar;		negativeValues: true; max: upperBoundPar.value;	defaultValue: -10	}

			Text { text: qsTr("Maximum of uniform:") }
			DoubleField{ name: "upperBoundPar"; label: qsTr("b");	id: upperBoundPar;		min: lowerBoundPar.value;	defaultValue: 10		}
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
		distributionName		: "Mixture Normal and Uniform"
		formula					: pi.label + " = " + pi.value + ", " + mu.label + " = " + mu.value + ", " + sigma.label + " = " + sigma.value + ", " + lowerBoundPar.label + " = " + lowerBoundPar.value + ", " + upperBoundPar.label + " = " + upperBoundPar.value
		enabled					: mainWindow.dataAvailable
	}

	LD.LDEstimateParameters { enabled: mainWindow.dataAvailable }

	LD.LDAssessFit { enabled: mainWindow.dataAvailable }
}
