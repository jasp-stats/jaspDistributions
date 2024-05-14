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

Section
{
	property string distributionName
	property string distributionSimpleName		: distributionName
	property string formula
	property bool	histogramIsBarPlot			: false
	property bool	showStatisticsMoment		: true
	property bool	showCumulativeDistribution	: true
	property bool	allowOnlyScaleColumns		: true
	property bool	suggestScaleColumns			: false

	title: enabled ? qsTr("Generate and Display Data") : qsTr("Generate and Display Data") + " - " + qsTr("[requires a loaded data set]")
	Group
	{
		Layout.columnSpan: 2
		title: qsTr("Generate new variable from %1").arg(distributionName) + " (" + formula + ")"

		AddColumnField	{ name: "newVariableName"; text: qsTr("Variable name: "); info: qsTr("Specify the name of the variable. Once filled, creates a column with samples drawn from the specified distribution in the current data set."); fieldWidth: 120; placeholderText: qsTr("e.g., random %1").arg(distributionSimpleName) }
		IntegerField	{ name: "sampleSize"; label: qsTr("Number of samples: "); info: qsTr("Specify the number of samples."); min: dataSetInfo.dataAvailable ? 1 : 0; defaultValue: dataSetInfo.rowCount; max: dataSetInfo.rowCount }
		Button
		{
			id: simulateNowButton
			name: "simulateNowButton"
			label: qsTr("Draw samples")
			info: qsTr("Sample from the theoretical distribution.")
			onClicked:
			{
				simulateNowButton.forceActiveFocus();
				simulateNow.checked = !simulateNow.checked

			}
		}
		CheckBox { name: "simulateNow"; visible: false; id: simulateNow }
	}


	VariablesForm
	{
		preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
		info: "<h3>" + qsTr("Get variable from data set: ") + "</h3>" + qsTr("Select a variable to display and fit with the theoretical distribution.")
		visible: true
		AvailableVariablesList { name: "allVariables" }
		AssignedVariablesList
		{
			name: "variable"; label: qsTr("Get variable from data set");
			allowedColumns: allowOnlyScaleColumns ? ["scale"] : []
			suggestedColumns: suggestScaleColumns ? ['scale'] : []
			singleVariable: true
		}
	}

	Group
	{
		title: qsTr("Statistics")
		CheckBox
		{
			name: "summary"
			label: qsTr("Descriptives")
			info: qsTr("Displays a descriptive table of the selected variable.")
			checked: true
		}
		CheckBox
		{
			visible: showStatisticsMoment
			name: "moments"; label: qsTr("First"); childrenOnSameRow: true
			infoLabel: qsTr("First observed moments")
			info: qsTr("Displays a table with the raw and central sample moments of the selected variable. Defaults to first 2 moments.")
			IntegerField{name: "momentsUpTo"; afterLabel: qsTr("observed moments"); defaultValue: 2; min: 1 }
		}
	}

	Group
	{
		title: qsTr("Plots")
		CheckBox
		{
			name: "histogram";  label: histogramIsBarPlot ? qsTr("Bar plot") : qsTr("Histogram with"); childrenOnSameRow: true
			IntegerField { visible: !histogramIsBarPlot; name: "histogramBins"; afterLabel: qsTr("bins"); defaultValue: 30 }
		}
		CheckBox{ visible: showCumulativeDistribution; name: "ecdf"; label: qsTr("Empirical cumulative distribution") }
	}
}
