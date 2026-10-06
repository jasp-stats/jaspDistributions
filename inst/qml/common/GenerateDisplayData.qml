//
// Copyright (C) 2013-2026 University of Amsterdam
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
	property string formula
	property bool	continuous		: true

	title: enabled ? qsTr("Generate and Display Data") : qsTr("Generate and Display Data") + " - " + qsTr("[requires a loaded data set]")
	CheckBox 
	{ 
		text: qsTr("Generate data from the theoretical distribution") 
		name: "generateData"
		id: generateData

		ComputedColumnField
		{
			name:					"generatedDataColumn"
			text:					qsTr("Column name")
			placeholderText:		qsTr("e.g., random data")
			fieldWidth:				120
			enabled:				generateData.checked
		}
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
			allowedColumns: continuous ? ["scale"] : ["nominal"]
			singleVariable: true
		}
	}

	Group
	{
		CheckBox
		{
			name: "dataDescriptives"
			label: qsTr("Descriptives")
			info: qsTr("Displays a descriptive table of the selected variable.")
		}

		CheckBox
		{
			name: "dataHistogram"
			label: continuous ? qsTr("Histogram") : qsTr("Bar plot")
			info: qsTr("Display a histogram/bar plot of the selected variable")
		}	
	}
}