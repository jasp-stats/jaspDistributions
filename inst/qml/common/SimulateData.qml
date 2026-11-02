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
	title:		dataSetInfo.dataAvailable ? qsTr("Simulate and Display Data") : qsTr("Simulate and Display Data") + " - " + qsTr("[requires a loaded data set]")
	enabled:	dataSetInfo.dataAvailable
	info:		qsTr("Draw a random sample from the specified distribution and save it as a new column in the data set, and describe the data used for estimation and for assessing the fit.")

	CheckBox
	{
		name:	"simulatedData"
		label:	qsTr("Simulate data from the specified distribution")
		info:	qsTr("Draws a random sample from the specified distribution (using the specified parameter values). The column can be used to estimate the parameters, e.g., to see how well they are recovered.")

		IntegerField
		{
			name:			"simulatedDataSampleSize"
			label:			qsTr("Sample size")
			defaultValue:	Math.max(1, dataSetInfo.rowCount)
			max:			Math.max(1, dataSetInfo.rowCount)
			min:			1
			info:			qsTr("Number of values to draw. Remaining rows of the column are left empty; cannot exceed the number of rows in the data set.")
		}

		// not a ComputedColumnField: a change of a computed column refreshes the analysis that uses it,
		// which wipes the state and draws a new sample, which changes the column again, and so on
		AddColumnField
		{
			id:					simulatedDataColumn
			name:				"simulatedDataColumn"
			label:				qsTr("Column name")
			placeholderText:	qsTr("e.g., random data")
			fieldWidth:			120 * jaspTheme.uiScale
			info:				qsTr("Name of the column in which the simulated data are saved.")
		}

		Button
		{
			id:			redrawButton
			label:		qsTr("Redraw")
			enabled:	simulatedDataColumn.value !== ""
			info:		qsTr("Draws a new sample into the column.")
			onClicked:
			{
				redrawButton.forceActiveFocus()
				simulatedDataRedraw.displayValue = String(Number(simulatedDataRedraw.value) + 1)
				simulatedDataRedraw.doEditingFinished()
			}
		}

		IntegerField { id: simulatedDataRedraw; name: "simulatedDataRedraw"; defaultValue: 0; visible: false }
	}

	CheckBox
	{
		name:		"descriptivesTable"
		label:		qsTr("Descriptives")
		info:		qsTr("Displays a descriptive table of the variable used for estimation and, if different, of the variable used for assessing the fit.")
	}
}
