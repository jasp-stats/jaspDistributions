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
	id: simulateData

	// Were (some of) the parameters estimated from data?
	property bool	hasFitVariable:	false
	// Name of the column simulated from the fitted distribution (empty if not simulated), so that it can be excluded from fitting
	property string	fittedColumn:	""

	title:		dataSetInfo.dataAvailable ? qsTr("Simulate Data") : qsTr("Simulate Data") + " - " + qsTr("[requires a loaded data set]")
	enabled:	dataSetInfo.dataAvailable
	info:		qsTr("Draw random samples and save them as new columns in the data set.")

	ComponentsList
	{
		name:				"simulateData"
		addItemManually:	false
		addBorder:			false
		headerLabels:		["", qsTr("Sample size"), qsTr("Column name")]
		values:
		[
			// Drawn from the specified parameter values, so this column can be used for estimation (e.g., parameter recovery).
			{ label: qsTr("Specified distribution"),	value: "specified"	},
			// Drawn from the fitted distribution, so this column can be used for assessing the fit, but not for estimation.
			{ label: qsTr("Fitted distribution"),		value: "fitted"		}
		]

		rowComponent: RowLayout
		{
			property bool isFitted:		rowValue === "fitted"
			property bool rowEnabled:	simulate.checked && simulate.enabled

			CheckBox
			{
				id:							simulate
				name:						"simulate"
				label:						rowLabel
				enabled:					!isFitted || simulateData.hasFitVariable
				Layout.preferredWidth:		160 * jaspTheme.uiScale
				info:						isFitted
											? qsTr("Draws a random sample from the distribution with the estimated parameter values. The column can be used to assess the fit, e.g., to see what the fit looks like when the distribution is correct. It cannot be used to estimate the parameters.")
											: qsTr("Draws a random sample from the specified distribution (using the specified parameter values). The column can be used to estimate the parameters, e.g., to see how well they are recovered.")
			}

			IntegerField
			{
				name:			"sampleSize"
				defaultValue:	Math.max(1, dataSetInfo.rowCount)
				max:			Math.max(1, dataSetInfo.rowCount)
				min:			1
				enabled:		rowEnabled
				info:			qsTr("Number of values to draw. Remaining rows of the column are left empty; cannot exceed the number of rows in the data set.")
                fieldWidth:		100
			}

			ComputedColumnField
			{
				id:					column
				name:				"column"
				placeholderText:	isFitted ? qsTr("e.g., predicted data") : qsTr("e.g., random data")
				fieldWidth:			160
				enabled:			rowEnabled
				info:				qsTr("Name of the column in which the simulated data are saved.")
			}

			// expose the fitted column, so that the variable for estimation can exclude it
			Binding
			{
				when:		isFitted
				target:		simulateData
				property:	"fittedColumn"
				value:		column.value
			}
		}
	}
}
