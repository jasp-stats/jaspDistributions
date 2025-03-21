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
	property bool includeSE: true
	property bool includeCI: true
	property bool includeUnbiased: false

	title: enabled ? qsTr("Estimate Parameters") : qsTr("Estimate Parameters") + " - " + qsTr("[requires a loaded data set]")
	info: qsTr("Displays a table with the parameter estimates. Changing parametrization changes which parameters are displayed.")
	CheckBox
	{
		name: "methodMLE";      label: qsTr("Maximum likelihood"); id: methodMLE
		info: qsTr("Estimates the parameters by the values in the domain at which the likelihood function is maximized. The likelihood function fixes the data argument (based on the selected variable) in the theoretical density function and views it as a function of the parameters. The optimization procedure is initialized with the values for the parameters entered under \"Show Distribution\".")
		CheckBox
		{
			name: "outputEstimates"; label: qsTr("Estimates"); checked: true
			CheckBox{ name: "biasCorrected"; label: qsTr("Bias corrected"); checked: false; visible: includeUnbiased}
			CheckBox{ name: "outputSE"; label: qsTr("Std. error"); checked: false; visible: includeSE }
			CheckBox
			{
				name: "ciInterval"; label: qsTr("Confidence interval"); childrenOnSameRow: true; visible: includeCI
				PercentField{ name: "ciIntervalInterval"; label: ""; defaultValue: 95 }
			}
		}
	}
}
