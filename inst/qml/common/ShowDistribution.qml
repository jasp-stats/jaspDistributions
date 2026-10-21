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
	expanded: true
	title: qsTr("Show Distribution")
	info: qsTr("Displays the theoretical distribution, given the specified parameter values or, if a variable to estimate parameters from is specified, the estimated parameter values.")
	columns: 2

	Group
	{
		title: qsTr("Texts")
		CheckBox{ name: "explanatoryText"; label: qsTr("Explanatory text"); info: qsTr("Displays additional texts that explain the displayed elements.")}
		CheckBox{ name: "parametersSupportMoments"; label: qsTr("Parameters, support, and moments"); info: qsTr("Displays parameters, support, and moments.")}
	}

	Group
	{
		title: qsTr("Table")
		CheckBox
		{ 
			name: "distributionTable" 
			label: qsTr("Distribution table")
			info: qsTr("Displays distribution table that shows quantiles, density, cumulative probability, and complement of the cumulative probability.")
			DropDown
			{
				label: qsTr("Based on")
				name: "distributionTableBasedOn"
				id: distributionTableBasedOn
				info: qsTr("Select whether you want to specify values of quantiles for which to display density and cumulative probability, or values of cumulative probability for which to display density and quantiles.")
				values: 
				[
					{ label: qsTr("quantiles"),					value: "quantile" },
					{ label: qsTr("cumulative probability"),	value: "cdf" },
				]
			}

			DropDown
			{
				label: qsTr("Specify points")
				name: "distributionTableType"
				id: distributionTableType
				info: qsTr("Select whether you want to specify the distribution table with a range of values, or manually.")
				values:
				[
					{ label: qsTr("as range"), value: "range"},
					{ label: qsTr("manually"), value: "manual"}
				]
			}

			ComponentsList
			{
				name: 			"distributionTablePoints"
				visible:		distributionTableType.value === "manual"
				// title:			qsTr("Manual points")
				info: qsTr("Values on which to evaluate the distribution.")
				rowComponent:	DoubleField 
				{ 
					name:	"point"
					min:	distributionTableBasedOn.value === "quantile" ? -Infinity : 0
					max:	distributionTableBasedOn.value === "quantile" ?  Infinity : 1 
				}
			}

			Group
			{
				title:			qsTr("Table range")
				visible:		distributionTableType.value === "range"
				DoubleField  { name: "distributionTableMin";	label: qsTr("Minimum");				id: distributionTableMin;	min: distributionTableBasedOn.value === "quantile" ? -Infinity : 0;	max: distributionTableMax.value; info: qsTr("The minimum of the range of the distribution table.")}
				DoubleField  { name: "distributionTableMax";	label: qsTr("Maximum");				id: distributionTableMax;	max: distributionTableBasedOn.value === "quantile" ?  Infinity : 1;	min: distributionTableMin.value; defaultValue: 1; info: qsTr("The maximum of the range of the distribution table.")}
				IntegerField { name: "distributionTableLength";	label: qsTr("Number of values");	id: distributionTableLength; defaultValue: 11; info: qsTr("How many points to display in the distribution table within the specified range.")}
			}
		}
	}

	Group
	{
		title: qsTr("Plots")

		Group
		{
			CheckBox{ name: "pdfPlot"; label: qsTr("Probability density function"); info: qsTr("Displays the probability density function.")}
			CheckBox{ name: "cdfPlot"; label: qsTr("Cumulative distribution function"); info: qsTr("Displays the cumulative distribution function.")}
			CheckBox{ name: "qfPlot";  label: qsTr("Quantile function"); info: qsTr("Displays the quantile function.")}
		}

		Row
		{
			spacing: jaspTheme.columnGroupSpacing
			DropDown
			{
				label: qsTr("Plot range based on")
				name: "plotRangeBasedOn"
				id: plotRangeBasedOn
				values: 
				[
					{ label: qsTr("quantiles"),					value: "quantile" },
					{ label: qsTr("cumulative probability"),	value: "cdf" },
					]
			}
			DoubleField  { name: "plotMin";	label: qsTr("from");				id: plotRangeMin;	min: plotRangeBasedOn.value === "quantile" ? -Infinity : 0;	max: plotRangeMax.value; defaultValue: -3}
			DoubleField  { name: "plotMax";	label: qsTr("to");					id: plotRangeMax;	max: plotRangeBasedOn.value === "quantile" ?  Infinity : 1;	min: plotRangeMin.value; defaultValue:  3}
		}

		ComponentsList
		{
			name: "plotDensityPoints"
			title: qsTr("Highlight density")
			rowComponent:	DoubleField { name:	"point"; min:	-Infinity;	max:	Infinity }
		}

		ComponentsList
		{
			name: "plotProbabilityLimits"
			title: qsTr("Highlight probability")
			// titles: [qsTr("From"), qsTr("To")]
			rowComponent:	RowLayout
			{
				spacing: 10 * preferencesModel.uiScale
				Row
				{
					DoubleField { label: qsTr("From");	name: "min"; min: -Infinity;	max: Infinity; defaultValue: -Infinity }
				}
				Row
				{
					DoubleField { label: qsTr("to");	name: "max"; min: -Infinity;	max: Infinity; defaultValue: Infinity }
				}
			}
		}
	}
}