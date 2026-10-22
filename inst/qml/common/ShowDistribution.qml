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
	title: qsTr("Show Distribution")
	info: qsTr("Displays the theoretical distribution, given the specified parameter values or, if a variable to estimate parameters from is specified, the estimated parameter values.")
	columns: 2

	// Quantiles and cumulative probabilities live on different scales, so each has its own fields:
	// switching between them then never leaves a value that is out of bounds for the other.

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
			name:	"distributionTable"
			label:	qsTr("Distribution table")
			info:	qsTr("Displays distribution table that shows quantiles, density, cumulative probability, and complement of the cumulative probability.")

			DropDown
			{
				id:			distributionTableBasedOn
				name:		"distributionTableBasedOn"
				label:		qsTr("Based on")
				startValue:	"cdf"
				info:		qsTr("Select whether you want to specify values of quantiles for which to display density and cumulative probability, or values of cumulative probability for which to display density and quantiles.")
				values:
				[
					{ label: qsTr("quantiles"),					value: "quantile"	},
					{ label: qsTr("cumulative probability"),	value: "cdf"		}
				]
			}

			DropDown
			{
				id:		distributionTableType
				name:	"distributionTableType"
				label:	qsTr("Specify points")
				info:	qsTr("Select whether you want to specify the distribution table with a range of values, or manually.")
				values:
				[
					{ label: qsTr("as range"), value: "range"	},
					{ label: qsTr("manually"), value: "manual"	}
				]
			}

			Group
			{
				visible: distributionTableType.value === "range"

				Row
				{
					spacing: jaspTheme.columnGroupSpacing
					visible: distributionTableBasedOn.value === "quantile"
					DoubleField { name: "distributionTableQuantileMin";	label: qsTr("From");	negativeValues: true; defaultValue: -3;	info: qsTr("The smallest quantile in the distribution table.") }
					DoubleField { name: "distributionTableQuantileMax";	label: qsTr("to");		negativeValues: true; defaultValue:  3;	info: qsTr("The largest quantile in the distribution table.") }
				}

				Row
				{
					spacing: jaspTheme.columnGroupSpacing
					visible: distributionTableBasedOn.value === "cdf"
					DoubleField { name: "distributionTableCdfMin";	label: qsTr("From");	min: 0; max: 1; defaultValue: 0;	info: qsTr("The smallest cumulative probability in the distribution table.") }
					DoubleField { name: "distributionTableCdfMax";	label: qsTr("to");		min: 0; max: 1; defaultValue: 1;	info: qsTr("The largest cumulative probability in the distribution table.") }
				}

				IntegerField { name: "distributionTableLength"; label: qsTr("Number of values"); min: 2; defaultValue: 11; info: qsTr("How many equally spaced points to display in the distribution table within the specified range.") }
			}

			ComponentsList
			{
				name:			"distributionTableQuantilePoints"
				visible:		distributionTableType.value === "manual" && distributionTableBasedOn.value === "quantile"
				addBorder:		false
				info:			qsTr("Quantiles at which to evaluate the distribution.")
				rowComponent:	FormulaField { name: "point"; defaultValue: "0" }
			}

			ComponentsList
			{
				name:			"distributionTableCdfPoints"
				visible:		distributionTableType.value === "manual" && distributionTableBasedOn.value === "cdf"
				addBorder:		false
				info:			qsTr("Cumulative probabilities at which to evaluate the distribution.")
				rowComponent:	FormulaField { name: "point"; min: 0; max: 1; defaultValue: "0.5" }
			}
		}
	}

	Group
	{
		title: qsTr("Plots")

		Group
		{
			CheckBox { id: pdfPlot;	name: "pdfPlot";	label: qsTr("Probability density function");		info: qsTr("Displays the probability density function.")		}
			CheckBox { id: cdfPlot;	name: "cdfPlot";	label: qsTr("Cumulative distribution function");	info: qsTr("Displays the cumulative distribution function.")	}
			CheckBox { id: qfPlot;	name: "qfPlot";		label: qsTr("Quantile function");					info: qsTr("Displays the quantile function.")					}
		}

		Row
		{
			spacing: jaspTheme.columnGroupSpacing
			enabled: pdfPlot.checked || cdfPlot.checked || qfPlot.checked

			DropDown
			{
				id:			plotRangeBasedOn
				name:		"plotRangeBasedOn"
				label:		qsTr("Plot range based on")
				// a range of cumulative probabilities adapts to the location and scale of any distribution
				startValue:	"cdf"
				info:		qsTr("Select whether the range of the plots is specified in quantiles or in cumulative probabilities.")
				values:
				[
					{ label: qsTr("quantiles"),					value: "quantile"	},
					{ label: qsTr("cumulative probability"),	value: "cdf"		}
				]
			}

			Row
			{
				spacing: jaspTheme.columnGroupSpacing
				visible: plotRangeBasedOn.value === "quantile"
				DoubleField { name: "plotQuantileMin"; label: qsTr("from");	negativeValues: true; defaultValue: -3;	info: qsTr("The smallest quantile shown in the plots.") }
				DoubleField { name: "plotQuantileMax"; label: qsTr("to");	negativeValues: true; defaultValue:  3;	info: qsTr("The largest quantile shown in the plots.") }
			}

			Row
			{
				spacing: jaspTheme.columnGroupSpacing
				visible: plotRangeBasedOn.value === "cdf"
				// 0 and 1 are excluded, because they correspond to infinite quantiles for distributions with unbounded support
				DoubleField { name: "plotCdfMin"; label: qsTr("from");	min: 0; max: 1; inclusive: JASP.None; defaultValue: 0.01;	info: qsTr("The cumulative probability of the smallest quantile shown in the plots.") }
				DoubleField { name: "plotCdfMax"; label: qsTr("to");	min: 0; max: 1; inclusive: JASP.None; defaultValue: 0.99;	info: qsTr("The cumulative probability of the largest quantile shown in the plots.") }
			}
		}

		ComponentsList
		{
			name:			"plotDensityPoints"
			title:			qsTr("Highlight density")
			enabled:		pdfPlot.checked || cdfPlot.checked || qfPlot.checked
			addBorder:		false
			headerLabels:	[qsTr("At quantile"), qsTr("Color")]
			info:			qsTr("Quantiles at which the density is highlighted: as a point in the probability density plot, and as the slope of a tangent in the cumulative distribution and quantile plots.")
			rowComponent:	RowLayout
			{
				spacing: 10 * preferencesModel.uiScale
				FormulaField	{ name: "point"; defaultValue: "0" }
				ColorOption	{ buttonInfo: qsTr("Color in which the density is highlighted.") }
			}
		}

		ComponentsList
		{
			name:			"plotProbabilityLimits"
			title:			qsTr("Highlight probability")
			enabled:		pdfPlot.checked || cdfPlot.checked || qfPlot.checked
			addBorder:		false
			headerLabels:	[qsTr("From"), qsTr("To"), qsTr("Color")]
			info:			qsTr("Intervals whose probability is highlighted in the plots. Use -Inf or Inf to leave the interval unbounded on that side (e.g., to highlight a tail).")
			rowComponent:	RowLayout
			{
				spacing: 10 * preferencesModel.uiScale
				FormulaField { name: "min"; defaultValue: "-Inf" }
				FormulaField { name: "max"; defaultValue: "Inf" }
				ColorOption { buttonInfo: qsTr("Color in which the probability is highlighted.") }
			}
		}
	}
}
