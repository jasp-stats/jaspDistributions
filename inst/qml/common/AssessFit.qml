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
	title: qsTr("Assess fit")
	Group
	{
		title: qsTr("Plots")
		CheckBox 
		{ 
			name:	"histPlot"
			label:	qsTr("Histogram vs. theoretical pdf")
			info:	qsTr("Displays a histogram of the selected variable overlayed with the probability density function of the fitted distribution")
		}

		CheckBox 
		{ 
			name:	"qqPlot" 
			label:	qsTr("Q-Q plot") 
			info:	qsTr("Displays the quantile-quantile plot. The *x*-axis shows the theoretical quantiles of the data points under the fitted distribution, the *y*-axis shows the empirical quantiles of the selected variable.")
			CheckBox
			{
				name:	"qqPlotCi"
				label:	qsTr("Confidence interval")
				childrenOnSameRow:	true
				CIField{ name: "qqPlotCiLevel" }
			}
		}

		CheckBox 
		{ 
			name:	"ecdfPlot"
			label:	qsTr("Empirical vs. theoretical cdf")
			info:	qsTr("Displays an empirical cumulative distribution plot overlayed with the cumulative distribution function of the fitted distribution")
		}

		CheckBox
		{
			name:	"ppPlot"
			label:	qsTr("P-P plot")
			info:	qsTr("Displays the probability-probability plot. The *x*-axis shows the theoretical value of the cumulative density function of the data points under the fitted distribution, the *y*-axis shows the empirical percentiles of the selected variable.")
			CheckBox
			{
				name:	"ppPlotCi"
				label:	qsTr("Confidence interval")
				childrenOnSameRow:	true
				CIField{ name: "ppPlotCiLevel" }
			}
		}
	}

	Group
	{
		title: qsTr("Statistics")
		CheckBox
		{
			name:	"goodnessOfFit"
			label:	qsTr("Goodness-of-fit tests")
			info:	qsTr("Displays goodness of fit tests. The selection of tests is based on a distribution and whether its parameters are estimated from the data.")
			CheckBox
			{
				name: "goodnessOfFitBootstrap"
				label: qsTr("Bootstraped p-values from")
				childrenOnSameRow: true
				IntegerField
				{
					name: "goodnessOfFitBootstrapSamples"
					afterLabel: qsTr("samples")
					defaultValue: 1000
				}
			}
		}

		CheckBox
		{
			name:	"informationCriteria"
			label:	qsTr("Information criteria")
			info:	qsTr("Displays the log-likelihood, AIC, and BIC of the distribution.")
		}

		CheckBox
		{
			name:	"momentsTable"
			label:	qsTr("First")
			info: qsTr("Displays a table with the raw and central moments of the selected variable and the moments of the distribution. Defaults to first 2 moments.")
			childrenOnSameRow: true
			IntegerField{name: "momentsTableNumber"; afterLabel: qsTr("moments"); defaultValue: 2; min: 1 }
		}
	}
}