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
	// Were (some of) the parameters estimated from data?
	property bool hasFitVariable:	false
	// Is the fit assessed on the same data that were used for estimating the parameters?
	property bool inSample:			assessVariable.currentValue === ""
	property bool estimated:		hasFitVariable
	property bool hasData:			hasFitVariable || !inSample

	title:		dataSetInfo.dataAvailable ? qsTr("Assess fit") : qsTr("Assess fit") + " - " + qsTr("[requires a loaded data set]")
	enabled:	dataSetInfo.dataAvailable
	info: qsTr("Assess how well the distribution fits the data. If a variable to assess the fit on is specified, the fit is assessed on that variable; otherwise on the variable used for estimating the parameters.")
	columns: 2

	DropDown
	{
		id:						assessVariable
		name:					"assessVariable"
		label:					qsTr("Assess fit on")
		Layout.columnSpan:		2
		addEmptyValue:			true
		placeholderText:		qsTr("<Variable used for estimation>")
		showVariableTypeIcon:	true
		allowedColumns:			["scale"]
		source:					[{ isDataSetVariables: true }]
		info:					qsTr("Variable used to assess the fit of the distribution. If empty, the fit is assessed on the variable used for estimation. Using a different variable (e.g., a hold-out sample) allows goodness-of-fit tests that treat the estimated parameters as known.")
	}

	Group
	{
		title:		qsTr("Plots")
		enabled:	hasData

		CheckBox
		{
			name:	"histPlot"
			label:	qsTr("Histogram vs. theoretical pdf")
			info:	qsTr("Displays a histogram of the variable overlaid with the probability density function of the distribution.")
		}

		CheckBox
		{
			name:	"ecdfPlot"
			label:	qsTr("Empirical vs. theoretical cdf")
			info:	qsTr("Displays the empirical cumulative distribution function of the variable overlaid with the cumulative distribution function of the distribution.")
		}

		CheckBox
		{
			name:	"qqPlot"
			label:	qsTr("Q-Q plot")
			info:	qsTr("Displays the quantile-quantile plot. The *x*-axis shows the theoretical quantiles of the data points under the distribution, the *y*-axis shows the empirical quantiles of the variable.")
			CheckBox
			{
				name:				"qqPlotCi"
				label:				qsTr("Confidence interval")
				childrenOnSameRow:	true
				CIField { name: "qqPlotCiLevel" }
			}
		}

		CheckBox
		{
			name:	"ppPlot"
			label:	qsTr("P-P plot")
			info:	qsTr("Displays the probability-probability plot. The *x*-axis shows the theoretical value of the cumulative distribution function of the data points under the distribution, the *y*-axis shows the empirical percentiles of the variable.")
			CheckBox
			{
				name:				"ppPlotCi"
				label:				qsTr("Confidence interval")
				childrenOnSameRow:	true
				CIField { name: "ppPlotCiLevel" }
			}
		}
	}

	Group
	{
		title:		qsTr("Statistics")
		enabled:	hasData

		// CheckBox
		// {
		// 	name:				"momentsTable"
		// 	label:				qsTr("Compare first")
		// 	info:				qsTr("Displays a table with the raw and central moments of the variable and the moments of the distribution.")
		// 	childrenOnSameRow:	true
		// 	IntegerField { name: "momentsTableNumber"; afterLabel: qsTr("moments"); defaultValue: 2; min: 1 }
		// }

		CheckBox
		{
			name:	"modelFitTable"
			label:	qsTr("Model fit")
			info:	qsTr("Displays the log-likelihood, the number of free parameters, and the AIC and BIC of the distribution. When assessed on a different variable than the one used for estimation, the log-likelihood is the out-of-sample (predictive) log-likelihood, and the AIC and BIC are not shown.")
		}

		CheckBox
		{
			id:		goodnessOfFit
			name:	"goodnessOfFit"
			label:	qsTr("Goodness-of-fit tests")
			info:	qsTr("Displays the Kolmogorov-Smirnov, Cramér-von Mises, and Anderson-Darling tests. When the parameters were estimated from the same data the fit is assessed on, the standard p-values are too conservative; use bootstrapped p-values instead.")

			RadioButtonGroup
			{
				name:		"goodnessOfFitPValue"
				title:		qsTr("p-values")
				RadioButton
				{
					value:		"asymptotic"
					label:		qsTr("Asymptotic")
					checked:	!(inSample && estimated)
					info:		qsTr("Valid when the parameters are fixed or were estimated on a different variable.")
				}
				RadioButton
				{
					value:				"bootstrap"
					label:				qsTr("Parametric bootstrap")
					checked:			inSample && estimated
					childrenOnSameRow:	true
					info:				qsTr("Valid also when the parameters were estimated from the same data. The distribution is re-estimated on each bootstrap sample.")
					IntegerField { name: "goodnessOfFitBootstrapSamples"; label: qsTr("with"); afterLabel: qsTr("samples"); defaultValue: 1000; min: 100 }
				}
			}
		}
	}
}
