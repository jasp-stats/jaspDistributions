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
	property string	distributionType		: "continuous" // "counts" or "categorical"
	property bool includeShapiroWilk		: false
	property bool isDistributionContinuous	: distributionType === "continuous"

	title: enabled ? qsTr("Assess Fit") : qsTr("Assess Fit") + " - " + qsTr("[requires a loaded data set]")

	Group
	{
        title: qsTr("Plots")
        CheckBox
        {
			name:  isDistributionContinuous ? "estPDF" : "estPMF";
			label: isDistributionContinuous ? qsTr("Histogram vs. theoretical pdf") : qsTr("Histogram vs. theoretical pmf")
			info: qsTr("Displays a histogram of the selected variable overlayed with the probability density function of the fitted distribution")
        }
        CheckBox
        {
			name:	"qqplot"
			label:	qsTr("Q-Q plot")
			info:	qsTr("Displays the quantile-quantile plot. The *x*-axis shows the theoretical quantiles of the data points under the fitted distribution, the *y*-axis shows the empirical quantiles of the selected variable.")
			CheckBox
            {
                name:               "qqPlotCi"
                label:              qsTr("Confidence interval")
                childrenOnSameRow:  true
				visible:            isDistributionContinuous
                CIField{ name: "qqPlotCiLevel" }
            }
        }
		CheckBox
		{
			name: "estCDF"
			label: qsTr("Empirical vs. theoretical cdf")
			info: qsTr("Displays an empirical cumulative distribution plot overlayed with the cumulative distribution function of the fitted distribution")
		}
        CheckBox
        {
            name: "ppplot";
            label: qsTr("P-P plot")
			info:	qsTr("Displays the probability-probability plot. The *x*-axis shows the theoretical value of the cumulative density function of the data points under the fitted distribution, the *y*-axis shows the empirical percentiles of the selected variable.")
			CheckBox
            {
                name:               "ppPlotCi"
                label:              qsTr("Confidence interval")
                childrenOnSameRow:  true
				visible:            isDistributionContinuous
                CIField{ name: "ppPlotCiLevel" }
            }
        }
	}

	Loader
	{
		sourceComponent: isDistributionContinuous ? continuous : ( distributionType == "counts" ? counts : categorical )
		Component
		{
			id: continuous
			Group
			{
				title: qsTr("Statistics")
				CheckBox{ name: "kolmogorovSmirnov"; label: qsTr("Kolmogorov-Smirnov"); info: qsTr("Displays the Kolmogorov-Smirnov test") }
				CheckBox{ name: "cramerVonMisses";   label: qsTr("Cramér–von Mises");	info: qsTr("Displays the Cramér-von Mises test") }
				CheckBox{ name: "andersonDarling";   label: qsTr("Anderson-Darling");	info: qsTr("Displays the Anderson-Darling test") }
				CheckBox{ name: "shapiroWilk";       label: qsTr("Shapiro-Wilk");		info: qsTr("Displays the Shapiro-Wilk test of normality"); visible: includeShapiroWilk }
			}
		}

		Component
		{
			id: counts
			Group
			{
				title: qsTr("Statistics")
				CheckBox{ name: "chiSquare"; label: qsTr("Chi-square"); info: qsTr("Displays the chi-square goodness of fit test")}
			}
		}

		Component
		{
			id: categorical // for stuff like bernoulli / categorical dist (in future) we don't have any statistics implemented yet
			Group{ }
		}
	}
}
