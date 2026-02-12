import QtQuick
import QtQuick.Layouts
import JASP
import JASP.Controls
import "./common/DistributionPresets.js" as DistributionPresets

Form
{
	columns: 1
	info: qsTr("Specify a set of distributions, estimate their parameters, and compare their fit to data.")
	VariablesForm
	{
		preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
		infoLabel: qsTr("Select data for distirbution comparison.")
		visible: true
		AvailableVariablesList { name: "allVariables" }
		AssignedVariablesList
		{
			name: "variable"; label: qsTr("Variable");
			allowedColumns: "scale"
			singleVariable: true
		}
	}

	Group
	{
		title: qsTr("Distribution specification presets")
		info: qsTr("Start with a set of pre-specified distributions")
		CheckBox
		{
			label: qsTr("Unbounded distributions")
			name: "presetUnbounded"
			id: presetUnbounded
			info: qsTr("Add unbounded distributions (e.g., Normal, Cauchy, ...).")
			checked: true
			onClicked: distributionSpecification.userEnteredInput()
		}

		CheckBox
		{
			label: qsTr("Shifted distributions")
			name: "presetShifted"
			id: presetShifted
			info: qsTr("Add bounded distributions (Exponential, Gamma, ...) with an added shift parameter. The shift parameter will be estimated by default.")
			onClicked: distributionSpecification.userEnteredInput()
		}

		CheckBox
		{
			label: qsTr("Bounded distributions")
			name: "presetBounded"
			id: presetBounded
			info: qsTr("Add bounded distributions (Exponential, Gamma, ...). ")
			onClicked: distributionSpecification.userEnteredInput()
			DoubleField
			{
				label: qsTr("Minimum")
				name: "presetBoundedMin"
				info: qsTr("Where is the minimum bound of the distribution. If not zero, the equivalent distribution with a shift parameter will be specified; the shift parameter will be by default treated as a constant (not estimated).")
				value: 0
				min: -Infinity
				id: presetBoundedMin
			}
		}
	}

	HelpButton
	{
		toolTip:			qsTr("Click to learn more about the syntax for distribution specification.")
		helpPage:			"compareDistributionSyntax"
	}

	TextArea
	{
		title: qsTr("Distribution specification")
		name: "distributionSpecification"
		id: distributionSpecification
		info: qsTr("Specify a list of distributions to fit to the data and compare.")
		text: {
			var lines = []
			if (presetUnbounded.checked)
				lines = lines.concat(DistributionPresets.unbounded)
			if (presetShifted.checked)
				lines = lines.concat(DistributionPresets.shifted)
			if (presetBounded.checked) {
				if (presetBoundedMin.value === 0) {
					lines = lines.concat(DistributionPresets.bounded)
				} else {
					lines = lines.concat(DistributionPresets.fixedShifted(presetBoundedMin.value))
				}
			}
			return lines.join("\n")
		}
		separator: "\n"
	}

	Section
	{
		title: qsTr("Output")
		info: qsTr("Control what output will be generated.")

		CheckBox
		{
			name: "comparisonTable"
			label: qsTr("Distribution comparison table")
			checked: true
			info: qsTr("Outputs the main distribution comparison table.")

			CheckBox 
			{
				name: "comparisonTableOrder"
				label: qsTr("Order by")
				infoLabel: qsTr("Order by AIC/BIC")
				info: qsTr("Orders the output by how well the distributions fit the data (according to AIC or BIC).")
				childrenOnSameRow: true
				checked: true
				DropDown
				{
					name: "comparisonTableOrderBy"
					values:   [
						{ label: qsTr("AIC"), value: "aic"},
						{ label: qsTr("BIC"), value: "bic"}
					]
					startValue: "bic"
				}
			}
		}

		Group
		{
			title: qsTr("Output per distribution")
			info: qsTr("Get detailed output for individual distributions.")
			CheckBox
			{
				name: "outputLimit"
				label: qsTr("Limit output to the first")
				infoLabel: qsTr("Limit output to the first x distributions")
				info: qsTr("Show the detailed output only for the top x distributions.")
				enabled: parameterEstimates.checked | goodnessOfFit.checked | empiricalPlots.checked
				checked: true
				childrenOnSameRow: true
				IntegerField
				{
					name: "outputLimitTo"
					value: 1
					min: 1
					afterLabel: qsTr("distributions")
				}
			}

			CheckBox
			{
				name: "parameterEstimates"
				id: parameterEstimates
				label: qsTr("Parameter estimates")
				info: qsTr("Obtain a table of parameter estimates. *Note*: All parameters are estimated with maximum likelihood.")
				checked: true
			}

			CheckBox
			{
				name: "goodnessOfFit"
				id: goodnessOfFit
				label: qsTr("Goodness of fit")
				info: qsTr("Compute goodness of fit tests. For most of the distributions, the default tests are Cramér von-Misses and Anderson-Darling for composite null hypothesis. Note that these tests rely on randomly splitting the data in two sets;\
				as a result, the results may be variable, especially for small sample sizes.\
				When a distribution does not have free parameters (i.e., all parameters are fixed), the tests are Kolmorogov-Smirnov, and Cramér von-Misses and Anderson-Darling for simple null hypothesis. \
				For normal distributions with free location and scale parameters, specific versions of goodness of fit tests are computed, appropriate for this setting. If the normal distribution has some parameters fixed, it is treated as any other distribution.")

				CheckBox
				{
					name: "goodnessOfFitBootstrap"
					label: qsTr("Parametric bootstrap from")
					infoLabel: qsTr("Parametric bootstrap from x samples")
					info: qsTr("Obtain the p-value of the goodness-of-fit tests using non-parametric bootstrap. In this case, the test statistics are always Kolmorogov-Smirnov, and Cramér von-Misses and Anderson-Darling for simple null hypothesis.")
					childrenOnSameRow: true
					IntegerField{
						name: "goodnessOfFitBootstrapSamples"
						value: 1000
						afterLabel: qsTr("samples")
					}
				}
			}

			CheckBox
			{
				name: "empiricalPlots"
				id: empiricalPlots
				label: qsTr("Plot the distribution against data")
				info: qsTr("Outputs histogram vs theoretical density plot, empirical vs. theoretical cumulative distribution function, the Q-Q plot, and the P-P plot.")
				CheckBox
				{
					name: "empiricalPlotsCi"
					label: qsTr("Confidence interval")
					info: qsTr("Add the confidence interval to the P-P and Q-Q plots.")
					childrenOnSameRow: true
					CIField
					{
						name: "empiricalPlotsCiLevel"
					}
				}
			}
		}
	}
}