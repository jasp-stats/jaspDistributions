import QtQuick
import QtQuick.Layouts
import JASP
import JASP.Controls
import "./common/DistributionPresets.js" as DistributionPresets

Form
{
	columns: 1

	VariablesForm
	{
		preferredHeight: jaspTheme.smallDefaultVariablesFormHeight
		info: qsTr("Select data for distirbution comparison.")
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
		CheckBox
		{
			label: qsTr("Unbounded distributions")
			name: "presetUnbounded"
			id: presetUnbounded
			checked: true
			onClicked: distributionSpecification.userEnteredInput()
		}

		CheckBox
		{
			label: qsTr("Shifted distributions")
			name: "presetShifted"
			id: presetShifted
			onClicked: distributionSpecification.userEnteredInput()
		}

		CheckBox
		{
			label: qsTr("Bounded distributions")
			name: "presetBounded"
			id: presetBounded
			onClicked: distributionSpecification.userEnteredInput()
			DoubleField
			{
				label: qsTr("Minimum")
				name: "presetBoundedMin"
				value: 0
				min: -Infinity
				id: presetBoundedMin
			}
		}
	}

	TextArea
	{
		title: qsTr("Distribution specification")
		name: "distributionSpecification"
		id: distributionSpecification
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

		CheckBox
		{
			name: "comparisonTable"
			label: qsTr("Distribution comparison table")
			checked: true

			CheckBox 
			{
				name: "comparisonTableOrder"
				label: qsTr("Order by")
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
			CheckBox
			{
				name: "outputLimit"
				label: qsTr("Limit output to the first")
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
				checked: true
			}

			CheckBox
			{
				name: "goodnessOfFit"
				id: goodnessOfFit
				label: qsTr("Goodness of fit")

				CheckBox
				{
					name: "goodnessOfFitBootstrap"
					label: qsTr("Parametric bootstrap from")
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
				label: qsTr("Plot distribution against data")
				CheckBox
				{
					name: "empiricalPlotsCi"
					label: qsTr("Confidence interval")
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