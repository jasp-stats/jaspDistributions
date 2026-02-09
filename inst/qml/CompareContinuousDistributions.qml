import QtQuick
import QtQuick.Layouts
import JASP
import JASP.Controls

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

	RadioButtonGroup
	{
		name: "distributionSpecification"
		title: qsTr("Distribution specification")
		RadioButton
		{
			id: distributionSpecificationAuto
			value: "auto"
			label: qsTr("Automatic")
			checked: true
		}

		RadioButton
		{
			id: distributionSpecificationManual
			value: "manual"
			label: qsTr("Manual")
		}
	}

	Group
	{
		title: qsTr("Automatic distribution specification")
		visible: distributionSpecificationAuto.checked

		columns: 1
		CheckBox
		{
			label: qsTr("Data are bounded from below at")
			id: dataBoundedBelow
			name: "dataBoundedBelow"
			childrenOnSameRow: true
			DoubleField
			{
				name: "dataBoundedBelowAt"
				value: 0
				max: dataBoundedAbove.checked ? dataBoundedAboveAt.value : Infinity
			}
		}

		CheckBox
		{
			label: qsTr("Data are bounded from above at")
			id: dataBoundedAbove
			name: "dataBoundedAbove"
			childrenOnSameRow: true
			DoubleField
			{
				name: "dataBoundedAboveAt"
				id: dataBoundedAboveAt
				value: 1
			}
		}

		CheckBox
		{
			label: qsTr("Include shifted distributions")
			name: "shiftedDistributionsIncluded"
			enabled: !dataBoundedAbove.checked & !dataBoundedBelow.checked
		}
	}

	TextArea
	{
		title: qsTr("Manual distribution specification")
		visible: distributionSpecificationManual.checked
		name: "manualDistributionSpecification"
		text: "normal(mu=0, sigma=1)"
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