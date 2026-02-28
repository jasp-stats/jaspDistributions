import QtQuick
import JASP
import JASP.Controls
import "./parameter-specification" as PS

Column
{
	id: root
	spacing: 5
	leftPadding: 10


	property var parametersValues: {
		if (distribution.currentValue === "Normal")
			return [
				{ label: qsTr("μ (mean), σ (std.deviation)"),	value: "sigma"	},
				{ label: qsTr("μ (mean), σ² (variance)"),		value: "sigma2"	},
				{ label: qsTr("μ (mean), τ (precision)"),		value: "tau"	}
			]
		if (distribution.currentValue === "Exponential")
			return [
				{ label: qsTr("λ (rate)"), 	value: "lambda"	},
				{ label: qsTr("β (scale)"),	value: "beta" 	}
			]
		return [
			{ label: qsTr("α (shape), θ (scale)"),	value: "theta"	},
			{ label: qsTr("α (shape), λ (rate)"),	value: "lambda"	},
			{ label: qsTr("α (shape), μ (mean)"),	value: "mu"		}
		]
	}

	Row
	{
		DropDown
		{
			id: distribution
			name: "distribution"
			values: [
				{ label: qsTr("Normal"),      value: "Normal"      },
				{ label: qsTr("Exponential"), value: "Exponential" },
				{ label: qsTr("Gamma"),       value: "Gamma"       }
			]
		}
		CheckBox
		{
			id: settings
			name: "settings"
			label: qsTr("Show advanced settings")
			checked: false
		}
	}

	Column
	{
		spacing: 5
		leftPadding: 10
		visible: settings.checked

		Row
		{
			DropDown
			{
				id: parameters
				name: "parameters"
				label: qsTr("Parameters")
				values: root.parametersValues
			}
		}


		PS.Normal
		{
			visible: distribution.currentValue === "Normal"
			parameters: parameters.currentValue
		}

		PS.Exponential
		{
			visible: distribution.currentValue === "Exponential"
			parameters: parameters.currentValue
		}
	}
}
