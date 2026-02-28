import QtQuick
import JASP
import JASP.Controls

Column
{
	property string parameters

	Row
	{
		DoubleField	{ name: "shiftedGammaAlpha";		label: qsTr("α (shape)");	min: 0;			defaultValue: 2 }
		CheckBox	{ name: "shiftedGammaAlphaFixed";	label: qsTr("Fixed") }
	}

	Row
	{
		visible: parameters === "theta"
		DoubleField	{ name: "shiftedGammaTheta";		label: qsTr("θ (scale)");	min: 0;			defaultValue: 1 }
		CheckBox	{ name: "shiftedGammaThetaFixed";	label: qsTr("Fixed") }
	}

	Row
	{
		visible: parameters === "lambda"
		DoubleField	{ name: "shiftedGammaLambda";		label: qsTr("λ (rate)");	min: 0;			defaultValue: 1 }
		CheckBox	{ name: "shiftedGammaLambdaFixed";	label: qsTr("Fixed") }
	}

	Row
	{
		visible: parameters === "mu"
		DoubleField	{ name: "shiftedGammaMu";		label: qsTr("μ (mean)");	min: 0;			defaultValue: 1 }
		CheckBox	{ name: "shiftedGammaMuFixed";	label: qsTr("Fixed") }
	}

	Row
	{
		DoubleField	{ name: "shiftedGammaShift";		label: qsTr("shift");		min: -Infinity;	defaultValue: 0 }
		CheckBox	{ name: "shiftedGammaShiftFixed";	label: qsTr("Fixed");	checked: true }
	}
}
