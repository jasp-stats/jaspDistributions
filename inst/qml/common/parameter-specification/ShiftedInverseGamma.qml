import QtQuick
import JASP
import JASP.Controls

Column
{
	property string parameters

	Row
	{
		DoubleField	{ name: "shiftedInverseGammaAlpha";		label: qsTr("α (shape)");	min: 0;			defaultValue: 2 }
		CheckBox	{ name: "shiftedInverseGammaAlphaFixed";	label: qsTr("Fixed") }
	}

	Row
	{
		visible: parameters === "theta"
		DoubleField	{ name: "shiftedInverseGammaTheta";		label: qsTr("θ (scale)");	min: 0;			defaultValue: 1 }
		CheckBox	{ name: "shiftedInverseGammaThetaFixed";	label: qsTr("Fixed") }
	}

	Row
	{
		visible: parameters === "lambda"
		DoubleField	{ name: "shiftedInverseGammaLambda";		label: qsTr("λ (rate)");	min: 0;			defaultValue: 1 }
		CheckBox	{ name: "shiftedInverseGammaLambdaFixed";	label: qsTr("Fixed") }
	}

	Row
	{
		visible: parameters === "mu"
		DoubleField	{ name: "shiftedInverseGammaMu";		label: qsTr("μ (mean)");	min: 0;			defaultValue: 1 }
		CheckBox	{ name: "shiftedInverseGammaMuFixed";	label: qsTr("Fixed") }
	}

	Row
	{
		DoubleField	{ name: "shiftedInverseGammaShift";		label: qsTr("shift");		min: -Infinity;	defaultValue: 0 }
		CheckBox	{ name: "shiftedInverseGammaShiftFixed";	label: qsTr("Fixed")}
	}
}
