import QtQuick
import JASP
import JASP.Controls

Column
{
	property string parameters

	Row
	{
		DoubleField	{ name: "gammaAlpha";		label: qsTr("α (shape)");	min: 0;	defaultValue: 2 }
		CheckBox	{ name: "gammaAlphaFixed";	label: qsTr("Fixed") }
	}

	Row
	{
		visible: parameters === "theta"
		DoubleField	{ name: "gammaTheta";		label: qsTr("θ (scale)");	min: 0;	defaultValue: 1 }
		CheckBox	{ name: "gammaThetaFixed";	label: qsTr("Fixed") }
	}

	Row
	{
		visible: parameters === "lambda"
		DoubleField	{ name: "gammaLambda";		label: qsTr("λ (rate)");	min: 0;	defaultValue: 1 }
		CheckBox	{ name: "gammaLambdaFixed";	label: qsTr("Fixed") }
	}

	Row
	{
		visible: parameters === "mu"
		DoubleField	{ name: "gammaMu";		label: qsTr("μ (mean)");	min: 0;	defaultValue: 1 }
		CheckBox	{ name: "gammaMuFixed";	label: qsTr("Fixed") }
	}
}
