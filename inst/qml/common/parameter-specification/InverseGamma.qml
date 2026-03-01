import QtQuick
import JASP
import JASP.Controls

Column
{
	property string parameters

	Row
	{
		DoubleField	{ name: "inverseGammaAlpha";		label: qsTr("α (shape)");	min: 0;	defaultValue: 2 }
		CheckBox	{ name: "inverseGammaAlphaFixed";	label: qsTr("Fixed") }
	}

	Row
	{
		visible: parameters === "theta"
		DoubleField	{ name: "inverseGammaTheta";		label: qsTr("θ (scale)");	min: 0;	defaultValue: 1 }
		CheckBox	{ name: "inverseGammaThetaFixed";	label: qsTr("Fixed") }
	}

	Row
	{
		visible: parameters === "lambda"
		DoubleField	{ name: "inverseGammaLambda";		label: qsTr("λ (rate)");	min: 0;	defaultValue: 1 }
		CheckBox	{ name: "inverseGammaLambdaFixed";	label: qsTr("Fixed") }
	}

	Row
	{
		visible: parameters === "mu"
		DoubleField	{ name: "inverseGammaMu";		label: qsTr("μ (mean)");	min: 0;	defaultValue: 1 }
		CheckBox	{ name: "inverseGammaMuFixed";	label: qsTr("Fixed") }
	}
}
