import QtQuick
import JASP
import JASP.Controls

Column
{
	property string parameters

	Row
	{
		visible: parameters === "mu"
		DoubleField	{ name: "waldMu";		label: qsTr("μ (mean)");		min: 0;	defaultValue: 1 }
		CheckBox	{ name: "waldMuFixed";	label: qsTr("Fixed") }
	}

	Row
	{
		visible: parameters === "mu"
		DoubleField	{ name: "waldLambda";		label: qsTr("λ (shape)");	min: 0;	defaultValue: 1 }
		CheckBox	{ name: "waldLambdaFixed";	label: qsTr("Fixed") }
	}

	Row
	{
		visible: parameters === "nu"
		DoubleField	{ name: "waldNu";		label: qsTr("ν (drift)");		min: 0;	defaultValue: 1 }
		CheckBox	{ name: "waldNuFixed";	label: qsTr("Fixed") }
	}

	Row
	{
		visible: parameters === "nu"
		DoubleField	{ name: "waldAlpha";		label: qsTr("α (threshold)");	min: 0;	defaultValue: 1 }
		CheckBox	{ name: "waldAlphaFixed";	label: qsTr("Fixed") }
	}

	Row
	{
		visible: parameters === "nu"
		DoubleField	{ name: "waldSigma";		label: qsTr("σ (noise)");	min: 0;	defaultValue: 1 }
		CheckBox	{ name: "waldSigmaFixed";	label: qsTr("Fixed") }
	}
}
