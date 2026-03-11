import QtQuick
import JASP
import JASP.Controls

Column
{
	property string parameters

	Row
	{
		visible: parameters === "mu"
		DoubleField	{ name: "shiftedWaldMu";		label: qsTr("μ (mean)");		min: 0;	defaultValue: 1 }
		CheckBox	{ name: "shiftedWaldMuFixed";	label: qsTr("Fixed") }
	}

	Row
	{
		visible: parameters === "mu"
		DoubleField	{ name: "shiftedWaldLambda";		label: qsTr("λ (shape)");	min: 0;	defaultValue: 1 }
		CheckBox	{ name: "shiftedWaldLambdaFixed";	label: qsTr("Fixed") }
	}

	Row
	{
		visible: parameters === "nu"
		DoubleField	{ name: "shiftedWaldNu";		label: qsTr("ν (drift)");		min: 0;	defaultValue: 1 }
		CheckBox	{ name: "shiftedWaldNuFixed";	label: qsTr("Fixed") }
	}

	Row
	{
		visible: parameters === "nu"
		DoubleField	{ name: "shiftedWaldAlpha";		label: qsTr("α (threshold)");	min: 0;	defaultValue: 1 }
		CheckBox	{ name: "shiftedWaldAlphaFixed";	label: qsTr("Fixed") }
	}

	Row
	{
		visible: parameters === "nu"
		DoubleField	{ name: "shiftedWaldSigma";		label: qsTr("σ (noise)");	min: 0;	defaultValue: 1 }
		CheckBox	{ name: "shiftedWaldSigmaFixed";	label: qsTr("Fixed") }
	}

	Row
	{
		DoubleField	{ name: "shiftedWaldShift";		label: qsTr("shift");		min: -Infinity;	defaultValue: 0 }
		CheckBox	{ name: "shiftedWaldShiftFixed";	label: qsTr("Fixed") }
	}
}
