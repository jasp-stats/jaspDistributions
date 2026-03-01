import QtQuick
import JASP
import JASP.Controls

Column
{
	property string parameters

	Row
	{
		visible: parameters === "mu"
		DoubleField	{ name: "shiftedLogLogisticMu";		label: qsTr("μ (log location)");	min: -Infinity;	defaultValue: 0 }
		CheckBox	{ name: "shiftedLogLogisticMuFixed";	label: qsTr("Fixed") }
	}

	Row
	{
		visible: parameters === "mu"
		DoubleField	{ name: "shiftedLogLogisticSigma";		label: qsTr("σ (log scale)");	min: 0;			defaultValue: 1 }
		CheckBox	{ name: "shiftedLogLogisticSigmaFixed";	label: qsTr("Fixed") }
	}

	Row
	{
		visible: parameters === "alpha"
		DoubleField	{ name: "shiftedLogLogisticAlpha";		label: qsTr("α (scale)");	min: 0;			defaultValue: 1 }
		CheckBox	{ name: "shiftedLogLogisticAlphaFixed";	label: qsTr("Fixed") }
	}

	Row
	{
		visible: parameters === "alpha"
		DoubleField	{ name: "shiftedLogLogisticBeta";		label: qsTr("β (shape)");	min: 0;			defaultValue: 2 }
		CheckBox	{ name: "shiftedLogLogisticBetaFixed";	label: qsTr("Fixed") }
	}

	Row
	{
		DoubleField	{ name: "shiftedLogLogisticShift";		label: qsTr("shift");		min: -Infinity;	defaultValue: 0 }
		CheckBox	{ name: "shiftedLogLogisticShiftFixed";	label: qsTr("Fixed") }
	}
}
