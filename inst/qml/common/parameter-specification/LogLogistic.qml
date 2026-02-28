import QtQuick
import JASP
import JASP.Controls

Column
{
	property string parameters

	Row
	{
		visible: parameters === "mu"
		DoubleField	{ name: "logLogisticMu";		label: qsTr("μ (log location)");	min: -Infinity;	defaultValue: 0 }
		CheckBox	{ name: "logLogisticMuFixed";	label: qsTr("Fixed") }
	}

	Row
	{
		visible: parameters === "mu"
		DoubleField	{ name: "logLogisticSigma";		label: qsTr("σ (log scale)");	min: 0;	defaultValue: 1 }
		CheckBox	{ name: "logLogisticSigmaFixed";	label: qsTr("Fixed") }
	}

	Row
	{
		visible: parameters === "alpha"
		DoubleField	{ name: "logLogisticAlpha";		label: qsTr("α (scale)");	min: 0;	defaultValue: 1 }
		CheckBox	{ name: "logLogisticAlphaFixed";	label: qsTr("Fixed") }
	}

	Row
	{
		visible: parameters === "alpha"
		DoubleField	{ name: "logLogisticBeta";		label: qsTr("β (shape)");	min: 0;	defaultValue: 2 }
		CheckBox	{ name: "logLogisticBetaFixed";	label: qsTr("Fixed") }
	}
}
