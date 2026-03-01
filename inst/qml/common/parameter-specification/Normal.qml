import QtQuick
import JASP
import JASP.Controls

Column
{
	property string parameters;

	Row
	{
		DoubleField	{ name: "normalMu";		label: qsTr("μ (mean)");		min: -Infinity;	defaultValue: 0 }
		CheckBox	{ name: "normalMuFixed";	label: qsTr("Fixed") }
	}

	Row
	{
		visible: parameters === "sigma"
		DoubleField	{ name: "normalSigma";		label: qsTr("σ (std. deviation)");	min: 0;	defaultValue: 1 }
		CheckBox	{ name: "normalSigmaFixed";	label: qsTr("Fixed") }
	}

	Row
	{
		visible: parameters === "sigma2"
		DoubleField	{ name: "normalSigma2";		label: qsTr("σ² (variance)");	min: 0;	defaultValue: 1 }
		CheckBox	{ name: "normalSigma2Fixed";	label: qsTr("Fixed") }
	}

	Row
	{
		visible: parameters === "tau"
		DoubleField	{ name: "normalTau";		label: qsTr("τ (precision)");	min: 0;	defaultValue: 1 }
		CheckBox	{ name: "normalTauFixed";	label: qsTr("Fixed") }
	}
}
