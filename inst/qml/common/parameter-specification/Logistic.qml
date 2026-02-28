import QtQuick
import JASP
import JASP.Controls

Column
{
	property string parameters

	Row
	{
		DoubleField	{ name: "logisticMu";		label: qsTr("μ (location)");	min: -Infinity;	defaultValue: 0 }
		CheckBox	{ name: "logisticMuFixed";	label: qsTr("Fixed") }
	}

	Row
	{
		DoubleField	{ name: "logisticSigma";		label: qsTr("σ (scale)");	min: 0;	defaultValue: 1 }
		CheckBox	{ name: "logisticSigmaFixed";	label: qsTr("Fixed") }
	}
}
