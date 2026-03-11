import QtQuick
import JASP
import JASP.Controls

Column
{
	property string parameters

	Row
	{
		DoubleField	{ name: "cauchyMu";		label: qsTr("μ (location)");	min: -Infinity;	defaultValue: 0 }
		CheckBox	{ name: "cauchyMuFixed";	label: qsTr("Fixed") }
	}

	Row
	{
		DoubleField	{ name: "cauchySigma";		label: qsTr("σ (scale)");	min: 0;	defaultValue: 1 }
		CheckBox	{ name: "cauchySigmaFixed";	label: qsTr("Fixed") }
	}
}
