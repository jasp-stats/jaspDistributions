import QtQuick
import JASP
import JASP.Controls

Column
{
	property string parameters

	Row
	{
		DoubleField	{ name: "studentTNu";		label: qsTr("ν (df)");		min: 0;		defaultValue: 5 }
		CheckBox	{ name: "studentTNuFixed";	label: qsTr("Fixed") }
	}

	Row
	{
		DoubleField	{ name: "studentTMu";		label: qsTr("μ (location)");	min: -Infinity;	defaultValue: 0 }
		CheckBox	{ name: "studentTMuFixed";	label: qsTr("Fixed") }
	}

	Row
	{
		DoubleField	{ name: "studentTSigma";		label: qsTr("σ (scale)");	min: 0;		defaultValue: 1 }
		CheckBox	{ name: "studentTSigmaFixed";	label: qsTr("Fixed") }
	}
}
