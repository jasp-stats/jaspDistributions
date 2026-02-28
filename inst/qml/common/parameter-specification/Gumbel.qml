import QtQuick
import JASP
import JASP.Controls

Column
{
	property string parameters

	Row
	{
		DoubleField	{ name: "gumbelMu";		label: qsTr("μ (location)");	min: -Infinity;	defaultValue: 0 }
		CheckBox	{ name: "gumbelMuFixed";	label: qsTr("Fixed") }
	}

	Row
	{
		DoubleField	{ name: "gumbelBeta";		label: qsTr("β (scale)");	min: 0;	defaultValue: 1 }
		CheckBox	{ name: "gumbelBetaFixed";	label: qsTr("Fixed") }
	}
}
