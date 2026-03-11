import QtQuick
import JASP
import JASP.Controls

Column
{
	property string parameters

	Row
	{
		DoubleField	{ name: "skewTXi";		label: qsTr("ξ (location)");	min: -Infinity;	defaultValue: 0 }
		CheckBox	{ name: "skewTXiFixed";	label: qsTr("Fixed") }
	}

	Row
	{
		DoubleField	{ name: "skewTOmega";		label: qsTr("ω (scale)");	min: 0;		defaultValue: 1 }
		CheckBox	{ name: "skewTOmegaFixed";	label: qsTr("Fixed") }
	}

	Row
	{
		DoubleField	{ name: "skewTAlpha";		label: qsTr("α (slant)");	min: -Infinity;	defaultValue: 0 }
		CheckBox	{ name: "skewTAlphaFixed";	label: qsTr("Fixed") }
	}

	Row
	{
		DoubleField	{ name: "skewTNu";		label: qsTr("ν (df)");	min: 0;		defaultValue: 5 }
		CheckBox	{ name: "skewTNuFixed";	label: qsTr("Fixed") }
	}
}
