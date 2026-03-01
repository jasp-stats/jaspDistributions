import QtQuick
import JASP
import JASP.Controls

Column
{
	property string parameters

	Row
	{
		DoubleField	{ name: "skewNormalXi";		label: qsTr("ξ (location)");	min: -Infinity;	defaultValue: 0 }
		CheckBox	{ name: "skewNormalXiFixed";	label: qsTr("Fixed") }
	}

	Row
	{
		DoubleField	{ name: "skewNormalOmega";		label: qsTr("ω (scale)");	min: 0;		defaultValue: 1 }
		CheckBox	{ name: "skewNormalOmegaFixed";	label: qsTr("Fixed") }
	}

	Row
	{
		DoubleField	{ name: "skewNormalAlpha";		label: qsTr("α (slant)");	min: -Infinity;	defaultValue: 0 }
		CheckBox	{ name: "skewNormalAlphaFixed";	label: qsTr("Fixed") }
	}
}
