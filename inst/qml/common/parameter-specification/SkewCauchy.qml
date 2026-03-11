import QtQuick
import JASP
import JASP.Controls

Column
{
	property string parameters

	Row
	{
		DoubleField	{ name: "skewCauchyXi";		label: qsTr("ξ (location)");	min: -Infinity;	defaultValue: 0 }
		CheckBox	{ name: "skewCauchyXiFixed";	label: qsTr("Fixed") }
	}

	Row
	{
		DoubleField	{ name: "skewCauchyOmega";		label: qsTr("ω (scale)");	min: 0;		defaultValue: 1 }
		CheckBox	{ name: "skewCauchyOmegaFixed";	label: qsTr("Fixed") }
	}

	Row
	{
		DoubleField	{ name: "skewCauchyAlpha";		label: qsTr("α (slant)");	min: -Infinity;	defaultValue: 0 }
		CheckBox	{ name: "skewCauchyAlphaFixed";	label: qsTr("Fixed") }
	}
}
