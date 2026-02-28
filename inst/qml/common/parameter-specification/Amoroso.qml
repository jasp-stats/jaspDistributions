import QtQuick
import JASP
import JASP.Controls

Column
{
	property string parameters

	Row
	{
		DoubleField	{ name: "amorosoA";			label: qsTr("a (location)");	min: -Infinity;	defaultValue: 0 }
		CheckBox	{ name: "amorosoAFixed";		label: qsTr("Fixed") }
	}

	Row
	{
		DoubleField	{ name: "amorosoTheta";		label: qsTr("θ (scale)");		min: -Infinity;	defaultValue: 1 }
		CheckBox	{ name: "amorosoThetaFixed";	label: qsTr("Fixed") }
	}

	Row
	{
		DoubleField	{ name: "amorosoAlpha";		label: qsTr("α (shape)");		min: 0;			defaultValue: 2 }
		CheckBox	{ name: "amorosoAlphaFixed";	label: qsTr("Fixed") }
	}

	Row
	{
		DoubleField	{ name: "amorosoBeta";		label: qsTr("β (shape)");		min: -Infinity;	defaultValue: 1 }
		CheckBox	{ name: "amorosoBetaFixed";	label: qsTr("Fixed") }
	}
}
