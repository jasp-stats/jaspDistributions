import QtQuick
import JASP
import JASP.Controls

Column
{
	property string parameters

	Row
	{
		DoubleField	{ name: "paretoAlpha";		label: qsTr("α (shape)");	min: 0;	defaultValue: 2 }
		CheckBox	{ name: "paretoAlphaFixed";	label: qsTr("Fixed") }
	}

	Row
	{
		DoubleField	{ name: "paretoBeta";		label: qsTr("β (scale)");	min: 0;	defaultValue: 1 }
		CheckBox	{ name: "paretoBetaFixed";	label: qsTr("Fixed") }
	}
}
