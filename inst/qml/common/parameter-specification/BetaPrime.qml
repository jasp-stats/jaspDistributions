import QtQuick
import JASP
import JASP.Controls

Column
{
	property string parameters

	Row
	{
		DoubleField	{ name: "betaPrimeAlpha";		label: qsTr("α (shape)");	min: 0;	defaultValue: 2 }
		CheckBox	{ name: "betaPrimeAlphaFixed";	label: qsTr("Fixed") }
	}

	Row
	{
		DoubleField	{ name: "betaPrimeBeta";		label: qsTr("β (shape)");	min: 0;	defaultValue: 2 }
		CheckBox	{ name: "betaPrimeBetaFixed";	label: qsTr("Fixed") }
	}
}
