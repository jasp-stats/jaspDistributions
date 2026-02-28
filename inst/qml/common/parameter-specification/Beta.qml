import QtQuick
import JASP
import JASP.Controls

Column
{
	property string parameters

	Row
	{
		DoubleField	{ name: "betaAlpha";		label: qsTr("α (shape)");	min: 0;	defaultValue: 2 }
		CheckBox	{ name: "betaAlphaFixed";	label: qsTr("Fixed") }
	}

	Row
	{
		DoubleField	{ name: "betaBeta";		label: qsTr("β (shape)");	min: 0;	defaultValue: 2 }
		CheckBox	{ name: "betaBetaFixed";	label: qsTr("Fixed") }
	}
}
