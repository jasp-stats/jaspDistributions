import QtQuick
import JASP
import JASP.Controls

Column
{
	property string parameters

	Row
	{
		DoubleField	{ id: triangularA; name: "triangularA";		label: qsTr("a (min)");	min: -Infinity;	defaultValue: 0; max: triangularB.value}
		CheckBox	{ name: "triangularAFixed";	label: qsTr("Fixed") }
	}

	Row
	{
		DoubleField	{ id: triangularB; name: "triangularB";		label: qsTr("b (max)");	min: triangularA.value;	defaultValue: 1}
		CheckBox	{ name: "triangularBFixed";	label: qsTr("Fixed") }
	}

	Row
	{
		DoubleField	{ id: triangularC; name: "triangularC";	label: qsTr("c (mode)"); min: triangularA.value; max: triangularB.value; defaultValue: 0.5 }
		CheckBox	{ name: "triangularCFixed";	label: qsTr("Fixed") }
	}
}
