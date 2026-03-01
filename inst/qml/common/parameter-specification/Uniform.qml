import QtQuick
import JASP
import JASP.Controls

Column
{
	property string parameters

	Row
	{
		DoubleField	{ id: uniformMin; name: "uniformMin";		label: qsTr("min");	min: -Infinity;	defaultValue: 0; max: uniformMax.value }
		CheckBox	{ name: "uniformMinFixed";	label: qsTr("Fixed") }
	}

	Row
	{
		DoubleField	{ id: uniformMax; name: "uniformMax";		label: qsTr("max");	defaultValue: 1; min: uniformMin.value }
		CheckBox	{ name: "uniformMaxFixed";	label: qsTr("Fixed") }
	}
}
