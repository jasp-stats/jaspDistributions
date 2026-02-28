import QtQuick
import JASP
import JASP.Controls

Column
{
	property string parameters

	Row
	{
		DoubleField	{ name: "centralFNu1";		label: qsTr("ν₁ (df)");	min: 0;	defaultValue: 5 }
		CheckBox	{ name: "centralFNu1Fixed";	label: qsTr("Fixed") }
	}

	Row
	{
		DoubleField	{ name: "centralFNu2";		label: qsTr("ν₂ (df)");	min: 0;	defaultValue: 5 }
		CheckBox	{ name: "centralFNu2Fixed";	label: qsTr("Fixed") }
	}
}
