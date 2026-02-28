import QtQuick
import JASP
import JASP.Controls

Column
{
	property string parameters

	Row
	{
		DoubleField	{ name: "noncentralFNu1";		label: qsTr("ν₁ (df)");		min: 0;	defaultValue: 5 }
		CheckBox	{ name: "noncentralFNu1Fixed";	label: qsTr("Fixed") }
	}

	Row
	{
		DoubleField	{ name: "noncentralFNu2";		label: qsTr("ν₂ (df)");		min: 0;	defaultValue: 5 }
		CheckBox	{ name: "noncentralFNu2Fixed";	label: qsTr("Fixed") }
	}

	Row
	{
		DoubleField	{ name: "noncentralFKappa";		label: qsTr("κ (noncentrality)");	min: 0;	defaultValue: 1 }
		CheckBox	{ name: "noncentralFKappaFixed";	label: qsTr("Fixed") }
	}
}
