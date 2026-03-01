import QtQuick
import JASP
import JASP.Controls

Column
{
	property string parameters

	Row
	{
		DoubleField	{ name: "noncentralTNu";		label: qsTr("ν (df)");			min: 0;		defaultValue: 5 }
		CheckBox	{ name: "noncentralTNuFixed";	label: qsTr("Fixed") }
	}

	Row
	{
		DoubleField	{ name: "noncentralTKappa";		label: qsTr("κ (noncentrality)");	min: -Infinity;	defaultValue: 0 }
		CheckBox	{ name: "noncentralTKappaFixed";	label: qsTr("Fixed") }
	}
}
