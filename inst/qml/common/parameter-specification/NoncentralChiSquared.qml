import QtQuick
import JASP
import JASP.Controls

Column
{
	property string parameters

	Row
	{
		DoubleField	{ name: "noncentralChiSquaredNu";		label: qsTr("ν (df)");			min: 0;	defaultValue: 5 }
		CheckBox	{ name: "noncentralChiSquaredNuFixed";	label: qsTr("Fixed") }
	}

	Row
	{
		DoubleField	{ name: "noncentralChiSquaredKappa";		label: qsTr("κ (noncentrality)");	min: 0;	defaultValue: 1 }
		CheckBox	{ name: "noncentralChiSquaredKappaFixed";	label: qsTr("Fixed") }
	}
}
