import QtQuick
import JASP
import JASP.Controls

Column
{
	property string parameters

	Row
	{
		DoubleField	{ name: "laplaceMu";		label: qsTr("μ (location)");	min: -Infinity;	defaultValue: 0 }
		CheckBox	{ name: "laplaceMuFixed";	label: qsTr("Fixed") }
	}

	Row
	{
		DoubleField	{ name: "laplaceBeta";		label: qsTr("β (scale)");	min: 0;	defaultValue: 1 }
		CheckBox	{ name: "laplaceBetaFixed";	label: qsTr("Fixed") }
	}
}
