import QtQuick
import JASP
import JASP.Controls

Column
{
	property string parameters

	Row
	{
		DoubleField	{ name: "gompertzEta";		label: qsTr("η (shape)");	min: 0;	defaultValue: 1 }
		CheckBox	{ name: "gompertzEtaFixed";	label: qsTr("Fixed") }
	}

	Row
	{
		DoubleField	{ name: "gompertzBeta";		label: qsTr("β (scale)");	min: 0;	defaultValue: 1 }
		CheckBox	{ name: "gompertzBetaFixed";	label: qsTr("Fixed") }
	}
}
