import QtQuick
import JASP
import JASP.Controls

Column
{
	property string parameters

	Row
	{
		DoubleField	{ name: "standardTNu";		label: qsTr("ν (df)");	min: 0;	defaultValue: 5 }
		CheckBox	{ name: "standardTNuFixed";	label: qsTr("Fixed") }
	}
}
