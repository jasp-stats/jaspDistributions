import QtQuick
import JASP
import JASP.Controls

Column
{
	property string parameters

	Row
	{
		DoubleField	{ name: "shiftedWeibullShape";		label: qsTr("shape");	min: 0;			defaultValue: 2 }
		CheckBox	{ name: "shiftedWeibullShapeFixed";	label: qsTr("Fixed") }
	}

	Row
	{
		DoubleField	{ name: "shiftedWeibullScale";		label: qsTr("scale");	min: 0;			defaultValue: 1 }
		CheckBox	{ name: "shiftedWeibullScaleFixed";	label: qsTr("Fixed") }
	}

	Row
	{
		DoubleField	{ name: "shiftedWeibullShift";		label: qsTr("shift");	min: -Infinity;	defaultValue: 0 }
		CheckBox	{ name: "shiftedWeibullShiftFixed";	label: qsTr("Fixed") }
	}
}
