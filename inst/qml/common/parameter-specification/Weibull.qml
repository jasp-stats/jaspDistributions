import QtQuick
import JASP
import JASP.Controls

Column
{
	property string parameters

	Row
	{
		DoubleField	{ name: "weibullShape";		label: qsTr("shape");	min: 0;	defaultValue: 2 }
		CheckBox	{ name: "weibullShapeFixed";	label: qsTr("Fixed") }
	}

	Row
	{
		DoubleField	{ name: "weibullScale";		label: qsTr("scale");	min: 0;	defaultValue: 1 }
		CheckBox	{ name: "weibullScaleFixed";	label: qsTr("Fixed") }
	}
}
