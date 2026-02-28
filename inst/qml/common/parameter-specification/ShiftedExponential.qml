import QtQuick
import JASP
import JASP.Controls

Column
{
	property string parameters

	Row
	{
		visible: parameters === "lambda"
		DoubleField	{ name: "shiftedExponentialLambda";		label: qsTr("λ (rate)");	min: 0;			defaultValue: 1 }
		CheckBox	{ name: "shiftedExponentialLambdaFixed";	label: qsTr("Fixed") }
	}

	Row
	{
		visible: parameters === "beta"
		DoubleField	{ name: "shiftedExponentialBeta";		label: qsTr("β (scale)");	min: 0;			defaultValue: 1 }
		CheckBox	{ name: "shiftedExponentialBetaFixed";	label: qsTr("Fixed") }
	}

	Row
	{
		DoubleField	{ name: "shiftedExponentialShift";		label: qsTr("shift");		min: -Infinity;	defaultValue: 0 }
		CheckBox	{ name: "shiftedExponentialShiftFixed";	label: qsTr("Fixed");	checked: true}
	}
}
