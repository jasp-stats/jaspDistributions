import QtQuick
import JASP
import JASP.Controls

Column
{
	property string parameters;

	Row
	{
		visible: parameters === "lambda"
		DoubleField	{ name: "exponentialLambda";		label: qsTr("λ (rate)");	min: 0;	defaultValue: 1 }
		CheckBox	{ name: "exponentialLambdaFixed";	label: qsTr("Fixed") }
	}

	Row
	{
		visible: parameters === "beta"
		DoubleField	{ name: "exponentialBeta";		label: qsTr("β (scale)");	min: 0;	defaultValue: 1 }
		CheckBox	{ name: "exponentialBetaFixed";	label: qsTr("Fixed") }
	}
}
