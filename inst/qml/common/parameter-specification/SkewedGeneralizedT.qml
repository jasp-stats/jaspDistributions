import QtQuick
import JASP
import JASP.Controls

Column
{
	property string parameters

	Row
	{
		DoubleField	{ name: "skewedGeneralizedTMu";		label: qsTr("μ (location)");	min: -Infinity;	defaultValue: 0 }
		CheckBox	{ name: "skewedGeneralizedTMuFixed";	label: qsTr("Fixed") }
	}

	Row
	{
		DoubleField	{ name: "skewedGeneralizedTSigma";		label: qsTr("σ (scale)");	min: 0;		defaultValue: 1 }
		CheckBox	{ name: "skewedGeneralizedTSigmaFixed";	label: qsTr("Fixed") }
	}

	Row
	{
		DoubleField	{ name: "skewedGeneralizedTLambda";		label: qsTr("λ (skew)");	min: -1;	max: 1;	defaultValue: 0 }
		CheckBox	{ name: "skewedGeneralizedTLambdaFixed";	label: qsTr("Fixed") }
	}

	Row
	{
		DoubleField	{ name: "skewedGeneralizedTP";		label: qsTr("p (kurtosis)");	min: 0;	defaultValue: 2 }
		CheckBox	{ name: "skewedGeneralizedTPFixed";	label: qsTr("Fixed") }
	}

	Row
	{
		DoubleField	{ name: "skewedGeneralizedTQ";		label: qsTr("q (kurtosis)");	min: 0;	defaultValue: 10 }
		CheckBox	{ name: "skewedGeneralizedTQFixed";	label: qsTr("Fixed") }
	}
}
