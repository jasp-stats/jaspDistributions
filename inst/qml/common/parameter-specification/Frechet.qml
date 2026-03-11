import QtQuick
import JASP
import JASP.Controls

Column
{
	property string parameters

	Row
	{
		DoubleField	{ name: "frechetAlpha";		label: qsTr("α (shape)");	min: 0;		defaultValue: 2 }
		CheckBox	{ name: "frechetAlphaFixed";	label: qsTr("Fixed") }
	}

	Row
	{
		DoubleField	{ name: "frechetSigma";		label: qsTr("σ (scale)");	min: 0;		defaultValue: 1 }
		CheckBox	{ name: "frechetSigmaFixed";	label: qsTr("Fixed") }
	}

	Row
	{
		DoubleField	{ name: "frechetTheta";		label: qsTr("θ (location)");	min: -Infinity;	defaultValue: 0 }
		CheckBox	{ name: "frechetThetaFixed";	label: qsTr("Fixed") }
	}
}
