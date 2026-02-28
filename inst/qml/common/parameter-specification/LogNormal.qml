import QtQuick
import JASP
import JASP.Controls

Column
{
	property string parameters

	Row
	{
		DoubleField	{ name: "logNormalMu";		label: qsTr("μ (log mean)");		min: -Infinity;	defaultValue: 0 }
		CheckBox	{ name: "logNormalMuFixed";	label: qsTr("Fixed") }
	}

	Row
	{
		DoubleField	{ name: "logNormalSigma";		label: qsTr("σ (log std. deviation)");	min: 0;	defaultValue: 1 }
		CheckBox	{ name: "logNormalSigmaFixed";	label: qsTr("Fixed") }
	}
}
