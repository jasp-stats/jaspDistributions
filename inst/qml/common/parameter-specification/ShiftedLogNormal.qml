import QtQuick
import JASP
import JASP.Controls

Column
{
	property string parameters

	Row
	{
		DoubleField	{ name: "shiftedLogNormalMu";		label: qsTr("μ (log-mean)");		min: -Infinity;	defaultValue: 0 }
		CheckBox	{ name: "shiftedLogNormalMuFixed";	label: qsTr("Fixed") }
	}

	Row
	{
		DoubleField	{ name: "shiftedLogNormalSigma";		label: qsTr("σ (log-std. dev.)");	min: 0;			defaultValue: 1 }
		CheckBox	{ name: "shiftedLogNormalSigmaFixed";	label: qsTr("Fixed") }
	}

	Row
	{
		DoubleField	{ name: "shiftedLogNormalShift";		label: qsTr("shift");				min: -Infinity;	defaultValue: 0 }
		CheckBox	{ name: "shiftedLogNormalShiftFixed";	label: qsTr("Fixed");	checked: true }
	}
}
