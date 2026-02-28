import QtQuick
import JASP
import JASP.Controls

Column
{
	property string parameters

	Row
	{
		DoubleField	{ name: "noncentralStudentTNu";		label: qsTr("ν (df)");			min: 0;		defaultValue: 5 }
		CheckBox	{ name: "noncentralStudentTNuFixed";	label: qsTr("Fixed") }
	}

	Row
	{
		DoubleField	{ name: "noncentralStudentTKappa";		label: qsTr("κ (noncentrality)");	min: -Infinity;	defaultValue: 0 }
		CheckBox	{ name: "noncentralStudentTKappaFixed";	label: qsTr("Fixed") }
	}

	Row
	{
		DoubleField	{ name: "noncentralStudentTMu";		label: qsTr("μ (location)");	min: -Infinity;	defaultValue: 0 }
		CheckBox	{ name: "noncentralStudentTMuFixed";	label: qsTr("Fixed") }
	}

	Row
	{
		DoubleField	{ name: "noncentralStudentTSigma";		label: qsTr("σ (scale)");	min: 0;		defaultValue: 1 }
		CheckBox	{ name: "noncentralStudentTSigmaFixed";	label: qsTr("Fixed") }
	}
}
