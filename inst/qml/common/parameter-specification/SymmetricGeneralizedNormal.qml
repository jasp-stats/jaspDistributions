import QtQuick
import JASP
import JASP.Controls

Column
{
	property string parameters

	Row
	{
		DoubleField	{ name: "symmetricGeneralizedNormalMu";		label: qsTr("μ (location)");	min: -Infinity;	defaultValue: 0 }
		CheckBox	{ name: "symmetricGeneralizedNormalMuFixed";	label: qsTr("Fixed") }
	}

	Row
	{
		DoubleField	{ name: "symmetricGeneralizedNormalAlpha";		label: qsTr("α (scale)");	min: 0;	defaultValue: 1 }
		CheckBox	{ name: "symmetricGeneralizedNormalAlphaFixed";	label: qsTr("Fixed") }
	}

	Row
	{
		DoubleField	{ name: "symmetricGeneralizedNormalBeta";		label: qsTr("β (shape)");	min: 0;	defaultValue: 2 }
		CheckBox	{ name: "symmetricGeneralizedNormalBetaFixed";	label: qsTr("Fixed") }
	}
}
