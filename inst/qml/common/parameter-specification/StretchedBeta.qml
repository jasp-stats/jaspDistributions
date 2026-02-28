import QtQuick
import JASP
import JASP.Controls

Column
{
	property string parameters

	Row
	{
		DoubleField	{ name: "stretchedBetaAlpha";		label: qsTr("α (shape)");	min: 0;		defaultValue: 2 }
		CheckBox	{ name: "stretchedBetaAlphaFixed";	label: qsTr("Fixed") }
	}

	Row
	{
		DoubleField	{ name: "stretchedBetaBeta";		label: qsTr("β (shape)");	min: 0;		defaultValue: 2 }
		CheckBox	{ name: "stretchedBetaBetaFixed";	label: qsTr("Fixed") }
	}

	Row
	{
		DoubleField	{ name: "stretchedBetaMin";	label: qsTr("min");	min: -Infinity;	defaultValue: 0 }
	}

	Row
	{
		DoubleField	{ name: "stretchedBetaMax";	label: qsTr("max");	min: -Infinity;	defaultValue: 1 }
	}
}
