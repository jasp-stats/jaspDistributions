import QtQuick
import JASP
import JASP.Controls

Column
{
	property string parameters;
	Row
	{
		DoubleField { name: "normalMu"; label: qsTr("μ");  min: -Infinity; defaultValue: 0 }
		CheckBox    { name: "normalMuFixed"; label: qsTr("Fixed") }
	}

	Row
	{
		visible: parameters === "sigma"
		DoubleField { name: "normalSigma";      label: qsTr("σ");  min: 0; defaultValue: 1 }
		CheckBox    { name: "normalSigmaFixed"; label: qsTr("Fixed") }
	}
}