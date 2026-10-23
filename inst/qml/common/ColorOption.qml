//
// Copyright (C) 2013-2026 University of Amsterdam
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public
// License along with this program.  If not, see
// <http://www.gnu.org/licenses/>.
//

import QtQuick
import JASP
import JASP.Controls

// A color option for R.
// ColorPicker neither sends changes to R nor restores a saved color, so the color is stored in a hidden,
// bound TextField instead: assigning its value from QML updates the option, and it restores itself from a saved file.
// The (unbound) picker is the root and has a name, so that it gets an x offset in the row, e.g. for the headerLabels of a ComponentsList.
ColorPicker
{
	id:								colorPicker

	property alias optionName:		colorValue.name
	property alias defaultValue:	colorValue.defaultValue

	name:							optionName + "Picker"
	isBound:						false
	buttonText:						""
	// typed colors are not shown on the button, so the color can only be picked from the dialog;
	// without width, the hidden field leaves no gap before the button
	textField.visible:				false
	textField.fieldWidth:			0
	value:							colorValue.value
	onValueChanged:					colorValue.value = value

	TextField
	{
		id:				colorValue
		name:			"color"
		visible:		false
		defaultValue:	jaspTheme.jaspBlue.toString()
	}
}
