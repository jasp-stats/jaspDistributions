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
import QtQuick.Layouts
import JASP
import JASP.Controls
import "./common" as Common

Form 
{

    Common.DistributionSpecification { }

	Group
	{
		Layout.columnSpan: 2
        DropDown
        {
            name: "variable"
            label: qsTr("Fit the distribution on")
            showVariableTypeIcon: true
            addEmptyValue: true
            placeholderText: qsTr("Do not fit the distribution")
            info: qsTr("Select a variable from the dataset to use for fitting the distribution.")
        }
    }


    Common.ShowDistribution { }
    Common.GenerateDisplayData { continuous: true}
    Common.ParameterEstimates { }
    Common.AssessFit { }

}