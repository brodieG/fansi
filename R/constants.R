## Copyright (C) Brodie Gaslam
##
## This file is part of "fansi - ANSI Control Sequence Aware String Functions"
##
## This program is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 2 or 3 of the License.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## Go to <https://www.r-project.org/Licenses> for copies of the licenses.

## Order of these is important as typically we convert them to integer codes
## with `match`

## Valid values for the `term.cap` argument

VALID.TERM.CAP <- c('all', 'bright', '256', 'truecolor', 'old')

## Valid values for the `ctl` argument,
##
## * nl: newlines
## * c0: other c0, including del
## * sgr: SGR ANSI CSI
## * csi: ANSI CSI, excluding SGR
## * esc: other \033 escape sequences, we assume they are two long
##
## These will eventually encoded in an integer as powers of 2, except for `all`
## which acts as a negation (see FANSI_ctl_as_int), so "nl" is 2^0, "c0" is 2^1,
## and so on.
##
## REMEMBER TO UPDATE CTL_ALL CONSTANT IF WE MODIFY THIS

VALID.CTL <- c("all", "nl", "c0", "sgr", "csi", "esc", "url", "osc")
