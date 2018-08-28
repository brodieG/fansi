## Copyright (C) 2018  Brodie Gaslam
##
## This file is part of "fansi - ANSI Control Sequence Aware String Functions"
##
## This program is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 2 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## Go to <https://www.r-project.org/Licenses/GPL-2> for a copy of the license.

## Order of these is important as typically we convert them to integer codes
## with `match`

## Valid values for the `term.cap` argument

VALID.TERM.CAP <- c('bright', '256', 'truecolor')

## Valid values for the `what` argument, 
## * nl: newlines
## * c0: other c0, including del
## * sgr: SGR ANSI CSI
## * csi: ANSI CSI, excluding SGR
## * esc: other \033 escape sequences, we assume they are two long
##
## REMEMBER TO UPDATE FANSI_STRIP_ALL CONSTANT IF WE MODIFY THIS

VALID.STRIP <- c("all", "nl", "c0", "sgr", "csi", "esc")
