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

## Helpers to extract the condition message only due to instability in
## C level error/warning in displaying the call or not.
##
## This seems to be related to whether functions are byte compiled or not, with
## non-bc ones not getting the call.  Possible we stopped seeing issues with the
## advent of always byte compiling packages.

tce <- function(x) tryCatch(x, error=conditionMessage)
tcw <- function(x) tryCatch(x, warning=conditionMessage)

## writeLines!
wl <- function(x) writeLines(c(x, "\033[m"))
