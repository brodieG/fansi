## Copyright (C) 2022 Brodie Gaslam
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

library(unitizer)
library(fansi)

unitizer_sect('simple tabs', {
  string <- '1\t12\t123\t1234\t12345678'
  tabs_as_spaces(string)
  tabs_as_spaces(string, c(2, 3, 4, 5, 8))
  tabs_as_spaces(string, c(2, 8))
  tabs_as_spaces(1:3)
})
unitizer_sect('newlines', {
  string.n <- paste0(
    '1\t12\t123\t1234\t12345678\n',
    '1\t12\t123\t1234\t12345678'
  )
  tabs_as_spaces(string.n)
  tabs_as_spaces(string.n, c(2, 3, 4, 8))
  tabs_as_spaces(string.n, c(2, 8))
})
unitizer_sect('corner cases', {
  tabs_as_spaces('')
  tabs_as_spaces('\t')
  tabs_as_spaces('\n')
  tabs_as_spaces(c(string, string, string))
  tabs_as_spaces('\t\t')
})
unitizer_sect('bad inputs', {
  tabs_as_spaces(string, warn=1:3)
  tabs_as_spaces(string, tab.stops='hello')
  tabs_as_spaces(string, ctl='hello')
  tabs_as_spaces(string, ctl=0)
})
