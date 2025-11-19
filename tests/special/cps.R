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

library(unitizer)
library(fansi)

unitizer_sect('Test all Code Points', {
  SURROGATE_START <- 0xD800
  SURROGATE_END <- 0xDFFF
  surrogate_range <- SURROGATE_START:SURROGATE_END
  all_codepoints <- setdiff(0:0x10FFFF, c(surrogate_range, 0x1B))
  all_chars <- intToUtf8(all_codepoints, multiple = TRUE)
  fansi_widths <- nchar_ctl(all_chars, type = 'width')
  table(fansi_widths, useNA='ifany')
})
