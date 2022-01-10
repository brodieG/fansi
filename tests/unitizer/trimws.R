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

library(fansi)

unitizer_sect("Equivalence", {
  txt1 <- c(
    "", "hello", " \t \n\r he\nl\tl\r o \r\t\n", "he\nl\tl\r o \r\t\n ",
    " \t \n\r he\nl\tl\r o", "  ", " \r\n"
  )
  identical(trimws(txt1), trimws_ctl(txt1))
  identical(trimws(txt1, which='left'), trimws_ctl(txt1, which='left'))
  identical(trimws(txt1, which='right'), trimws_ctl(txt1, which='right'))
})
unitizer_sect("Controls", {
  txt2 <- c(
    "\033[31m he\033[42;1m llo \033[49m\n\t ",
    " \033]8;;https://x.yz\033\\\tLINK\033[45m hello \033]8;;\033\\ \033[31m"
  )
  trimws_ctl(txt2)
  trimws_ctl(txt2, which='left')
  trimws_ctl(txt2, which='right')
  trimws_ctl(txt2, ctl=c("all", "url", "osc"))
  trimws_ctl(txt2, ctl=c("all", "sgr", "csi"))
  # A control isn't a control
  trimws_ctl(" \r\a A \a\t ", ctl=c("all", "c0"))
})
unitizer_sect("Errors / Corner caess", {
  trimws_ctl(character())
  trimws_ctl("hello", which="top")
  trimws_ctl("hello", whitespace=" ")
})
