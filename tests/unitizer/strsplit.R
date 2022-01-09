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

unitizer_sect("basic splits", {
  str.0 <- c("hello world", "goodbye moon")
  identical(strsplit(str.0[1], " "), strsplit_ctl(str.0[1], " "))
  identical(strsplit(str.0, "h"), strsplit_ctl(str.0, "h"))
  identical(strsplit(str.0, "m"), strsplit_ctl(str.0, "m"))
  identical(strsplit(str.0, "g"), strsplit_ctl(str.0, "g"))

  str.1 <- "hello\033[31m world"
  str.2 <- "\033[42m hello\033[m world, Goodbye Moon"
  strsplit_ctl(str.1, " ")
  strsplit_ctl(str.1, "hello")
  strsplit_ctl(str.2, ", ")
  strsplit_ctl(c(str.1, "hello world", str.2), "hello")
})
unitizer_sect("corner cases", {
  strsplit_ctl("hello\033[31m world", "")
  strsplit_ctl("hello\033[31m world", "[", fixed=TRUE)

  strsplit_ctl("hello\033[31m world", NA_character_)
  strsplit_ctl("hello\033[31m world", character())
  strsplit_ctl("hello\033[31m world", letters)

  splits <- c('h', 'e', 'o', 'llo', 'x', 'hello')
  str.spl1 <- strsplit_ctl(rep("hello", 6), splits)
  str.spl2 <- strsplit(rep("hello", 6), splits)
  identical(str.spl1, str.spl2)

  str.spl3 <- strsplit_ctl(rep("\033[31mhello\033[39m", 6), splits)
  str.spl3
  identical(lapply(str.spl3, strip_ctl), str.spl2)

  strsplit_ctl("", " ")
  strsplit_ctl("", "")
  strsplit_ctl(c("\033[31mab\033[0m", ""), "")

  strsplit_ctl("hello", NULL)

  # split by escape

  str.sp14 <- c("\033[31mhello\nworld", "\ngoodbye\nmoon")
  strsplit_ctl(str.sp14, "\n")
  strsplit_sgr(str.sp14, "\n")
  strsplit_ctl(str.sp14, "\n", ctl=c('all', 'nl'))
})
unitizer_sect('bad intputs', {
  str.bytes <- "\xDE"
  Encoding(str.bytes) <- "bytes"
  strsplit_ctl(str.bytes, "")
  strsplit_ctl(str.2, NA)
  strsplit_ctl(str.2, "", warn=NULL)
  strsplit_ctl(str.2, "", fixed=NA_integer_)
  strsplit_ctl(str.2, "", perl=NA_integer_)
  strsplit_ctl(str.2, "", useBytes=NA_integer_)
  strsplit_ctl(str.2, "", term.cap=1:3)
  strsplit_ctl(str.2, "", term.cap="bananas")
  strsplit_ctl(str.2, "", ctl=1:3)
  strsplit_ctl(str.2, "", ctl="bananas")
  strsplit_ctl("a b", str.bytes)
})
unitizer_sect('issue 55', {
  # can't work, ideally would issue a warning, but detecting stripped
  # escape sequences in regular expression will be complicated

  strsplit_ctl("hello\nworld", "\n")
  strsplit_sgr("hello\033[31mworld", "\033[31m", fixed=TRUE)

  # should work

  strsplit_ctl("a\nb", "\n", ctl=c('all', 'nl'))
  strsplit_sgr("hello\nworld", "\n")
})
