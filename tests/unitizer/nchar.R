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

unitizer_sect('basic tests', {
  nchar_ctl(c('hello', 'world'))
  nchar_ctl(c('hello', 'world'), type='width')
  nchar_ctl(c('hello', 'world'), type='wi') # partial match

  # Keep NA
  na.world <- c('hello', NA, 'world', '')
  identical(nchar_ctl(na.world), nchar(na.world))
  identical(
    nchar_ctl(na.world, keepNA=FALSE),
    nchar(na.world, keepNA=FALSE)
  )
  identical(
    nchar_ctl(na.world, keepNA=NA, type='width'),
    nchar(na.world, keepNA=NA, type='width')
  )
  identical(
    nchar_ctl(na.world, keepNA=TRUE, type='width'),
    nchar(na.world, keepNA=TRUE, type='width')
  )
  identical(nzchar_ctl(na.world), nzchar(na.world))
  identical(nzchar_ctl(na.world, keepNA=TRUE), nzchar(na.world, keepNA=TRUE))
  identical(nzchar_ctl(na.world, keepNA=NA), nzchar(na.world, keepNA=NA))

  identical(nchar_ctl(na.world, type='bytes'), nchar(na.world, type='bytes'))
  identical(
    nchar_ctl(na.world, keepNA=FALSE, type='bytes'),
    nchar(na.world, keepNA=FALSE, type='bytes')
  )
  identical(
    nchar_ctl(na.world, keepNA=TRUE, type='bytes'),
    nchar(na.world, keepNA=TRUE, type='bytes')
  )
  w.names <- c(a='hello', b='world')
  identical(nchar_ctl(w.names), nchar(w.names))
  w.dim <- matrix(
    letters[1:6], 2, 3, dimnames=list(X=LETTERS[2:3], Y=LETTERS[24:26])
  )
  identical(nchar_ctl(w.dim), nchar(w.dim))

  # Strip equivalence
  hw.sgr <- c(
    'hello', 'wo\033[42mrld', '\033[31m', 'mo\non', 'star\033[p',
    'link: \033]8;;xy.z\033\\hello\033]8;;\033\\ world'
  )
  identical(nchar_ctl(hw.sgr), nchar(strip_ctl(hw.sgr)))

  # Bad encoding
  x <- "\xf0"
  if(isTRUE(l10n_info()[['UTF-8']])) {
    # don't translate unknown in UTF-8 locale
    inherits(try(nchar_ctl(x), silent=TRUE), "try-error")
  } else TRUE
  Encoding(x) <- "UTF-8"
  identical(nzchar_ctl(x), nzchar(x))
  nchar_ctl(x)
  nchar_ctl(c("", x))
  identical(nchar_ctl(x, allowNA=TRUE), nchar(x, allowNA=TRUE))
})
unitizer_sect('with escapes', {
  esc.2 <- "\n\r\033P\033[31m\a"

  nchar_ctl(esc.2)
  nchar_ctl(esc.2, warn=FALSE)
  nzchar_ctl(esc.2)
  nzchar_ctl(esc.2, warn=FALSE)

  # not an SGR

  nchar_ctl("\033[31#mworld", ctl="sgr")
  nchar_ctl("\033[31#mworld", ctl="csi")
})
unitizer_sect('ctl', {
  esc.3 <- "\n\t\033[31m\033[41!m\033p"
  nzchar_ctl(esc.3, warn=FALSE)
  nzchar_ctl(sprintf("%sa", esc.3), warn=FALSE)
  nzchar_ctl(esc.3, ctl=c('sgr', 'csi', 'esc'), warn=FALSE)
  nzchar_ctl(esc.3, ctl=c('c0', 'nl'), warn=FALSE)
  nzchar_ctl("\n\t\n", ctl=c('nl'), warn=FALSE)
  nzchar_ctl("\t\n", ctl=c('nl'), warn=FALSE)
})
unitizer_sect('corner cases', {
  ## Bad byte in ESC, generally okay as they are not emitted, saving to
  ## variables to avoid issues with parse/deparse in unitizer < 1.4.18

  ncbad <- c(
    "\033\x80", "\033[31;\x80m", "\033[31;\x80p",  "\033]8;\x80;a.b\033\\",
    "\033];\x80;a.b\033\\"
  )
  Encoding(ncbad) <- "UTF-8"
  nchar_ctl(ncbad)

  ## Old R version behavior
  fansi:::set_rver(numeric_version("3.2.1"))
  nzchar_ctl(c("\033[31mA", "\033[31m"))
  nchar_ctl(c("\033[31mA", "\033[31m"))
  fansi:::set_rver()
})
unitizer_sect('bad inputs', {
  nchar_ctl(9:10, warn=1:3)
  nchar_ctl("hello\033[31m world", allowNA=1:3)
  nchar_ctl("hello\033[31m world", keepNA=1:3)
  nchar_ctl("hello\033[31m world", strip=1:3)
  nchar_ctl("hello\033[31m world", ctl="bananas")
  nchar_ctl("hello\033[31m world", type=NA_character_)
  nchar_ctl("hello\033[31m world", type=1)
  nchar_ctl("hello\033[31m world", type="bananas")

  nzchar_ctl(9:10, warn=1:3)
  nzchar_ctl("hello\033[31m world", keepNA=1:3)
  nzchar_ctl("hello\033[31m world", ctl=1)
  nzchar_ctl("hello\033[31m world", ctl="bananas")
})
