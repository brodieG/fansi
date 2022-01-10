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

## remember that when reviewing these int_max will be reset to
## the original value

old_max <- fansi:::set_int_max(15)

unitizer_sect('tabs', {
  tabs_as_spaces("\t1234567")
  tryCatch(tabs_as_spaces("\t12345678"), error=conditionMessage)

  # we're trying to trigger the failover allocation mode for
  # FANSI_size_buff, where double the requested size is over the max size

  invisible(fansi:::set_int_max(12))
  tabs_as_spaces(c("\t", "\t123"))
})
unitizer_sect('wrap', {
  invisible(fansi:::set_int_max(15))
  string <- '0123456789'
  strwrap_ctl(string, 16)
  strwrap2_ctl(string, 16, pad.end=' ')
  tce(strwrap2_ctl(string, 17, pad.end=' '))
  strwrap_ctl(string, 16, prefix='-----')
  tce(strwrap_ctl(string, 16, prefix='------'))
  strwrap_ctl(string, 16, indent=5)
  tce(strwrap_ctl(string, 16, indent=6))
  strwrap_ctl(string, 16, indent=2, prefix='---')
  tce(strwrap_ctl(string, 16, indent=3, prefix='---'))

  string2 <- '012345678901234'
  string3 <- '0123456789012345'
  strwrap_ctl(string2, 16)
  tce(strwrap_ctl(string3, 16))

  string4 <- '\033[31m0123456789'
  tce(strwrap_ctl(string4, 16))

  ## Overflow when wrap adds a closing tag
  invisible(fansi:::set_int_max(9))
  tce(strwrap_ctl("A\033[31m a", 5))
})
unitizer_sect('html', {
  invisible(fansi:::set_int_max(38))
  sgr_to_html("\033[31ma")
  # whole string over
  tce(sgr_to_html("\033[31mab"))
  # Sequences alone over
  tce(sgr_to_html("\033[31m\033[42mhello"))
  # Over due to classes
  invisible(fansi:::set_int_max(57))
  tce(sgr_to_html("\033[31m\033[42mhello", classes=TRUE))
  # Fits exactly
  invisible(fansi:::set_int_max(58))
  (x <- sgr_to_html("\033[31m\033[42mhello", classes=TRUE))
  nchar(x)
  # Over
  invisible(fansi:::set_int_max(4))
  tce(sgr_to_html("hello"));

  tce(html_esc("hello"));
  tce(html_esc("<"));
  tce(html_esc("<!"));
  tce(html_esc("&"));
  tce(html_esc("'"));
})
unitizer_sect('unhandled', {
  invisible(fansi:::set_int_max(10))
  string <- paste0(rep("\a", 10), collapse="")
  unhandled_ctl(string)
  tcw(unhandled_ctl(c('\a', string)))
  suppressWarnings(unhandled_ctl(c('\a', string)))
})
unitizer_sect('size buffer', {
  invisible(fansi:::set_int_max(old_max))
  fansi:::size_buff(c(0L, 127L, 128L, 64L, 200L, 1024L))
  fansi:::size_buff(c(0L, 127L, -128L))

  invisible(fansi:::set_int_max(130))
  fansi:::size_buff(c(0L, 127L, 128L, 64L, 200L, 1024L))
  invisible(fansi:::set_int_max(64))
  fansi:::size_buff(c(0L, 32L, 63L, 64L))
  fansi:::size_buff(c(0L, 32L, 63L, 65L))

  # see src/write.c for details on what these should be and why
  invisible(fansi:::set_int_max(old_max))
  dat <- fansi:::size_buff_prot_test()
  dat['first', 'self']       == dat['smaller 1.0', 'self']
  dat['new buff', 'prev']    == dat['grow 1.0', 'self']
  dat['new buff', 'prev']    != dat['new buff', 'self']
  dat['smaller 1.1', 'self'] == dat['grow 1.0', 'self']
  dat['smaller 2.0', 'self'] == dat['new buff', 'self']
  dat['smaller 2.0', 'prev'] == dat['new buff', 'prev']
  dat['smaller 2.0', 'prev'] == dat['grow 2.0', 'prev']
  dat['grow 1.1', 'prev']    == dat['grow 2.0', 'self']
  dat['grow 2.1', 'prev']    == dat['grow 1.1', 'self']
})
unitizer_sect('misc', {
  invisible(fansi:::set_int_max(5))
  # this is from trying to create result matrix names, so need longer than that
  # to test other stuff
  substr_ctl("\033[43mA B", 5, 5)
  # substr int max long?
  substr_ctl("12345", 1, 5)
  substr_ctl("123456", 1, 6)

  ## this caused a segfault due to missing comma in error(...), but with change
  ## to returning R_BlankString not a thing anymore
  ## invisible(fansi:::set_int_max(1L))
  ## substr_ctl("1", 2, 2)
})
fansi:::reset_limits()

unitizer_sect('R_len_t', {
  old_rlent <- fansi:::set_rlent_max(5)
  tabs_as_spaces("A\tB")
  new_rlent <- fansi:::set_rlent_max(old_rlent)
})
fansi:::reset_limits()

unitizer_sect('internal', {
  tce(.Call(fansi:::FANSI_buff_test_reset))
  tce(.Call(fansi:::FANSI_buff_test_copy_overflow))
  tce(.Call(fansi:::FANSI_buff_test_mcopy_overflow))
  tce(.Call(fansi:::FANSI_buff_test_fill_overflow))
})
