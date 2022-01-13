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
unitizer_sect("wrap", {
  # two ways to terminate OSC, with an ST, or with a BELL (\a)
  txt <- "This is a link"
  base.st <- '%s\033]8;%s;%s\033\\%s\033]8;;\033\\%s'
  base.a <- '%s\033]8;%s;%s\a%s\033]8;;\a%s'
  url <- "https://x.yz"
  u0 <- sprintf(base.st, "", "", url, txt, "")
  u1 <- sprintf(base.a, "", "", url, txt, "")

  strtrim_ctl(u0, 5)

  strwrap_ctl(u0, 5)
  strwrap_ctl(u0, 8)
  strwrap_ctl(u1, 5)

  u2 <- sprintf(base.st, "", "id=one", url, txt, "")
  u2a <- sprintf(base.st, "", "id=", url, txt, "")
  u2b <- sprintf(base.st, "", "id", url, txt, "")
  u3 <- sprintf(base.st, "", "title=ab:id=one", url, txt, "")
  u4 <- sprintf(base.st, "", "id=one:title=ba", url, txt, "")
  u6 <- sprintf(base.st, "", "id=one~title=ba", url, txt, "")

  strwrap_ctl(u2, 5)
  strwrap_ctl(u3, 5)
  strwrap_ctl(u4, 5)
  strwrap_ctl(u6, 5)

  u7 <- sprintf(base.st, "AB", "id=one", url, txt, "CD")

  strwrap_ctl(u7, 8)

  # Mixed SGR
  txt2 <- "This\033[34m is a link\033[49m"
  u8 <- sprintf(base.st, "A\033[43mB", "", url, txt2, "CD")

  strwrap_ctl(u8, 7)

  # Too many semicolons
  url2 <- ";wow;https://x.yz"
  u9 <- sprintf(base.st, "", "", url2, txt, "")

  strwrap_ctl(u9, 5)

  # OOB char for OSC
  u10 <- sprintf(base.st, "", "\x07", url, txt, "")  # not good
  u11 <- sprintf(base.st, "", "\x0e", url, txt, "")  # 0x08-0x20 okayish
  Encoding(u10) <- "UTF-8"
  Encoding(u11) <- "UTF-8"
  # Unsupported parameter (only id supported), questionable whether
  # this should be allowed to carry or not.
  u11a <- sprintf(base.st, "", "hello", url, txt, "")

  strwrap_ctl(u10, 5)
  strwrap_ctl(u11, 5)
  strwrap_ctl(u11a, 5)
  nchar_ctl(c(u10, u11, u11a))

  # OOB chars for URL
  u12 <- sprintf(base.st, "", "", "\x08", txt, "")
  u12a <- sprintf(base.st, "", "", "\x80", txt, "")
  Encoding(u12) <- "UTF-8"
  Encoding(u12a) <- "UTF-8"

  strwrap_ctl(u12, 5)
  strwrap_ctl(u12a, 5)
  nchar_ctl(c(u12, u12a))
  nchar_ctl(c(u12, u12a), allowNA=TRUE)

  # Unterminated, gets consumed, not shown if we're terminating.
  u13 <- "a\033]8;;THE END"
  u13a <- "a\033];;THE END"
  u14 <- "a\033]8;;THE END\033]8;;NO?"
  u15 <- "a\033]8;;THE END\033]8;;\033["
  strwrap_ctl(u13, 5);
  strwrap_ctl(u13, 5, terminate=FALSE);
  strwrap_ctl(u13a, 5);
  strwrap_ctl(u14, 5);
  strwrap_ctl(u15, 5);
  nchar_ctl(c(u13, u13a, u14, u15))

  # Empty Fields
  u16 <- sprintf(base.st, "", "", "", txt, "")
  u17 <- sprintf(base.st, "", "", "", "", "")
  strwrap_ctl(u16, 5)
  strwrap_ctl(u17, 5)

  # Terminate
  strwrap_ctl(u0, 5, terminate=FALSE)

  # Carry
  open <- '\033]8;;ab.c\033\\'
  u21 <- c('he\033[44mllo \033]8;;de.f\033\\world', 'night moon')
  strwrap_ctl(u21, 5, carry="\033[33m")
  strwrap_ctl(u21, 5, carry=paste0(open, "\033[33m"))
})
unitizer_sect('normalize', {
  # Adjacent URLs with same ID should be merged
  u18 <- "A\033]8;id=a;x.yz\033\\B\033]8;id=a;x.yz\033\\C\033]8;;\033\\D"
  normalize_state(u18)
  # Merge even when URL fully closed
  u18a <- "A\033]8;id=a;x.yz\033\\B\033]8;;\033\\\033]8;id=a;x.yz\033\\C\033]8;;\033\\D"
  normalize_state(u18a)

  # Abutting URLs with no intervening content: last wins
  u18b <- "A\033]8;id=a;x.yz\033\\\033]8;id=b;x.yz\033\\C\033]8;;\033\\D"
  normalize_state(u18b)

  # These two should not merge
  u19 <- "A\033]8;id=a;x.yz\033\\B\033]8;id=b;x.yz\033\\C\033]8;;\033\\D"
  u20 <- "A\033]8;id=a;x.yz\033\\B\033]8;id=a;w.yz\033\\C\033]8;;\033\\D"
  normalize_state(u19)
  normalize_state(u20)
})
unitizer_sect('substr', {
  substr_ctl(u0, 6, 9)
  substr_ctl(u0, 6, 9, terminate=FALSE)
  substr_ctl("hello world", 3, 8, carry="\033]8;;a.b\033\\")

  # corner cases with bad/non-portable bytes
  np.bytes <- c(
    "A\033]8;a=\x0d:id=c;x.y\033\\B",
    "A\033]8;a=c:id=\x0d;x.y\033\\B",
    "A\033]8;a=c:id=d;x.\x0d\033\\B",
    "A\033]8;a=c:id=d;x.\x80\033\\B"
  )
  Encoding(np.bytes) <- "UTF-8"
  substr_ctl(np.bytes[1:3], 2, 2)
  substr_ctl(np.bytes[4], 2, 2)
})
unitizer_sect('tohtml', {
  to_html(u0)

  to_html("A\033[44mB\033]8;;x.y\033\\C\033[33m\033]8;;\033\\D")
  to_html("A\033[44mB\033]8;;x.y\033\\C\033[33m\033]8;;w.z\033\\D")

  u23 <- c(
    "A \033[44mB \033]8;;x.y\033\\C \033[33m\033]8;;w.z\033\\D",
    "E \033]8;;www.z.com\033\\F \033[4mG",
    "H \033]8;;\033\\\033[48;5;67m I"
  )
  to_html(u23)
  to_html(strwrap_ctl(u23, 4))
})
unitizer_sect('osc', {
  # Non-URL OSC
  nchar_ctl("\033]hello \aworld")
  nchar_ctl("\033]hello \033\\world")
  # but don't support OSC (interpret as 2 char ESC)
  nchar_ctl("\033]hello \033\\world", ctl=c('all', 'osc'))

  x <- "\033]hello\x80\033\\world"
  Encoding(x) <- "UTF-8"
  nchar_ctl(x)
  nchar_ctl("\033]hello world")
})

