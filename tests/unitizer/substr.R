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

unitizer_sect("Simple", {
  str01 <- sprintf("hello %sworld%s how", red, inv);

  substr_ctl(str01, 1, 7)
  substr_ctl(str01, 7, 11)
  substr_ctl(str01, 8, 10)
  substr_ctl(str01, 8, 14)

  str02 <- sprintf(
    "%shello world %sit's a %scrazy world%s out there %sisn't it%s%s right?",
    grn.bg, red, end, rgb.und, inv, end, rgb.256
  )
  # enable truecolor as not enabled by default
  term.cap <- c('bright', '256', 'truecolor')

  substr_ctl(str02, 1, 7)
  substr_ctl(str02, 10, 20)
  substr_ctl(str02, 15, 40, term.cap=term.cap)
  substr_ctl(str02, 35, 60, term.cap=term.cap)

  str03 <-sprintf("hello %sworld", rgb.und)

  substr_ctl(str03, 1, 12, term.cap=term.cap)

  str04 <- sprintf("hello%s%s world%s%s yowza", red, inv, grn.bg, rgb.und)

  substr_ctl(str04, 5, 7, term.cap=term.cap)
  substr_ctl(str04, 5, 13, term.cap=term.cap)
})
unitizer_sect("Multi-line", {
  str.m.0 <- paste0(
    "\033[44m",
    c("hello world", rep("goodbye \033[45mmoon", 2), "yowza bombastic"),
    "\033[m"
  )
  substr_ctl(str.m.0, (1:4) * 2, (3:8) * 2)
})
unitizer_sect("tabs", {
  substr2_ctl("yo\tworld", 1, 8, tabs.as.spaces=TRUE)
})
unitizer_sect("Corner cases", {
  substr_ctl("hello", 0, -1)
  substr_ctl("hello", 0, 0)
  substr_ctl(rep("hello", 2), c(1, 0), c(1, 1))

  substr_ctl(character(), 1, 1)
  substr_ctl(list("hello", list("goodbye", "there")), 1, 2)
  substr_ctl(structure(list(list("goodbye", "there")), class="foo"), 1, 2)

  str.0 <- "\033[31mred\033[m"
  str.1 <- "\033[31mred\033[42m"
  str.2 <- c(str.0, str.1)

  substr_ctl(str.2, 0, 0)
  substr_ctl(str.2, 1, 1)
  substr_ctl(str.2, 3, 3)
  substr_ctl(str.2, 4, 4)

  substr_ctl(str.2, 3, 4)
  substr_ctl(str.2, 3, 5)

  substr_ctl(str.2, 3, 4, terminate=FALSE)
  substr_ctl(str.2, 3, 5, terminate=FALSE)

  substr_ctl(str.2, -1, 2)
  substr_ctl(str.2, -2, -1)
  substr_ctl(str.2, 4, 1)
  substr_ctl(str.2, 4, 1, terminate=FALSE)
  substr_ctl(str.2, 4, 1, carry="\033[44m")
  substr_ctl(str.2, 4, 1, carry="\033[44m", terminate=FALSE)

  substr_ctl("hello", 5, 5)
  substr_ctl("hello", 6, 6)
  substr_ctl("hello", 7, 6)
  substr_ctl("hello", 6, 7)
  substr_ctl("hello", 7, 5)

  substr_ctl("hello", 0, 6)
  substr_ctl("hello", 0, 5)
  substr_ctl("hello", 1, 6)

  substr_ctl("hello", "1", 1)
  substr_ctl("hello", 1, "1")

  substr_ctl("hello", "a", "b")

  substr_ctl("hello", 1, NA_integer_)
  substr_ctl("hello", NA_integer_, 1)

  # Nested
  substr_ctl(rep("\033[31mhello\033[m", 3), c(3,2,1), c(3,4,5))

  # Preserve attributes
  str.3 <- structure("fu\033[42mba\033[0mr", class="foo", at="bar")
  substr_ctl(str.3, 2, 3)

  # Turn off sgr
  substr_ctl(str.2, 2, 6, ctl=c('all', 'sgr'))
  substr_ctl(str.2, 8, 10, ctl=c('all', 'sgr'))

  # Make sure things stay in order
  substr2_ctl(rep("o\033[31m ", 2), 1:2, 1:2)

  # bad sequence at beginning or end
  substr_ctl("hello\033[41b", 1, 5)
  substr_ctl("hello\033[41b", 1, 6)
  substr_ctl("\033[1p\033[31mA", 1, 1)
  substr_ctl("\033[1p\033[31mA", 0, 1)
  substr_ctl("\033[1p\033[31mA", -1, 1)

  # Good/bad sequence at beginning
  substr_ctl("\033[31m\033[1pA", 1, 1)
  substr_ctl("\033[31m\033[1pA", 0, 1)
  substr_ctl("\033[31m\033[1pA", -1, 1)

  # Good good (test re-emission)
  substr_ctl("\033[41m\033[1mA", 1, 1)
  substr_ctl("\033[41m\033[1mA", 0, 1)
  substr_ctl("\033[41m\033[1mA", -1, 1)

  # Re-issue when state change out of substring
  str.4 <- c("A\033[45mB", "A")
  substr_ctl(str.4, 1, 1, carry=TRUE, terminate=FALSE)

  # Incomplete sequences
  substr_ctl("a\033[42", 1, 1)
  substr_ctl("a\033[42", 1, 2)
  substr_ctl("a\033[42", 1, 2, terminate=FALSE)
  substr_ctl("a\033]8;;END", 1, 1)
  # Incomplete, but we know it's a URL, so we remove it even if past end
  substr_ctl("a\033]8;;END", 1, 2)
  # But leave it if not terminating
  substr_ctl("a\033]8;;END", 1, 2, terminate=FALSE)
  substr_ctl("a\033];;END", 1, 1)
  substr_ctl("a\033];;END", 1, 2)
  substr_ctl("a\033[38;5mb", 1, 2, term.cap="all")
  substr_ctl("a\033[38;2mb", 1, 2, term.cap="all")
  substr_ctl("a\033[38;2;255mb", 1, 2, term.cap="all")
  substr_ctl("a\033[38;2;255;255mb", 1, 2, term.cap="all")

  # Select leading controls
  substr_ctl("\033[45pA", 1, 1, warn=FALSE)
  substr_ctl("\033[45pA", 0, 1, warn=FALSE)

  # NA handling
  substr_ctl(c("AB", NA, "CD"), 1, 2)
  substr_ctl(c("AB", NA, "CD"), 1, 2, carry=TRUE)
  substr_ctl(c("AB", "CD"), c(NA, 1), 2)
  substr_ctl(c("AB", "CD"), c(NA, 1), 2, carry=TRUE)

  # Old vs new term.cap behavior (almost certainly captured already, but adding
  # specific tests).
  substr_ctl("\033[38;5;4mA", 1, 1, term.cap="bright")
  substr_ctl("\033[38;5;4mA", 1, 1, term.cap=c("bright", "old"))
  substr_ctl("\033[38;5;4mA", 1, 1, term.cap=c("all", "256"))
  substr_ctl("\033[38;5;4mA", 1, 1, term.cap=c("all", "256", "old"))
  substr_ctl("\033[38;5;4mA", 1, 1, term.cap=c("256"))
  substr_ctl("\033[38;5;4mA", 1, 1, term.cap=c("256", "old"))
  substr_ctl("\033[38;2;1;1;1mA", 1, 1, term.cap="bright")
  substr_ctl("\033[38;2;1;1;1mA", 1, 1, term.cap=c("bright", "old"))
  substr_ctl("\033[38;2;1;1;1mA", 1, 1, term.cap=c("all", "truecolor"))
  substr_ctl("\033[38;2;1;1;1mA", 1, 1, term.cap=c("all", "truecolor", "old"))
  substr_ctl("\033[38;2;1;1;1mA", 1, 1, term.cap=c("truecolor"))
  substr_ctl("\033[38;2;1;1;1mA", 1, 1, term.cap=c("truecolor", "old"))
  substr_ctl("\033[107mA", 1, 1, term.cap="256")
  substr_ctl("\033[107mA", 1, 1, term.cap=c("256", "old"))
  substr_ctl("\033[107mA", 1, 1, term.cap=c("all", "bright"))
  substr_ctl("\033[107mA", 1, 1, term.cap=c("all", "bright", "old"))
  substr_ctl("\033[107mA", 1, 1, term.cap=c("bright"))
  substr_ctl("\033[107mA", 1, 1, term.cap=c("bright", "old"))

  # Detect changes in last truecolor byte
  str.5 <- c("\033[48;2;100;100;100mAB", "\033[48;2;100;100;100mCD")
  substr_ctl(str.5, 2, 2, terminate=FALSE, carry=TRUE, term.cap="all")
  str.5a <- c("\033[48;2;100;100;100mAB", "\033[48;2;100;100;101mCD")
  substr_ctl(str.5a, 2, 2, terminate=FALSE, carry=TRUE, term.cap="all")
})
unitizer_sect("Obscure escapes", {
  # illegal 38/48
  tryCatch(
    substr_ctl("\033[38;6;31mworld\033[m", 2, 3),
    warning=conditionMessage
  )
  suppressWarnings(substr_ctl("\033[38;6;31mworld\033[m", 2, 3))

  # illegal colors leave prior color unchanged

  tryCatch(
    substr_ctl("\033[31mhello\033[38;5;256m world\033[m", 7, 8),
    warning=conditionMessage
  )
  suppressWarnings(substr_ctl("\033[31mhello\033[38;5;256m world\033[m", 7, 8))

  # fraktur and double underline and prop spacing, and other odd ones

  substr_ctl("\033[20mworld\033[m", 2, 3)
  substr_ctl("\033[21mworld\033[m", 2, 3)
  substr_ctl(rep("\033[26mhello \033[50mworld\033[m", 2), c(2, 8), c(3, 10))
  substr_ctl(rep("\033[61mwor\033[65mld\033[m", 2), c(2, 4), c(3, 5))

  # unknown tokens

  tryCatch(
    substr_ctl("\033[56mworld\033[m", 2, 3),
    warning=conditionMessage
  )
  suppressWarnings(substr_ctl("\033[56mworld\033[m", 2, 3))
  tryCatch(
    substr_ctl("\033[66mworld\033[m", 2, 3),
    warning=conditionMessage
  )
  tryCatch(
    substr_ctl("\033[200mworld\033[m", 2, 3),
    warning=conditionMessage
  )
  # bright colors

  substr_ctl(rep("\033[91mwor\033[101mld\033[m", 2), c(2, 4), c(3, 5))
})
unitizer_sect('bad args', {
  # bad args
  hello2.0 <- "\033[42m\thello world\033[m foobar"
  substr2_ctl(hello2.0, 1, 2, warn=NULL)

  substr2_ctl(hello2.0, 1, 2, tabs.as.spaces=1)
  substr2_ctl(hello2.0, 1, 2, tabs.as.spaces=NA)
  substr2_ctl(hello2.0, 1, 2, tab.stops=-(1:3))
  substr2_ctl(hello2.0, 1, 2, tab.stops=0)
  substr2_ctl(hello2.0, 1, 2, round='bananas')
  substr2_ctl(hello2.0, 1, 2, term.cap=0)
  substr2_ctl(hello2.0, 1, 2, term.cap='bananas')
  substr2_ctl(hello2.0, 1, 2, type='bananas')

  substr2_ctl(hello2.0, 1, 2, ctl='bananas')
  substr2_ctl(hello2.0, 1, 2, ctl=0)
})
unitizer_sect('`ctl` related issues', {
  # Make sure SGR end properly detected
  substr_sgr("\033[31;42mhello world", 2, 4)

  # Repeated SGR

  substr_sgr("\033[31m\033[42mhello world", 2, 4)

  # Intermediate byte (this is not an SGR!); tryCatch due to inconsistency
  # on whether call is included in condition message

  tryCatch(
    substr_sgr("\033[31;42!mhello world", 2, 4),
    warning=function(x) conditionMessage(x)
  )
  # non-SGR CSI mixed with SGR when not parsing non-SGR CSI

  substr_sgr("\033[55;38l\033[31mhello world", 2, 4, warn=FALSE)
  substr_sgr("\033[31m\033[55;38lhello world", 2, 4, warn=FALSE)
  substr_sgr("hello \033[31m\033[55;38lworld", 7, 9, warn=FALSE)

  # Mix of escapes

  substr_ctl("\033[55;38l\033[31mhello world", 2, 4, warn=FALSE)
  substr_ctl("\033[31m\033[55;38lhello world", 2, 4, warn=FALSE)
  substr_ctl("hello \033[31m\033[55;38lworld", 7, 9, warn=FALSE)
  substr_ctl("hello\033[55;38l \033[31mworld", 4, 7, warn=FALSE)

  # C0 / nl

  substr_sgr("ab\n\tcd\n", 3, 6, warn=FALSE)
  substr_sgr("ab\n\033[31m\tcd\n", 3, 6, warn=FALSE)
  substr_ctl("ab\n\033[31m\tcd\n", 3, 6, warn=FALSE, ctl=c('all', 'nl'))
  substr_ctl("ab\n\033[31m\tcd\n", 3, 6, warn=FALSE, ctl=c('all', 'nl', 'c0'))

  # Index reporting

  substr_sgr(c("\a", "b", "c"), 1, 1)
  substr_sgr(c("a", "\b", "c"), 1, 1)
  substr_sgr(c("a", "b", "\ac"), 1, 1)
})
unitizer_sect("Rep Funs - Equivalence", {
  txt0 <- "ABCD"
  ## Basic equivalence
  identical(`substr_ctl<-`(txt0, 2, 2, value="#"), `substr<-`(txt0, 2, 2, "#"))
  identical(`substr_ctl<-`(txt0, 2, 2, value="#?"), `substr<-`(txt0, 2, 2, "#?"))
  identical(`substr_ctl<-`(txt0, 2, 3, value="#?-"), `substr<-`(txt0, 2, 3, "#?-"))

  identical(`substr_ctl<-`(txt0, 0, 0, value="#"), `substr<-`(txt0, 0, 0, "#"))
  identical(`substr_ctl<-`(txt0, 2, 1, value="#"), `substr<-`(txt0, 2, 1, "#"))
  identical(`substr_ctl<-`(txt0, 10, 12, value="#"), `substr<-`(txt0, 10, 12, "#"))
  identical(`substr_ctl<-`(txt0, 2, 3, value="#"), `substr<-`(txt0, 2, 3, "#"))

  identical(`substr_ctl<-`(txt0, 1, 5, value="#"), `substr<-`(txt0, 1, 5, "#"))
  identical(`substr_ctl<-`(txt0, 0, 5, value="#"), `substr<-`(txt0, 0, 5, "#"))

  ## Bug in R means we can't use identical
  `substr_ctl<-`(txt0, 0, -1, value="#")

  ## Recycling
  rep1 <- c("_", "_.")
  rep2 <- c("_", "_.", "...")

  identical(`substr_ctl<-`(txt0, 2, 3, value=rep1), `substr<-`(txt0, 2, 3, rep1))
  identical(`substr_ctl<-`(txt0, 2, 3, value=rep2), `substr<-`(txt0, 2, 3, rep2))

  txt1 <- c("AB", "CDE")
  identical(`substr_ctl<-`(txt1, 2, 3, value='_'), `substr<-`(txt1, 2, 3, '_'))
  identical(`substr_ctl<-`(txt1, 2, 3, value=rep1), `substr<-`(txt1, 2, 3, rep1))
  identical(`substr_ctl<-`(txt1, 2, 3, value=rep2), `substr<-`(txt1, 2, 3, rep2))

  txt2 <- c("AB", "CDE", "EFGH")
  identical(`substr_ctl<-`(txt2, 2, 3, value='_'), `substr<-`(txt2, 2, 3, '_'))
  identical(`substr_ctl<-`(txt2, 2, 3, value=rep1), `substr<-`(txt2, 2, 3, rep1))
  identical(`substr_ctl<-`(txt2, 2, 3, value=rep2), `substr<-`(txt2, 2, 3, rep2))

  txt3a <- txt3b <- c("ABC", "ABC")
  substr(txt3a[2], 2, 2) <- "_"
  substr_ctl(txt3b[2], 2, 2) <- "_"
  identical(txt3a, txt3b)

  ## NA handling
  identical(
    `substr_ctl<-`(txt0, 2, 3, value=NA_character_),
    `substr<-`(txt0, 2, 3, NA_character_)
  )
  txt.na <- NA_character_
  identical(`substr_ctl<-`(txt.na, 1, 2, value="AB"),`substr<-`(txt.na, 1, 2,  "AB"))
})

unitizer_sect("Rep Funs - SGR", {
  txt1 <- "\033[33mABCD"
  txt2 <- "\033[33mA\033[44mBCD"
  txt3 <- "\033[33mA\033[44mBC\033[1mD"

  `substr_ctl<-`(txt1, 2, 2, value="#")
  `substr_ctl<-`(txt1, 2, 3, value="#?-")
  `substr_ctl<-`(txt1, 2, 3, value="#\033[32m?-")
  `substr_ctl<-`(txt1, 2, 3, value="#\033[32m?-\033[0m")
  `substr_ctl<-`(txt1, 2, 3, value="#\033[0m?-")

  `substr_ctl<-`(txt2, 2, 3, value="#\033[32m?-")
  `substr_ctl<-`(txt2, 2, 3, value="#\033[32m?-\033[0m")
  `substr_ctl<-`(txt2, 2, 3, value="#\033[0m?-")

  `substr_ctl<-`(txt3, 2, 3, value="#\033[32m?-")
  `substr_ctl<-`(txt3, 2, 3, value="#\033[32m?-\033[0m")
  `substr_ctl<-`(txt3, 2, 3, value="#\033[0m?-")

  ## Terminate
  `substr_ctl<-`(txt2, 2, 2, terminate=FALSE, value="#")
  `substr_ctl<-`(txt2, 2, 3, terminate=FALSE, value="#\033[32m?-")
  `substr_ctl<-`(txt2, 2, 3, terminate=FALSE, value="#\033[32m?-\033[0m")
  `substr_ctl<-`(txt2, 2, 3, terminate=FALSE, value="#\033[0m?-")
  `substr_ctl<-`(txt1, 2, 3, terminate=FALSE, value="#\033[0m?\033[45m-")
  `substr_ctl<-`(txt1, 2, 3, terminate=FALSE, value="#\033[0m\033[45m?-")

  txt4 <- c(txt2, txt0, "\033[39mABCD")
  ## Different lengths
  `substr_ctl<-`(txt4, 2, 3, value="#")
  `substr_ctl<-`(txt4, 2, 3, value=c("#", "?"))
  `substr_ctl<-`(txt4, 2, 3, value=c("#", "?", "$"))

  ## Lengths + Carry; note sequences in middle of `value` boundary are treated
  ## differently than on the ends.
  `substr_ctl<-`(txt4, 2, 2, carry=TRUE, value="#")
  `substr_ctl<-`(txt4, 2, 3, carry=TRUE, value="#\033[32m?-")
  `substr_ctl<-`(txt4, 2, 3, carry=TRUE, value="#\033[42m?-\033[0m")
  `substr_ctl<-`(txt4, 2, 3, carry=TRUE, value="#\033[0m?-")
  ## Weirdness here because the 39 in `value` causes re-issue of 45.  This is
  ## correct; a consequence of the mess of termintate=FALSE in replace mode.
  rep4 <- c("\033[32m_\033[45m", ".-", "\033[39m__")
  `substr_ctl<-`(txt4, 2, 3, carry=TRUE, value=rep4)

  ## Lengths + Terminate + Carry
  `substr_ctl<-`(txt4, 2, 2, terminate=FALSE, carry=TRUE, value="#")
  `substr_ctl<-`(txt4, 2, 3, terminate=FALSE, carry=TRUE, value="#\033[32m?-")
  `substr_ctl<-`(txt4, 2, 3, terminate=FALSE, carry=TRUE, value="#\033[35m?-\033[0m")
  `substr_ctl<-`(txt4, 2, 3, terminate=FALSE, carry=TRUE, value="#\033[0m?-")
  `substr_ctl<-`(txt4, 2, 3, terminate=FALSE, carry=TRUE, value=rep4)

  ## Reference for bridge against end of prior `value` substring
  txt5 <- c("ABD", "DFG")
  `substr_ctl<-`(txt5, 2, 2, value=".\033[45m", carry=TRUE, terminate=FALSE)

  ## Tabs
  txt6 <- "A123456789B"
  `substr2_ctl<-`(txt6, 2, 9, value="\t", tabs.as.spaces=TRUE)
  `substr2_ctl<-`(txt6, 2, 3, value="\t", tabs.as.spaces=TRUE)
  `substr2_ctl<-`(txt6, 2, 10, value="\t", tabs.as.spaces=TRUE)

  ## Encodings
  txt7a <- "\u0160os"
  txt7b <- "sos"
  txt7c <- "so\u0160"
  val.scar <- "\u0161"
  Encoding(`substr_ctl<-`(txt7a, 1, 1, value=val.scar))
  Encoding(`substr_ctl<-`(txt7a, 1, 1, value="s"))
  Encoding(`substr_ctl<-`(txt7a, 2, 2, value=val.scar))
  Encoding(`substr_ctl<-`(txt7a, 2, 2, value="s"))
  Encoding(`substr_ctl<-`(txt7b, 2, 2, value=val.scar))
  Encoding(`substr_ctl<-`(txt7b, 2, 2, value="s"))
  Encoding(`substr_ctl<-`(txt7c, 3, 3, value=val.scar))
  Encoding(`substr_ctl<-`(txt7c, 3, 3, value="s"))

})
unitizer_sect("Rep Funs - Corner Cases", {
  ## Include trail when selecting past end of `value`
  `substr_ctl<-`(txt2, 1, 3, terminate=FALSE, value="#\033[32m?\033[0m")

  ## Only portions of string that are replaced are modified; leading and
  ## trailing controls remain, possibly causing redundant sequences when the
  ## lead and trail sequences are zero width, particularly with terminate=T.
  txt8 <- "\033[32mAB\033[45m"
  `substr_ctl<-`(txt8, 1, 2, value="12")
  `substr_ctl<-`(txt8, 1, 2, value="12", terminate=FALSE)
  `substr_ctl<-`(txt8, 1, 3, value="12")
  `substr_ctl<-`(txt8, 1, 3, value="1")
  `substr_ctl<-`(txt8, 1, 3, value="")
  `substr_ctl<-`(txt8, 1, 3, value="123")
  `substr_ctl<-`(txt8, 0, 2, value="12")
  `substr_ctl<-`(txt8, 0, 3, value="12")

  ## Zero width gets inserted
  `substr_ctl<-`(txt8, 1, 3, value="\033[1m", terminate=FALSE)

  ## Errors
  tce(`substr_ctl<-`(txt8, 1, 3, value="A", carry="\033[41m"))
  lat <- "fa\xe7ile"
  Encoding(lat) <- "latin1"
  tce(`substr_ctl<-`(lat, 1, 3, value="ABC"))

  ## NA handling
  txt.na2 <- c("AB", NA, "BC")
  `substr_ctl<-`(txt.na2, 1, 1, value="#")
  txt.nona <- c("AB", "BC", "CD")
  `substr_ctl<-`(txt.nona, 1, 1, value=c("#", NA), carry=TRUE)
})

