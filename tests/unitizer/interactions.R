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

unitizer_sect("substr", {
  str.0 <- c("\033[44mhello", "world")
  substr_ctl(str.0, 2, 4)
  substr_ctl(str.0, 2, 4, carry=TRUE)
  substr_ctl(str.0, 2, 4, carry="\033[33m")

  substr2_ctl(str.0, 2, 4, carry="\033[33m")
  substr_sgr(str.0, 2, 4, carry="\033[33m")
  substr2_sgr(str.0, 2, 4, carry="\033[33m")

  str.1 <- c("hello", "\033[44mworld", "barrow")
  substr_ctl(str.1, 2, 4)
  substr_ctl(str.1, 2, 4, carry=TRUE)
  substr_ctl(str.1, 2, 4, carry="\033[33m")

  str.2 <- c("\033[33mA\033[44mBCD", "ABCD", "\033[39mABCD")
  substr_ctl(str.2, 2, 2)
  substr_ctl(str.2, 2, 2, carry=TRUE)
  substr_ctl(str.2, 2, 2, carry=TRUE, terminate=FALSE)

  ## End background should be kept
  str.3 <- c("\033[35mA\033[42mB", "\033[49mCD")
  substr_ctl(str.3, 2, 2, carry=TRUE, terminate=FALSE)
})
wrp.0 <- c(
  "once upon \033[44ma time in a land far away over ",
  "the mountains and \033[7m sea lived a fair creature ",
  "with \033[4mdark itentions and a yappy dog."
)
unitizer_sect("wrap/trim", {
  strwrap_ctl(wrp.0, 20)
  strwrap_ctl(wrp.0, 20, carry=TRUE)
  strwrap_ctl(wrp.0, 20, carry="\033[33m")

  strwrap_sgr(wrp.0, 20, carry="\033[33m")
  strwrap2_ctl(wrp.0, 20, carry="\033[33m")
  strwrap2_sgr(wrp.0, 20, carry="\033[33m")

  strtrim_ctl(wrp.0, 20, carry="\033[33m")
  strtrim_sgr(wrp.0, 20, carry="\033[33m")
  strtrim2_ctl(wrp.0, 20, carry="\033[33m")
  strtrim2_sgr(wrp.0, 20, carry="\033[33m")

  wrp.1 <- c(
    "once upon \033[44ma time in a land far away over ",
    "the mountains and \033[7m sea lived a \033[32mfair creature ",
    "with \033[4mdark itentions and a yappy dog."
  )
  strtrim_ctl(wrp.0, 20, carry="\033[33m")

  wrp.2 <- c("hello \033[42mworld", "goodnight\033[49m moon", "oh \033[39mboy")
  strwrap_ctl(wrp.2, 10, carry="\033[35m", simplify=FALSE)
  strwrap_ctl(wrp.2, 10, carry="\033[35m", simplify=FALSE, terminate=FALSE)
})
unitizer_sect("normalize", {
  str.2 <- c("\033[44mhello", "wo\033[mrld", "barrow")
  normalize_state(str.2)
  normalize_state(str.2, carry=TRUE)
  # unlike substr/wrap normalize does not add the color from carry,
  # it just accounts for its presence from prior strings in e.g. computing
  # the close string.
  normalize_state(str.2, carry="\033[33m")
})
unitizer_sect("carry corner cases", {
  substr_ctl("", 2, 3, carry="\033[33m")
  # Empty, because carry is presumed to exist open previously.
  substr_ctl("", 2, 3, carry="\033[33m", terminate=FALSE)
  # This requires a close
  substr_ctl("\033[39m", 2, 3, carry="\033[33m", terminate=FALSE)
  # This requires a close
  substr_ctl("\033[39m", 2, 3, carry="\033[33m", terminate=FALSE, normalize=TRUE)

  substr_ctl("", 0, 1, carry="\033[33m")
  substr_ctl("", 0, 1, carry="\033[33m", terminate=FALSE)
  # Should close because we do request one character
  substr_ctl("\033[39m", 0, 1, carry="\033[33m", terminate=FALSE)
  substr_ctl("\033[39m", 0, 1, carry="\033[33m", terminate=FALSE, normalize=TRUE)
  substr_ctl("", 0, 0, carry="\033[33m")
  substr_ctl("", 0, 0, carry="\033[33m", terminate=FALSE)
  # No close because we read nothing at all
  substr_ctl("\033[39m", 0, 0, carry="\033[33m", terminate=FALSE)

  substr_ctl(character(), 2, 4, carry="\033[33m")
  substr_ctl(NA, 2, 4, carry="\033[33m")
  substr_ctl(environment(), 2, 4, carry="\033[33m")
  substr_ctl("hello", 2, 4, carry=c("\033[33m", "\033[44m"))

  substr_ctl(str.0, 2, 4, carry=NA_character_)
  substr_ctl(str.0, 2, 4, carry=character())
  substr_ctl(str.0, 2, 4, carry=1)
  substr_ctl(str.0, 2, 4, carry=Inf)

  ## Carrying of other SGRs
  sgrs <- c(
    "A\033[31mB", "C\033[1mD", "E\033[4mF",
    "G\033[13mH", "I\033[62mJ", "K\033[39mL",
    "M\033[52mN", "O\033[65mP", "Q\033[22mR",
    "S\033[24mT", "T\033[54mU", "V\033[10mW"
  )
  substr_ctl(sgrs, 2, 2, carry=TRUE)

  normalize_state(str.2, carry=NA_character_)
  normalize_state(str.2, carry=character())
  normalize_state(str.2, carry=1)
  normalize_state(str.2, carry=Inf)

  strwrap_ctl(wrp.0, 20, carry=NA_character_)
  strwrap_sgr(wrp.0, 20, carry=character())
  strwrap2_ctl(wrp.0, 20, carry=1)
  strwrap2_sgr(wrp.0, 20, carry=Inf)

  ## leading SGR consumed and merged with carry
  strwrap_ctl(c("\033[33mA \033[4mB", "\033[44mC D"), carry=TRUE, 2)
})
unitizer_sect("terminate", {
  str.0 <- c("hel\033[33m", "wo\033[44mrld")
  substr_ctl(str.0, 2, 5, terminate=FALSE)
  substr_sgr(str.0, 2, 5, terminate=FALSE)
  substr2_ctl(str.0, 2, 5, terminate=FALSE)
  substr2_sgr(str.0, 2, 5, terminate=FALSE)

  strwrap_ctl(wrp.0, 20, terminate=FALSE)
  strwrap_sgr(wrp.0, 20, terminate=FALSE)
  strwrap2_ctl(wrp.0, 20, terminate=FALSE)
  strwrap2_sgr(wrp.0, 20, terminate=FALSE)

  strtrim_ctl(wrp.0, 20, terminate=FALSE)
  strtrim_sgr(wrp.0, 20, terminate=FALSE)
  strtrim2_ctl(wrp.0, 20, terminate=FALSE)
  strtrim2_sgr(wrp.0, 20, terminate=FALSE)

  ## Error
  strtrim2_sgr(wrp.0, 20, terminate=NA)
})
unitizer_sect("bridge", {
  fansi:::bridge("\033[42m", "\033[31m")
  fansi:::bridge("\033[42m", "\033[31m", normalize=TRUE)
  fansi:::bridge("", "\033[31m")
  fansi:::bridge("\033[42m", "")
  fansi:::bridge("\033[42m", "\033[42m")
  end <- c("\033[31", "\033[41m", NA_character_, "\033[44m")
  restart <- c("", NA_character_, "\033[45m", "\033[45m")
  fansi:::bridge(end, restart)

  # this is unterminated URL
  base.st <- '%s\033]8;%s;%s\033\\'
  url <- "https://x.yz"
  u0 <- sprintf(base.st, "", "", url)

  fansi:::bridge(paste0("\033[42m", u0), "\033[31m")
  fansi:::bridge("\033[31m", paste0("\033[42m", u0))

  # in replace
  txt <- c("A\033[31mBC", "D\033[39mE\033[42mF")
  `substr_ctl<-`(txt, 2, 2, value="?", normalize=TRUE, carry=TRUE)
})
unitizer_sect("at end / close", {
  x <- c("a\033[31mb", "c", "\033[42md")
  state_at_end(x)
  state_at_end(x, carry=TRUE)
  state_at_end(x, carry=TRUE, normalize=TRUE)
  state_at_end("a\033[pb")
  state_at_end("a\033[pb", warn=FALSE)
  state_at_end(c("\033[42mA", NA_character_, "\033[31mA"))
  state_at_end(c("\033[42mA", NA_character_, "\033[31mA"), carry=TRUE)

  close_state(x)
  close_state(x, normalize=TRUE)
  close_state("a\033[pb")
  close_state("a\033[pb", warn=FALSE)

  # test warnings/errors without arg specified
  state_no_arg <- function(x) {
    fansi:::VAL_IN_ENV(x=x, warn=TRUE, term.cap="all", ctl="all", carry=TRUE)
    tcw(
      tce(
        .Call(
          fansi:::FANSI_state_at_end, x,
          WARN.INT, TERM.CAP.INT, CTL.INT,
          TRUE, carry,
          NA_character_, FALSE
    ) ) )
  }
  x <- "\xf0"
  Encoding(x) <- "UTF-8"
  state_no_arg(x)
  y <- "\033[45phello"
  state_no_arg(y)
})
