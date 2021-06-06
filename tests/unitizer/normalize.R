library(fansi)

unitizer_sect("no expansion", {
  normalize_sgr("A\033[31mB")
  normalize_sgr("\033[31mAB")
  normalize_sgr("AB\033[31m")
  normalize_sgr(c("A\033[31mB", "AB"))
  normalize_sgr(c("\033[31mAB", "AB"))
  normalize_sgr(c("AB\033[31m", "AB"))
})
unitizer_sect("simple expansion", {
  normalize_sgr(c("A\033[31;42mB", "AB"))
  normalize_sgr(c("\033[31;42mAB", "AB"))
  normalize_sgr(c("AB\033[31;42m", "AB"))

  normalize_sgr(c("A\033[31;42mB", "A\033[39;4mB"))
  normalize_sgr(c("A\033[31;42mB", "\033[39;4mAB"))
  normalize_sgr(c("AB\033[31;42m", "A\033[39;4mB"))
  normalize_sgr(c("AB\033[31;42m", "A\033[0;4mB"))
  normalize_sgr(c("AB\033[31;42m", "AB\033[0;4m"))
  normalize_sgr(c("AB\033[31;42m", "\033[0;4mAB"))
})
unitizer_sect("superflous codes", {
  normalize_sgr(c("A\033[31;44;38;5;226;36mBC\033[mD"))
  normalize_sgr(c("A\033[31;44;38;5;226;36m\033[0mBCD"))
})
unitizer_sect("broad code test", {
  normalize_sgr(c("A\033[33;44mB\033[1;3;4mCD\033[mE"))
  normalize_sgr(
    "A\033[33;44mB\033[1;2;3;4;5;6;7;8;9;11;21;26;51;52;53;60;61;62;63;64mC\033[m"
  )
  ## Are we somehow producing bad SGR (we did once)
  unhandled_ctl(
    normalize_sgr(
      "A\033[33;44mB\033[1;2;3;4;5;6;7;8;9;11;21;26;51;52;53;60;61;62;63;64mC\033[m"
  ) )
  normalize_sgr(
    "A\033[33;44mB\033[1;2;3;4;5;6;7;8;9;11;21;26;51;52;53;60;61;62;63;64m\033[mC"
  )
  normalize_sgr(
    "A\033[33;44mB\033[1;2;3;4;5;6;7;8;9;11;21;26;51;52;53;60;61;62;63;64mC\033[mD"
  )
})
unitizer_sect("errors and warnings", {
  normalize_sgr(list(1, 2, 3))
  x <- c("A\033[38;2;100;150;3;36mBC\033[mD")
  normalize_sgr(x, term.cap=character())
  ## no warning
  normalize_sgr(x, term.cap=c('bright', '256', 'truecolor'))
})
unitizer_sect("in functions", {
  string1 <- "hello \033[33;44mblue world"
  string2 <- "\033[4;1mgoodbye\033[7m white \033[mmoon"
  string3 <- c(string1, string2)

  strwrap_ctl(string1, 11, normalize=TRUE)
  strwrap_ctl(string2, 11, normalize=TRUE)
  strwrap_ctl(string3, 11, normalize=TRUE)
  strwrap2_ctl(string3, 11, normalize=TRUE, pad.end=" ")

  strwrap_sgr(string3, 11, normalize=TRUE)
  strwrap2_sgr(string3, 11, normalize=TRUE, pad.end=" ")

  strtrim_sgr(string3, 8, normalize=TRUE);
  strtrim_sgr(string3, 8, normalize=TRUE);

  substr_ctl("\033[33;44mhello\033[m world", 3, 8, normalize=TRUE)
  substr2_ctl("\033[33;44mhello\033[m world", 3, 8, normalize=TRUE)
  substr_sgr("\033[33;44mhello\033[m world", 3, 8, normalize=TRUE)
  substr2_sgr("\033[33;44mhello\033[m world", 3, 8, normalize=TRUE)

  substr_ctl(string3, c(3, 3), c(8, 15), normalize=TRUE)

  strsplit_ctl(string3, " ", normalize=TRUE)
})

