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
  substr_ctl("", 2, 4, carry="\033[33m")
  substr_ctl("", 2, 4, carry = "\033[33m", terminate=FALSE)
  substr_ctl(character(), 2, 4, carry="\033[33m")
  substr_ctl(NA, 2, 4, carry="\033[33m")
  substr_ctl(environment(), 2, 4, carry="\033[33m")
  substr_ctl("hello", 2, 4, carry=c("\033[33m", "\033[44m"))

  substr_ctl(str.0, 2, 4, carry=NA_character_)
  substr_ctl(str.0, 2, 4, carry=character())
  substr_ctl(str.0, 2, 4, carry=1)
  substr_ctl(str.0, 2, 4, carry=Inf)

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
})

unitizer_sect("bridge", {
  fansi:::bridge("\033[42m", "\033[31m")
  fansi:::bridge("\033[42m", "\033[31m", normalize=TRUE)
  fansi:::bridge("", "\033[31m")
  fansi:::bridge("\033[42m", "")
  fansi:::bridge("\033[42m", "\033[42m")

  # this is unterminated URL
  base.st <- '%s\033]8;%s;%s\033\\'
  url <- "https://x.yz"
  u0 <- sprintf(base.st, "", "", url)

  fansi:::bridge(paste0("\033[42m", u0), "\033[31m")
  fansi:::bridge("\033[31m", paste0("\033[42m", u0))
})
unitizer_sect("at end / close", {
  x <- c("a\033[31mb", "c", "\033[42md")
  state_at_end(x)
  state_at_end(x, carry=TRUE)
  state_at_end(x, carry=TRUE, normalize=TRUE)
  state_at_end("a\033[pb")
  state_at_end("a\033[pb", warn=FALSE)

  close_state(x)
  close_state(x, normalize=TRUE)
  close_state("a\033[pb")
  close_state("a\033[pb", warn=FALSE)
})
