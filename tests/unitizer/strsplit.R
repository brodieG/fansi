library(fansi)

unitizer_sect("basic splits", {
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
})
