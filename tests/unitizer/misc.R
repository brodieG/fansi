library(unitizer)
library(fansi)

unitizer_sect("digits", {
  ints <- c(-100L, -9999L, -1L, 0L, 1L, 9L, 10L, 99L, 100L, 101L, 9999L)
  cbind(
    ints,
    fansi:::digits_in_int(ints)
  )
})
unitizer_sect("unhandled", {
  # example
  string.0 <- c(
    "\033[41mhello world\033[m", "foo\033[22>m", "\033[999mbar",
    "baz \033[31#3m", "a\033[31k", "hello\033m world"
  )
  unhandled_ctl(string.0)
  # some more interesting cases
  string.1 <- c(
    "foo\033[22>mhello\033[9999m", "a\033[31k", "hello\033m world \033"
  )
  unhandled_ctl(string.1)

  # A malformed ESCape

  unhandled_ctl("hello\033\033\033[45p wor\ald")

  # a bad utf8 string and other bad stuff

  utf8.bad <- "hello\033\033\033[45p \xF0how wor\ald"
  unhandled_ctl(utf8.bad)

})
unitizer_sect("strtrim", {
  strtrim_ctl(" hello world", 7)
  strtrim_ctl("\033[42m hello world\033[m", 7)
  strtrim_ctl(" hello\nworld", 7)
  strtrim_ctl("\033[42m hello\nworld\033[m", 7)
  strtrim_ctl("\nhello\nworld", 7)
  strtrim_ctl("\033[42m\nhello\nworld\033[m", 7)
  strtrim_ctl("\thello\rworld foobar", 12)
  strtrim_ctl("\033[42m\thello\rworld\033[m foobar", 12)

  strtrim2_ctl("\033[42m\thello world\033[m foobar", 12, tabs.as.spaces=TRUE)
})
