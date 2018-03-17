library(unitizer)
library(fansi)

unitizer_sect("term_cap_test", {
  tct <- term_cap_test()
  tct
  fansi_lines(LETTERS, step=6)
})
unitizer_sect("digits", {
  ints <- c(-100L, -9999L, -1L, 0L, 1L, 9L, 10L, 99L, 100L, 101L, 9999L)
  cbind(
    ints,
    fansi:::digits_in_int(ints)
  )
})
unitizer_sect("add_int", {
  fansi:::add_int(1, 1)
  fansi:::add_int(2^31 - 1, 1)
  fansi:::add_int(2^31 - 1, 0)
  fansi:::add_int(-2^31 + 1, 0)
  fansi:::add_int(-2^31 + 1, -1)
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

  utf8.bad.0 <- "hello\033\033\033[45p \xF0how wor\ald"
  Encoding(utf8.bad.0) <- "UTF-8"
  unhandled_ctl(utf8.bad.0)
  utf8.bad.1 <- "hello \xF0ho"
  Encoding(utf8.bad.1) <- "UTF-8"
  unhandled_ctl(utf8.bad.1)

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
unitizer_sect("utf8clen", {
  # Can't test directly, but we can check what character lenght we get back from
  # nchar and infer whether things have changed or not
  #
  # These tests are designed to start failing if behavior of utf8clen changes
  #
  # See src/main/valid_utf8.h

  # U+0000..U+007F     | 00..7F |
  # U+0080..U+07FF     | C2..DF | 80..BF
  # U+0800..U+0FFF     | E0     |*A0..BF*| 80..BF
  # U+1000..U+CFFF     | E1..EC | 80..BF | 80..BF
  # U+D000..U+D7FF     | ED     |*80..9F*| 80..BF
  # U+E000..U+FFFF     | EE..EF | 80..BF | 80..BF
  # U+10000..U+3FFFF   | F0     |*90..BF*| 80..BF | 80..BF
  # U+40000..U+FFFFF   | F1..F3 | 80..BF | 80..BF | 80..BF
  # U+100000..U+10FFFF | F4     |*80..8F*| 80..BF | 80..BF

  chrs <- c(
    "\xc2\x80", "\xDF\xBF",
    "\xe0\xA0\x80", "\xE0\xBF\xBF",
    "\xe1\x80\x80", "\xeC\xbf\xbf",
    "\xed\x80\x80", "\xed\x9f\xbf",
    "\xee\x90\x80", "\xef\xbf\xbf",
    "\xf0\x90\x80\x80", "\xf4\x8f\xbf\xbf",
    "\xf8\x80\x80\x80\x80", "\xfb\x80\x80\x80\x80",
    "\xfc\x80\x80\x80\x80\x80", "\xff\x80\x80\x80\x80\x80"
  )
  Encoding(chrs) <- "UTF-8"
  nchar(chrs, allowNA=TRUE)
  nchar_ctl(chrs, allowNA=TRUE)

  # Of the 10xxxxxx variety

  utf8.bad.2 <- "\xBFaaaaaa"
  Encoding(utf8.bad.2) <- "UTF-8"
  nchar(utf8.bad.2, allowNA=TRUE)
  nchar_ctl(utf8.bad.2, allowNA=TRUE)

  substr(utf8.bad.2, 1, 1)
  substr_ctl(utf8.bad.2, 1, 1)
})
unitizer_sect("C funs", {
  fansi:::cleave(1:10)
  fansi:::cleave(1:9)
  fansi:::cleave(1:10 + .1)

  # sort_chr doesn't guarantee that things will be sorted lexically, just that
  # alike things will be contiguous

  set.seed(42)
  jumbled <- as.character(rep(1:10, 10))[sample(1:100)]
  sorted <- fansi:::sort_chr(jumbled)

  which(as.logical(diff(as.numeric(sorted))))
})
