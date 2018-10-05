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

  # Specifying term cap

  unhandled_ctl("\033[38;5;220mworld\033[m", "bright")
  unhandled_ctl("\033[38;2;10;20;30mworld\033[m", "bright")
  unhandled_ctl("\033[38;2;10;20;30mworld\033[m", "bri")
  unhandled_ctl("\033[38;2;10;20;30mworld\033[m", NULL)
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

  strtrim_sgr("\033[42m\the\allo world\033[m foobar", 12, warn=FALSE)
  strtrim2_sgr(
    "\033[42m\the\allo world\033[m foobar", 12, tabs.as.spaces=TRUE,
    warn=FALSE, tab.stops=2
  )
  # bad args

  hello2.0 <- "\033[42m\thello world\033[m foobar"
  strtrim_ctl(1:3, width=10)
  strtrim_ctl(hello2.0, width="35")
  strtrim_ctl(hello2.0, width=NA_integer_)
  strtrim_ctl(hello2.0, width=10, warn=NULL)
  strtrim_ctl(hello2.0, width=10, ctl=0)
  strtrim_ctl(hello2.0, width=10, ctl='bananas')

  strtrim2_ctl(1:3, width=10)
  strtrim2_ctl(hello2.0, width="35")
  strtrim2_ctl(hello2.0, width=NA_integer_)
  strtrim2_ctl(hello2.0, width=10, warn=NULL)

  strtrim2_ctl(hello2.0, width=10, tabs.as.spaces=NA)
  strtrim2_ctl(hello2.0, width=10, tabs.as.spaces=1:3)
  strtrim2_ctl(hello2.0, width=10, tab.stops=-(1:3))
  strtrim2_ctl(hello2.0, width=10, tab.stops=0)

  strtrim2_ctl(hello2.0, width=10, ctl=0)
  strtrim2_ctl(hello2.0, width=10, ctl='bananas')
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
unitizer_sect("enc check", {
  x <- y <- "He\x9f"
  Encoding(x) <- "latin1"
  fansi:::check_enc(x, 1)

  Encoding(y) <- "bytes"
  fansi:::check_enc(y, 1)

  fansi:::check_enc("hello", 1)
})
unitizer_sect("what as int", {
  fansi:::ctl_as_int(c(1, 2, 3, 4, 5))
  fansi:::ctl_as_int(c(2, 3, 4, 5))
  fansi:::ctl_as_int(c(1, 2, 3, 7))
  fansi:::ctl_as_int(c(2, 3, 7))
})
unitizer_sect("HTML helper", {
  html_esc(character())
  html_esc(1:10)
  html_esc(NA_character_)
  html_esc("<he&llo>")
  html_esc("ow&wo")
  html_esc(c("hello", "wor<ld>s", NA, ""))
  html_esc("<<<<")

  txt <- c(
    "day > night",
    "hello world"
  )
  html_code_block(character())
  html_code_block(txt)
  html_code_block(1:10)
  html_code_block(txt, class=c('not-fansi', 'plain'))
  html_code_block(txt, class=NULL)
})
unitizer_sect("hooks", {
  h.1 <- list(
    set=function(...) cat("Set hooks: ", names(list(...)), "\n"),
    get=function(...) function(...) "old.hook"
  )
  h.2 <- list(
    set=function(...) cat("Set hooks: ", names(list(...)), "\n"),
    get=function(...) "not a function"
  )
  h.3 <- list(
    set=function(...) cat("Set hooks: ", names(list(...)), "\n"),
    get=function(...) stop("error in get")
  )
  h.4 <- list(
    set=function(...) stop("error in set"),
    get=function(...) function() "old.hook"
  )
  ## Works

  set_knit_hooks(list(1, 2))
  set_knit_hooks(list(function() NULL, function() NULL))
  res1 <- set_knit_hooks(h.1, .test=TRUE)
  res1[['new.hooks']][['output']]("hello")
  res1[['new.hooks']][['output']]("hello\033[31m world")

  p.f.2 <- function(x, y) NULL
  p.f.3 <- function(x, class) sprintf("new proc fun, '%s'", class)
  p.f.4 <- function(x, class) stop("new proc fun")

  ## bad inputs

  set_knit_hooks(h.1, proc.fun=p.f.2)
  set_knit_hooks(h.1, which="hello")
  set_knit_hooks(h.1, which=NULL)

  ## works

  res2 <- set_knit_hooks(
    h.1, which=c('output', 'message'), class=c('f-output', 'f-message'),
    proc.fun=p.f.3, .test=TRUE
  )

  res2[['new.hooks']][['message']]("hello")
  res2[['new.hooks']][['message']]("hello\033[31m world")
  res2[['new.hooks']][['output']]("hello\033[31m world")

  ## error in proc.fun

  res3 <- set_knit_hooks(
    h.1, which=c('message', 'warning'), proc.fun=p.f.4, .test=TRUE
  )
  res3[['new.hooks']][['warning']]("hello")
  res3[['new.hooks']][['warning']]("hello\033[31mworld")

  ## hook errors

  set_knit_hooks(h.2)
  set_knit_hooks(h.3)
  set_knit_hooks(h.4)

  ## Other errors

  set_knit_hooks(h.1, style=NULL)
  set_knit_hooks(h.1, class=1:10)
  set_knit_hooks(h.1, class=letters)
  set_knit_hooks(h.1, which=c('output', 'message', 'output'))
})
unitizer_sect("fansi lines", {
  fansi_lines(1:3)
  fansi_lines(1:3, step='hello')
})
