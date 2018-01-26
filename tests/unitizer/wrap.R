library(unitizer)
library(fansi)

unitizer_sect("Basic wrap", {
  hello.0 <- "hello world this is a lovely day"

  identical(strwrap_csi(hello.0, width=10), strwrap(hello.0, width=10))

  hello.1 <- "hello  world  this  is.  a lovely day."
  identical(strwrap_csi(hello.1, width=10), strwrap(hello.1, width=10))

  writeLines(strwrap_csi(hello.1, width=10))
  writeLines(strwrap(hello.1, width=10))

  hello.2 <- "hello\rworld\rthis  is.  a lovely day."
  identical(strwrap(hello.2, width=10), strwrap_csi(hello.2, width=10))

  hello.3 <- "hello\rworld\nthis  is.  a lovely\n day."
  identical(strwrap(hello.3, width=10), strwrap_csi(hello.3, width=10))

  hello.4 <- "  hello  world  this  is  a lovely day."
  identical(strwrap(hello.4, width=10), strwrap_csi(hello.4, width=10))

  hello.5 <- "hello.\n\n\nworld"
  identical(strwrap(hello.5, width=10), strwrap_csi(hello.5, width=10))

  hello.5a <- "hello.\n \n \nworld"
  identical(strwrap(hello.5a, width=10), strwrap_csi(hello.5a, width=10))

  # special preserve of double space

  hello.6a <- 'hello."  there'
  identical(strwrap(hello.6a, width=40), strwrap_csi(hello.6a, width=40))

  hello.6b <- 'hello.\'  there'
  identical(strwrap(hello.6b, width=40), strwrap_csi(hello.6b, width=40))

  hello.6c <- 'hello.)  there'
  identical(strwrap(hello.6c, width=40), strwrap_csi(hello.6c, width=40))

})
unitizer_sect("Basic Ansi", {
  hello2.0 <-
    paste0("hello ", red, "world ", grn.bg, " this is a  lovely", end, "day.")
  strwrap_csi(hello2.0, 10)

  identical(
    strwrap_csi(strip_ansi(hello2.0), 10), strwrap(strip_ansi(hello2.0), 10)
  )
  # Specific turn off tags - turn off bold and faint

  hello.bold.faint <- paste0(
    "hello \033[1mbolded once upon a time\033[22m ",
    "normal \033[2mfainting in faintness oh no\033[22m normal"
  )
  strwrap_csi(hello.bold.faint, 10)

  # Specific turn off tags - blinking

  hello.blinky <- paste0(
    "hello \033[5mbliking slowly oh my\033[25m ",
    "normal \033[6mblinking quickly oh my\033[25m normal"
  )
  strwrap_csi(hello.blinky, 10)
})
unitizer_sect("Long Wrap", {
  wrap.nrm <- strwrap(strip_ansi(lorem.r.thanks), 40)
  wrap.csi <- strwrap_csi(lorem.r.thanks, 40)

  identical(wrap.nrm, strip_ansi(wrap.csi))
  nchar(strip_ansi(wrap.csi))
  nchar(wrap.csi)
})
unitizer_sect("Other Escapes", {
  strwrap_csi("hello \033kworld yohoo", 12)
  strwrap_csi("hello \033\nworld yohoo", 12)

  # c0 escapes should be treated as zero width

  strwrap_csi("hello\x1F\x1F\x1F\x1F\x1F\x1F world yohoo", 12)
})
unitizer_sect("prefix / initial", {
  pre <- "\033[32m+ \033[0m"
  ini <- "\033[33m> \033[0m"

  hello.8a <- "hello world yohoo"

  wrap.csi.2 <- strwrap_csi(hello.8a, 10, prefix=pre, initial=ini)
  wrap.csi.2
  wrap.nrm.2 <- strwrap(hello.8a, 10, prefix="+ ", initial="> ")
  identical(strip_ansi(wrap.csi.2), wrap.nrm)

  hello.8b <- c(hello.8a, "oh my this has 2 elements")
  strwrap_csi(hello.8b, 10, prefix=pre, initial=ini)

  pre.2 <- "\x1b[32m\xd0\x9f \x1b[0m"
  ini.2 <- "\x1b[33m\xd1\x80 \x1b[0m"
  hello.8c <- "hello Привет world"

  Encoding(pre.2) <- "UTF-8"
  Encoding(ini.2) <- "UTF-8"
  Encoding(hello.8c) <- "UTF-8"

  strwrap_csi(hello.8c, 10, prefix=pre.2, initial=ini.2)
})

# Things to test:
#
# * Ansi in prefix, initial, and body
# * UTF8 in prefix, initial, and body
# * Wide UTF8, combining UTF8
# * Special/control characters
# * Tabs
# * leading spaces
# * prefix / initial / indent / exdent
# * words without breaks that exceed width
