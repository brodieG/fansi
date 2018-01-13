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
