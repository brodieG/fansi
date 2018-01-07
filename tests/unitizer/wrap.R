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

  hello.6 <- 'hello."  there how are you'
  identical(strwrap(hello.6, width=15), strwrap_csi(hello.6, width=15))
})
unitizer_sect("Basic Ansi", {
  hello2.0 <-
    paste0("hello ", red, "world ", grn.bg, " this is a  lovely", end, "day.")
  strwrap_csi(hello2.0, 10)

  identical(
    strwrap_csi(strip_ansi(hello2.0), 10), strwrap(strip_ansi(hello2.0), 10)
  )
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
