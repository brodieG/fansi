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
})

# Things to test:
#
# * Ansi in prefix, initial, and body
# * UTF8 in prefix, initial, and body
# * Wide UTF8, combining UTF8
# * Paragraphs / newlines
# * Special/control characters
# * Spaces in sequence
# * Tabs
# * leading spaces
# * prefix / initial / indent / exdent
# * words without breaks that exceed width
