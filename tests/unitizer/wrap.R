library(unitizer)

unitizer_sect("Basic wrap", {
  strwrap_csi("hello world this is a lovely day", width=15)
})

# Things to test:
#
# * Ansi in prefix, initial, and body
# * UTF8 in prefix, initial, and body
# * Wide UTF8, combining UTF8
# * Paragraphs
