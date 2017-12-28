library(fansi)

unitizer_sect("Strip ansi", {
  strip_ansi(sprintf("hello %sworld%s", red, end))
  strip_ansi(sprintf("he%sllo %sworld", red, end))
  strip_ansi(sprintf("%shello %sworld%s", grn.bg, red, end))
  strip_ansi(sprintf("%s%shello %sworld%s", grn.bg, inv, red, end))

  string <- paste("string", format(1:10))
  string[c(2,4,6)] <- paste0(red, string[c(2,4,6)], end)

  strip_ansi(string)
})
unitizer_sect("Corner cases", {
  strip_ansi("hello\033")
  # should this be stripped?  Not 100% clear since terminal seems to be waiting
  # for input after it is cated
  strip_ansi("hello\033[")
})

unitizer_sect("Whitespace", {
  fansi:::strip_white('hello     world')
  fansi:::strip_white('hello.    world')
  fansi:::strip_white(c('hello     world', 'hello.    world'))
  fansi:::strip_white('hello.   world?   moon!   wow.')
  fansi:::strip_white('  hello')
})
