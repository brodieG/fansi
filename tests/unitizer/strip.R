library(fansi)

unitizer_sect("Strip ansi", {
  end <- "\033[0m"
  red <- "\033[31m"
  inv <- "\033[7m"
  grn.bg <- "\033[42m"
  rgb.und <- "\033[4;38;2;0;120;200m"

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
