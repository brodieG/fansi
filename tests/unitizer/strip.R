library(fansi)

unitizer_sect("Strip ansi", {
  end <- "\033[0m"
  red <- "\033[31m"
  inv <- "\033[7m"
  grn.bg <- "\033[42m"
  rgb.und <- "\033[4;38;2;0;120;200m"

  strip_ansi(sprintf("hello %sworld%s", red, end))

})
