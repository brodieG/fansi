library(fansi)

unitizer_sect("Simple", {
  end <- "\033[0m"
  red <- "\033[31m"
  inv <- "\033[7m"
  grn.bg <- "\033[42m"
  rgb.und <- "\033[4;38;2;0;120;200m"
  rgb.256 <- "\033[48;5;141m"

  str01 <- sprintf("hello %sworld%s how", red, inv);

  ansi_substr2(str01, 1, 7)
  ansi_substr2(str01, 7, 11)
  ansi_substr2(str01, 8, 10)
  ansi_substr2(str01, 8, 14)

  str02 <- sprintf(
    "%shello world %sit's a %scrazy world%s out there %sisn't it%s%s right?",
    grn.bg, red, end, rgb.und, inv, end, rgb.256
  )
  ansi_substr2(str02, 1, 7)
  ansi_substr2(str02, 10, 20)
  ansi_substr2(str02, 15, 40)
  ansi_substr2(str02, 35, 60)

  str03 <-sprintf("hello %sworld", rgb.und)

  ansi_substr2(str03, 1, 12)
})
