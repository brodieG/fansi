library(fansi)

unitizer_sect("Simple", {
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

  str04 <- sprintf("hello%s%s world%s%s yowza", red, inv, grn.bg, rgb.und)

  ansi_substr2(str04, 5, 7)
  ansi_substr2(str04, 5, 13)
})
