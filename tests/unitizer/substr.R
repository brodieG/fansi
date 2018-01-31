library(fansi)

unitizer_sect("Simple", {
  str01 <- sprintf("hello %sworld%s how", red, inv);

  substr_esc(str01, 1, 7)
  substr_esc(str01, 7, 11)
  substr_esc(str01, 8, 10)
  substr_esc(str01, 8, 14)

  str02 <- sprintf(
    "%shello world %sit's a %scrazy world%s out there %sisn't it%s%s right?",
    grn.bg, red, end, rgb.und, inv, end, rgb.256
  )
  substr_esc(str02, 1, 7)
  substr_esc(str02, 10, 20)
  substr_esc(str02, 15, 40)
  substr_esc(str02, 35, 60)

  str03 <-sprintf("hello %sworld", rgb.und)

  substr_esc(str03, 1, 12)

  str04 <- sprintf("hello%s%s world%s%s yowza", red, inv, grn.bg, rgb.und)

  substr_esc(str04, 5, 7)
  substr_esc(str04, 5, 13)

  # single character
})

