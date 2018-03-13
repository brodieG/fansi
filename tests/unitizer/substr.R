library(fansi)

unitizer_sect("Simple", {
  str01 <- sprintf("hello %sworld%s how", red, inv);

  substr_ctl(str01, 1, 7)
  substr_ctl(str01, 7, 11)
  substr_ctl(str01, 8, 10)
  substr_ctl(str01, 8, 14)

  str02 <- sprintf(
    "%shello world %sit's a %scrazy world%s out there %sisn't it%s%s right?",
    grn.bg, red, end, rgb.und, inv, end, rgb.256
  )
  # enable truecolor as not enabled by default
  term.cap <- c('bright', '256', 'truecolor')

  substr_ctl(str02, 1, 7)
  substr_ctl(str02, 10, 20)
  substr_ctl(str02, 15, 40, term.cap=term.cap)
  substr_ctl(str02, 35, 60, term.cap=term.cap)

  str03 <-sprintf("hello %sworld", rgb.und)

  substr_ctl(str03, 1, 12, term.cap=term.cap)

  str04 <- sprintf("hello%s%s world%s%s yowza", red, inv, grn.bg, rgb.und)

  substr_ctl(str04, 5, 7, term.cap=term.cap)
  substr_ctl(str04, 5, 13, term.cap=term.cap)
})
unitizer_sect("Corner cases", {
  substr_ctl("hello", 0, -1)
  substr_ctl("hello", 0, 0)
  substr_ctl(rep("hello", 2), c(1, 0), c(1, 1))
})
