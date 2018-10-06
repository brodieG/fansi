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
unitizer_sect("Multi-line", {
  str.m.0 <- paste0(
    "\033[44m",
    c("hello world", rep("goodbye \033[45mmoon", 2), "yowza bombastic"),
    "\033[m"
  )
  substr_ctl(str.m.0, (1:4) * 2, (3:8) * 2)
})
unitizer_sect("tabs", {
  substr2_ctl("yo\tworld", 1, 8, tabs.as.spaces=TRUE)
})
unitizer_sect("Corner cases", {
  substr_ctl("hello", 0, -1)
  substr_ctl("hello", 0, 0)
  substr_ctl(rep("hello", 2), c(1, 0), c(1, 1))

  substr_ctl(character(), 1, 1)
  substr_ctl(list("hello", list("goodbye", "there")), 1, 2)
  substr_ctl(structure(list(list("goodbye", "there")), class="foo"), 1, 2)

  str.0 <- "\033[31mred\033[m"
  str.1 <- "\033[31mred\033[42m"
  str.2 <- c(str.0, str.1)

  substr_ctl(str.2, 0, 0)
  substr_ctl(str.2, 1, 1)
  substr_ctl(str.2, 3, 3)
  substr_ctl(str.2, 4, 4)

  substr_ctl(str.2, 3, 4)
  substr_ctl(str.2, 3, 5)

  substr_ctl(str.2, -1, 2)
  substr_ctl(str.2, -2, -1)

  substr_ctl("hello", 5, 5)
  substr_ctl("hello", 6, 6)
  substr_ctl("hello", 7, 6)
  substr_ctl("hello", 6, 7)
  substr_ctl("hello", 7, 5)

  substr_ctl("hello", 0, 6)
  substr_ctl("hello", 0, 5)
  substr_ctl("hello", 1, 6)

  substr_ctl("hello", "1", 1)
  substr_ctl("hello", 1, "1")

  substr_ctl("hello", "a", "b")

  substr_ctl("hello", 1, NA_integer_)
  substr_ctl("hello", NA_integer_, 1)

  # Nested

  substr_ctl(rep("\033[31mhello\033[m", 3), c(3,2,1), c(3,4,5))

  # Preserve attributes

  str.3 <- structure("fu\033[42mba\033[0mr", class="foo", at="bar")
  substr_ctl(str.3, 2, 3)

  # Turn off sgr

  substr_ctl(str.2, 2, 6, ctl=c('all', 'sgr'))
  substr_ctl(str.2, 8, 10, ctl=c('all', 'sgr'))
})
unitizer_sect("Obscure escapes", {
  # illegal 38/48

  tryCatch(
    substr_ctl("\033[38;6;31mworld\033[m", 2, 3),
    warning=conditionMessage
  )
  suppressWarnings(substr_ctl("\033[38;6;31mworld\033[m", 2, 3))

  # illegal colors leave prior color unchanged

  tryCatch(
    substr_ctl("\033[31mhello\033[38;5;256m world\033[m", 7, 8),
    warning=conditionMessage
  )
  suppressWarnings(substr_ctl("\033[31mhello\033[38;5;256m world\033[m", 7, 8))

  # fraktur and double underline and prop spacing, and other odd ones

  substr_ctl("\033[20mworld\033[m", 2, 3)
  substr_ctl("\033[21mworld\033[m", 2, 3)
  substr_ctl(rep("\033[26mhello \033[50mworld\033[m", 2), c(2, 8), c(3, 10))
  substr_ctl(rep("\033[61mwor\033[65mld\033[m", 2), c(2, 4), c(3, 5))

  # unknown tokens

  tryCatch(
    substr_ctl("\033[56mworld\033[m", 2, 3),
    warning=conditionMessage
  )
  suppressWarnings(substr_ctl("\033[56mworld\033[m", 2, 3))
  tryCatch(
    substr_ctl("\033[66mworld\033[m", 2, 3),
    warning=conditionMessage
  )
  tryCatch(
    substr_ctl("\033[200mworld\033[m", 2, 3),
    warning=conditionMessage
  )
  # bright colors

  substr_ctl(rep("\033[91mwor\033[101mld\033[m", 2), c(2, 4), c(3, 5))
})
unitizer_sect('bad args', {
  # bad args

  hello2.0 <- "\033[42m\thello world\033[m foobar"
  substr2_ctl(hello2.0, 1, 2, warn=NULL)

  substr2_ctl(hello2.0, 1, 2, tabs.as.spaces=1)
  substr2_ctl(hello2.0, 1, 2, tabs.as.spaces=NA)
  substr2_ctl(hello2.0, 1, 2, tab.stops=-(1:3))
  substr2_ctl(hello2.0, 1, 2, tab.stops=0)
  substr2_ctl(hello2.0, 1, 2, round='bananas')
  substr2_ctl(hello2.0, 1, 2, term.cap=0)
  substr2_ctl(hello2.0, 1, 2, term.cap='bananas')
  substr2_ctl(hello2.0, 1, 2, type='bananas')

  substr2_ctl(hello2.0, 1, 2, ctl='bananas')
  substr2_ctl(hello2.0, 1, 2, ctl=0)

})
unitizer_sect('`ctl` related issues', {
  # Make sure SGR end properly detected

  substr_sgr("\033[31;42mhello world", 2, 4)

  # Repeated SGR

  substr_sgr("\033[31m\033[42mhello world", 2, 4)

  # Intermediate byte (this is not an SGR!); tryCatch due to inconsistency
  # on whether call is included in condition message

  tryCatch(
    substr_sgr("\033[31;42!mhello world", 2, 4),
    warning=function(x) conditionMessage(x)
  )
  # non-SGR CSI mixed with SGR when not parsing non-SGR CSI

  substr_sgr("\033[55;38l\033[31mhello world", 2, 4, warn=FALSE)
  substr_sgr("\033[31m\033[55;38lhello world", 2, 4, warn=FALSE)
  substr_sgr("hello \033[31m\033[55;38lworld", 7, 9, warn=FALSE)

  # Mix of escapes

  substr_ctl("\033[55;38l\033[31mhello world", 2, 4, warn=FALSE)
  substr_ctl("\033[31m\033[55;38lhello world", 2, 4, warn=FALSE)
  substr_ctl("hello \033[31m\033[55;38lworld", 7, 9, warn=FALSE)
  substr_ctl("hello\033[55;38l \033[31mworld", 4, 7, warn=FALSE)

  # C0 / nl

  substr_sgr("ab\n\tcd\n", 3, 6, warn=FALSE)
  substr_sgr("ab\n\033[31m\tcd\n", 3, 6, warn=FALSE)
  substr_ctl("ab\n\033[31m\tcd\n", 3, 6, warn=FALSE, ctl=c('all', 'nl'))
  substr_ctl("ab\n\033[31m\tcd\n", 3, 6, warn=FALSE, ctl=c('all', 'nl', 'c0'))
})
