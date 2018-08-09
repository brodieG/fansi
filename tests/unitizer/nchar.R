library(fansi)

unitizer_sect('basic tests', {
  nchar_ctl(c('hello', 'world'))
  nchar_ctl(c('hello', 'world'), type='width')
  nchar_ctl(c('hello', 'world'), type='wi') # partial match

  # Keep NA

  na.world <- c('hello', NA, 'world')
  nchar_ctl(na.world)
  nchar_ctl(na.world, keepNA=FALSE)
  nchar_ctl(na.world, keepNA=NA, type='width')
  nchar_ctl(na.world, keepNA=TRUE, type='width')

  nzchar_ctl(na.world)
  nzchar_ctl(na.world, keepNA=NA)
  nzchar_ctl(na.world, keepNA=TRUE)

})
unitizer_sect('with escapes', {
  esc.2 <- "\n\r\033P\033[31m\a"

  nchar_ctl(esc.2)
  nchar_ctl(esc.2, warn=FALSE)

  nzchar_ctl(esc.2)
  nzchar_ctl(esc.2, warn=FALSE)
})
unitizer_sect('bad inputs', {
  nchar_ctl(9:10, warn=1:3)
  nchar_ctl("hello\033[31m world", allowNA=1:3)
  nchar_ctl("hello\033[31m world", keepNA=1:3)
  nchar_ctl("hello\033[31m world", strip=1:3)
  nchar_ctl("hello\033[31m world", strip="bananas")
  nchar_ctl("hello\033[31m world", type=NA_character_)
  nchar_ctl("hello\033[31m world", type=1)
  nchar_ctl("hello\033[31m world", type="bananas")

  nzchar_ctl(9:10, warn=1:3)
  nzchar_ctl("hello\033[31m world", keepNA=1:3)
})
