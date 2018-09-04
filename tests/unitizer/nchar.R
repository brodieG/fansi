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

  # not an SGR

  nchar_ctl("\033[31#mworld", ctl="sgr")
  nchar_ctl("\033[31#mworld", ctl="csi")
})
unitizer_sect('ctl', {
  esc.3 <- "\n\t\033[31m\033[41!m\033p"
  nzchar_ctl(esc.3, warn=FALSE)
  nzchar_ctl(sprintf("%sa", esc.3), warn=FALSE)
  nzchar_ctl(esc.3, ctl=c('sgr', 'csi', 'esc'), warn=FALSE)
  nzchar_ctl(esc.3, ctl=c('c0', 'nl'), warn=FALSE)
  nzchar_ctl("\n\t\n", ctl=c('nl'), warn=FALSE)
  nzchar_ctl("\t\n", ctl=c('nl'), warn=FALSE)
})
unitizer_sect('bad inputs', {
  nchar_ctl(9:10, warn=1:3)
  nchar_ctl("hello\033[31m world", allowNA=1:3)
  nchar_ctl("hello\033[31m world", keepNA=1:3)
  nchar_ctl("hello\033[31m world", strip=1:3)
  nchar_ctl("hello\033[31m world", ctl="bananas")
  nchar_ctl("hello\033[31m world", type=NA_character_)
  nchar_ctl("hello\033[31m world", type=1)
  nchar_ctl("hello\033[31m world", type="bananas")

  nzchar_ctl(9:10, warn=1:3)
  nzchar_ctl("hello\033[31m world", keepNA=1:3)
  nzchar_ctl("hello\033[31m world", ctl=1)
  nzchar_ctl("hello\033[31m world", ctl="bananas")
})
