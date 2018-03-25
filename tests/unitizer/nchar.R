library(fansi)

unitizer_sect('basic tests', {
  nchar_ctl(c('hello', 'world'))
  nchar_ctl(c('hello', 'world'), type='width')

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
