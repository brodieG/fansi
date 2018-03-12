library(fansi)

unitizer_sect('basic tests', {
  nchar_ctl(c('hello', 'world'))
  nchar_ctl(c('hello', 'world'), type='width')
  nchar_ctl(c('A\u030A'))
  nchar(c('A\u030A'))  # for reference, base gets it wrong too
  nchar_ctl(c('A\u030A'), type='width')

  # Wide chars

  nchar_ctl("\u4E00\u4E01\u4E03")
  nchar_ctl("\u4E00\u4E01\u4E03", type='width')

  # Keep NA

  na.world <- c('hello', NA, 'world')
  nchar_ctl(na.world)
  nchar_ctl(na.world, keepNA=FALSE)
  nchar_ctl(na.world, keepNA=NA, type='width')
  nchar_ctl(na.world, keepNA=TRUE, type='width')

  nzchar_ctl(na.world)
  nzchar_ctl(na.world, keepNA=NA)
  nzchar_ctl(na.world, keepNA=TRUE)

  # Allow NA for illegal sequences

  hello.illegal <- c("hello", "\xF0", "\xF0aaaa")
  Encoding(hello.illegal) <- 'UTF-8'

  nchar_ctl(hello.illegal)
  nchar_ctl(hello.illegal, allowNA=TRUE)

  # nzchar doesn't care about multi-byte illegal

  nzchar_ctl(hello.illegal)
})
unitizer_sect('with escapes', {
  esc.1 <- sprintf(
    "hello \033[31mworld\033[m%s\033[48;5;123m blahs \033[m%s",
    "\u76F4\u8349",
    "\u56FA\u55F0\u5F8C"
  )
  Encoding(esc.1) <- 'UTF-8'
  nchar_ctl(esc.1)
  nchar_ctl(esc.1, type='width')

  nzchar_ctl(esc.1)

  esc.2 <- "\n\r\033P\033[31m\a"

  nchar_ctl(esc.2)
  nchar_ctl(esc.2, warn=FALSE)

  nzchar_ctl(esc.2)
  nzchar_ctl(esc.2, warn=FALSE)

  nchar_ctl(c(esc.1, esc.2, 'hello'), warn=FALSE)
})
