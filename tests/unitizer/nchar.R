library(fansi)

unitizer_sect('basic tests', {
  nchar_esc(c('hello', 'world'))
  nchar_esc(c('hello', 'world'), type='width')
  nchar_esc(c('A\u030A'))
  nchar(c('A\u030A'))  # for reference, base gets it wrong too
  nchar_esc(c('A\u030A'), type='width')

  # Wide chars

  nchar_esc("\u4E00\u4E01\u4E03")
  nchar_esc("\u4E00\u4E01\u4E03", type='width')

  # Keep NA

  na.world <- c('hello', NA, 'world')
  nchar_esc(na.world)
  nchar_esc(na.world, keepNA=FALSE)
  nchar_esc(na.world, keepNA=NA, type='width')
  nchar_esc(na.world, keepNA=TRUE, type='width')

  nzchar_esc(na.world)
  nzchar_esc(na.world, keepNA=NA)
  nzchar_esc(na.world, keepNA=TRUE)

  # Allow NA for illegal sequences

  hello.illegal <- c("hello", "\xF0", "\xF0aaaa")
  Encoding(hello.illegal) <- 'UTF-8'

  nchar_esc(hello.illegal)
  nchar_esc(hello.illegal, allowNA=TRUE)

  # nzchar doesn't care about multi-byte illegal

  nzchar_esc(hello.illegal)
})
unitizer_sect('with escapes', {
  esc.1 <- sprintf(
    "hello \033[31mworld\033[m%s\033[48;5;123m blahs \033[m%s",
    "\u76F4\u8349",
    "\u56FA\u55F0\u5F8C"
  )
  Encoding(esc.1) <- 'UTF-8'
  nchar_esc(esc.1)
  nchar_esc(esc.1, type='width')

  nzchar_esc(esc.1)

  esc.2 <- "\n\r\033P\033[31m\a"

  nchar_esc(esc.2)
  nchar_esc(esc.2, warn=FALSE)

  nzchar_esc(esc.2)
  nzchar_esc(esc.2, warn=FALSE)

  nchar_esc(c(esc.1, esc.2, 'hello'), warn=FALSE)
})
