library(fansi)

unitizer_sect("Strip ansi", {
  strip_esc(sprintf("hello %sworld%s", red, end))
  strip_esc(sprintf("he%sllo %sworld", red, end))
  strip_esc(sprintf("%shello %sworld%s", grn.bg, red, end))
  strip_esc(sprintf("%s%shello %sworld%s", grn.bg, inv, red, end))

  string <- paste("string", format(1:10))
  string[c(2,4,6)] <- paste0(red, string[c(2,4,6)], end)

  strip_esc(string)
})
unitizer_sect("Corner cases", {
  strip_esc("hello\033")
  # should this be stripped?  Not 100% clear since terminal seems to be waiting
  # for input after it is cated
  strip_esc("hello\033[")

  # illegal sequence

  strip_esc("hello\033[31##3m illegal")
  strip_esc("hello\033[31##m legal")
})
unitizer_sect("Whitespace", {
  fansi:::process('hello     world')
  fansi:::process('hello.    world')
  fansi:::process(c('hello     world', 'hello.    world'))
  fansi:::process('hello.   world?   moon!   wow.')
  fansi:::process('  hello')
  fansi:::process('  hello\n  world')
  fansi:::process('  hello  \n  world')
  fansi:::process('  hello world\n  ')
  fansi:::process('hello.   ')
  fansi:::process('hello!  ')
  fansi:::process('hello? ')
  fansi:::process('hello? ')

  # Tabs / ctrl; newlines remain

  fansi:::process(' \t hello')
  fansi:::process(' \t\a\r hello')

  # interactiong between punct and ctrl

  fansi:::process('hello.  \r world.')

  # CSIs

  fansi:::process('hello.  \033[31m world.\033[0m')

  # Make sure we are not inadvertently changing SXPs

  str1 <- c("hello ", " world")
  fansi:::process(str1)
  str1

  # Paragraphs and so on

  fansi:::process('hello.\n\nworld')
  fansi:::process('hello.\n\n\nworld')
  fansi:::process('hello.\n\n\n\nworld')
  fansi:::process('hello.\n  \nworld')
  fansi:::process('hello.\n\t\nworld')
  fansi:::process('hello.\n\t\n\tworld')
  fansi:::process('hello.\n \t \n \t world')
  fansi:::process('hello.\n\nworld\n\n')
  fansi:::process('hello.\n\nworld\n\n  ')
  fansi:::process('\n\nhello.\n\t\n\tworld\n\t\n woohoo\n ')
  fansi:::process('\n \t\nhello.\n\t\n\tworld\n\t\n woohoo\n ')
})
unitizer_sect("Selective stripping", {
  string.0 <- "hello\033k\033[45p world\n\033[31mgoodbye\a moon"

  strip_esc(string.0)
  strip_esc(string.0, "sgr")
  strip_esc(string.0, c("nl", "c0", "sgr", "csi", "esc"))
  strip_esc(string.0, "all")  # equivalently
  strip_esc(string.0, c("c0", "esc"))
  strip_esc(string.0, c("nl"))

  # negations

  strip_esc(string.0, c("all", "c0", "esc"))
  strip_esc(string.0, c("all", "sgr"))

  # add some illegal sequences

  string.1 <- "hello\033\033[45p world\n\033[31#3mgoodbye\a moon"

  strip_esc(string.1, c("nl", "sgr", "esc"))
  strip_esc(string.1, "all")
  strip_esc(string.1, c("c0", "nl"))
  strip_esc(string.1, c("all", "sgr"))

  # longer vec

  strip_esc(c(string.0, string.1, "hello"), warn=FALSE)

  # possible corner cases

  string.2 <- "\033k\033[45p\a\n\033[31mgoodbye moon"
  strip_esc(string.2)
  strip_esc(string.2, "sgr")

  string.3 <- "hello world\033k\033[45p\a\n\033[31m"
  strip_esc(string.3)
  strip_esc(string.3, "sgr")
})
