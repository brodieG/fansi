library(fansi)

unitizer_sect("Strip ansi", {
  strip_ctl(sprintf("hello %sworld%s", red, end))
  strip_ctl(sprintf("he%sllo %sworld", red, end))
  strip_ctl(sprintf("%shello %sworld%s", grn.bg, red, end))
  strip_ctl(sprintf("%s%shello %sworld%s", grn.bg, inv, red, end))

  string <- paste("string", format(1:10))
  string[c(2,4,6)] <- paste0(red, string[c(2,4,6)], end)

  strip_ctl(string)
  strip_sgr(string)
  strip_sgr(1:3)
})
unitizer_sect("Corner cases", {
  strip_ctl("hello\033")
  # should this be stripped?  Not 100% clear since terminal seems to be waiting
  # for input after it is cated
  strip_ctl("hello\033[")

  # illegal sequence

  strip_ctl("hello\033[31##3m illegal")
  strip_ctl("hello\033[31##m legal")

  # non-char inputs; really should just coerce to char and move on since we know
  # these can't contain the sequences (actually, only true of numerics; other
  # objects e.g. Diff can produce chars with sequences)

  strip_ctl(1:3)
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

  strip_ctl(string.0)
  strip_ctl(string.0, "sgr")
  strip_ctl(string.0, c("nl", "c0", "sgr", "csi", "esc"))
  strip_ctl(string.0, "all")  # equivalently
  strip_ctl(string.0, c("c0", "esc"))
  strip_ctl(string.0, c("nl"))

  # don't strip anything (null op)

  strip_ctl(string.0, character())

  # negations

  strip_ctl(string.0, c("all", "c0", "esc"))
  strip_ctl(string.0, c("all", "sgr"))

  # add some illegal sequences

  string.1 <- "hello\033\033[45p world\n\033[31#3mgoodbye\a moon"

  strip_ctl(string.1, c("nl", "sgr", "esc"))
  strip_ctl(string.1, c("csi"))
  strip_ctl(string.1, "all")
  strip_ctl(string.1, c("c0", "nl"))
  strip_ctl(string.1, c("all", "sgr"))

  strip_sgr(string.1)

  # longer vec

  strip_ctl(c(string.0, string.1, "hello"), warn=FALSE)

  # possible corner cases

  string.2 <- "\033k\033[45p\a\n\033[31mgoodbye moon"
  strip_ctl(string.2)
  strip_ctl(string.2, "sgr")

  string.3 <- "hello world\033k\033[45p\a\n\033[31m"
  strip_ctl(string.3)
  strip_ctl(string.3, "sgr")

})
unitizer_sect("Bad Inputs", {
  strip_ctl("hello\033[41mworld", warn=1:3)
  strip_ctl("hello\033[41mworld", ctl=1:3)
  strip_ctl("hello\033[41mworld", ctl="bananas")
  strip_ctl("hello\033[41mworld", strip="sgr")

  strip_sgr("hello\033[41mworld", warn=1:3)

})
