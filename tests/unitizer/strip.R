## Copyright (C) 2022 Brodie Gaslam
##
## This file is part of "fansi - ANSI Control Sequence Aware String Functions"
##
## This program is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 2 or 3 of the License.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## Go to <https://www.r-project.org/Licenses> for copies of the licenses.

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
  # Even partially recognized escapes are stripped assuming that they could be
  # recognized at all (with the special exception that a single leading ESC will
  # be stripped if any control is active).
  strip_ctl("hello\033")
  strip_ctl("hello\033", ctl=c('nl', 'c0'))
  strip_ctl("hello\033[")
  strip_ctl("hello\033[42")
  strip_ctl("hello\033[42", ctl=c('all', 'csi', 'sgr'))

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
  fansi:::process(' \t\a\r hello', ctl=c("all", "c0"))

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

  # corner cases
  fansi:::process('hello.\n\033[44m\nworld')
  fansi:::process('hello.\n\033[44m\n \t\nworld')
  fansi:::process('hello.\033[44m\n\n \t\nworld')
  fansi:::process('hello.\n\n \t\n\033[44mworld')
  fansi:::process('hello.\n\n\033[44m \t\nworld')

  fansi:::process('hello \033[44m world')
  fansi:::process("hello. \033[44m world")

  fansi:::process('hello\033[44m\033[31m world')
  fansi:::process('hello\033[44m\033[31m\n\nworld')
  fansi:::process('hello\n\033[44m\033[31m\nworld')
  fansi:::process('hello\n\n\033[44m\033[31mworld')

  fansi:::process('hello\033[44m\033[31d world')
  fansi:::process('hello \033[44m\033[31d world')
  fansi:::process('hello \033[44m \033[31d world')
  fansi:::process('hello\033[44m\033[31d world', ctl=c("all", "csi"))
  fansi:::process('hello \033[44m\033[31d world', ctl=c("all", "csi"))
  fansi:::process('hello \033[44m \033[31d world', ctl=c("all", "csi"))
  fansi:::process('hello\033[44m\a world', ctl=c("all"))
  fansi:::process('hello\033[44m\a world', ctl=c("all", "c0"))
  fansi:::process('hello.  \033[44m\a world', ctl=c("all"))
  fansi:::process('hello.  \033[44m\a world', ctl=c("all", "c0"))
  fansi:::process('hello. \033[44m \a world', ctl=c("all"))
  fansi:::process('hello. \033[44m \a world', ctl=c("all", "c0"))
  fansi:::process('hello.\n\033[44m \a world', ctl=c("all"))
  fansi:::process('hello.\n\033[44m \a world', ctl=c("all", "c0"))
  fansi:::process('hello.\n\033[44m\n\a world', ctl=c("all"))
  fansi:::process('hello.\n\033[44m\n\a world', ctl=c("all", "c0"))
  fansi:::process('hello.\n\033[44m\a\n world', ctl=c("all"))
  fansi:::process('hello.\n\033[44m\a\n world', ctl=c("all", "c0"))
})
unitizer_sect("Selective stripping", {
  string.0 <- "hello\033k\033[45p world\n\033[31mgoodbye\a moon"

  strip_ctl(string.0)
  strip_ctl(string.0, "sgr")
  strip_ctl(string.0, c("nl", "c0", "sgr", "csi", "esc"))
  strip_ctl(string.0, "all")  # equivalently
  # this breaks CSIs
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
