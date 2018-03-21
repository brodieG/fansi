library(unitizer)
library(fansi)

unitizer_sect("Basic wrap", {
  hello.0 <- "hello world this is a lovely day"
  identical(strwrap_ctl(hello.0, width=10), strwrap(hello.0, width=10))

  hello.1 <- "hello  world  this  is.  a lovely day."
  identical(strwrap_ctl(hello.1, width=10), strwrap(hello.1, width=10))

  writeLines(strwrap_ctl(hello.1, width=10))
  writeLines(strwrap(hello.1, width=10))

  hello.2 <- "hello\rworld\rthis  is.  a lovely day."
  identical(strwrap(hello.2, width=10), strwrap_ctl(hello.2, width=10))

  hello.3 <- "hello\rworld\nthis  is.  a lovely\n day."
  identical(strwrap(hello.3, width=10), strwrap_ctl(hello.3, width=10))

  hello.4 <- "  hello  world  this  is  a lovely day."
  identical(strwrap(hello.4, width=10), strwrap_ctl(hello.4, width=10))

  hello.5 <- "hello.\n\n\nworld"
  identical(strwrap(hello.5, width=10), strwrap_ctl(hello.5, width=10))

  hello.5a <- "hello.\n \n \nworld"
  identical(strwrap(hello.5a, width=10), strwrap_ctl(hello.5a, width=10))

  # special preserve of double space

  hello.6a <- 'hello."  there'
  identical(strwrap(hello.6a, width=40), strwrap_ctl(hello.6a, width=40))

  hello.6b <- 'hello.\'  there'
  identical(strwrap(hello.6b, width=40), strwrap_ctl(hello.6b, width=40))

  hello.6c <- 'hello.)  there'
  identical(strwrap(hello.6c, width=40), strwrap_ctl(hello.6c, width=40))

})
unitizer_sect("Basic Ansi", {
  hello2.0 <-
    paste0("hello ", red, "world ", grn.bg, " this is a  lovely", end, "day.")
  strwrap_ctl(hello2.0, 10)

  identical(
    strwrap_ctl(strip_ctl(hello2.0, "sgr"), 10),
    strwrap(strip_ctl(hello2.0, "sgr"), 10)
  )
  # turn off tag generic

  hello2.1 <- paste0("hello \033[41mworld\033[m how are you today")
  hello2.2 <- paste0("hello \033[41mworld\033[0m how are you today")

  strwrap_ctl(hello2.1, 15)

  # Specific turn off tags - turn off bold and faint

  hello.bold.faint <- paste0(
    "hello \033[1mbolded once upon a time\033[22m ",
    "normal \033[2mfainting in faintness oh no\033[22m normal"
  )
  strwrap_ctl(hello.bold.faint, 10)

  # Specific turn off tags - blinking

  hello.blinky <- paste0(
    "hello \033[5mbliking slowly oh my\033[25m ",
    "normal \033[6mblinking quickly oh my\033[25m normal"
  )
  strwrap_ctl(hello.blinky, 10)
})
unitizer_sect("Long Wrap", {
  wrap.nrm <- strwrap(strip_ctl(lorem.r.thanks, "sgr"), 40)
  wrap.csi <- strwrap_ctl(lorem.r.thanks, 40)

  identical(wrap.nrm, strip_ctl(wrap.csi, "sgr"))
  nchar(strip_ctl(wrap.csi, "sgr"))
  nchar(wrap.csi)
})
unitizer_sect("Other Escapes", {
  strwrap_ctl("hello \033kworld yohoo", 12)
  strwrap_ctl("hello \033\nworld yohoo", 12)

  # c0 escapes should be treated as zero width

  strwrap_ctl("hello\x1F\x1F\x1F\x1F\x1F\x1F world yohoo", 12)

  # Various different types of warnings

  strwrap_ctl("hello \033[999mworld", 6)
  strwrap_ctl("hello \033[31#31mworld", 6)
  strwrap_ctl("hello \033[999nworld", 6)

  strwrap_ctl("hello \033[999mworld", 6, warn=FALSE)
  strwrap_ctl("hello \033[31#31mworld", 6, warn=FALSE)
  strwrap_ctl("hello \033[999nworld", 6, warn=FALSE)
})
unitizer_sect("prefix / initial simple", {
  # a version of lorem with paragraphs

  lorem.sentence <- unlist(strsplit(lorem, "[.]\\K ", perl=TRUE))
  lorem.sentence <- gsub(",", ",\n", lorem.sentence, fixed=TRUE)
  lorem.para <- c(
    paste0(lorem.sentence[1:2], collapse="\n\n"),
    paste0(lorem.sentence[3:4], collapse="\n\t\n\t  \n")
  )
  identical(
    strwrap_ctl(lorem.para, indent=2), strwrap(lorem.para, indent=2)
  )
  identical(
    strwrap_ctl(lorem.para, exdent=2), strwrap(lorem.para, exdent=2)
  )
  identical(
    strwrap_ctl(lorem.para, indent=4, exdent=2),
    strwrap(lorem.para, indent=4, exdent=2)
  )
})
unitizer_sect("prefix / initial with ESC", {
  pre <- "\033[32m+ \033[0m"
  ini <- "\033[33m> \033[0m"

  hello.8a <- "hello world yohoo"

  wrap.csi.2 <- strwrap_ctl(hello.8a, 14, prefix=pre, initial=ini)
  wrap.csi.2
  wrap.nrm.2 <- strwrap(hello.8a, 14, prefix="+ ", initial="> ")
  identical(strip_ctl(wrap.csi.2, "sgr"), wrap.nrm.2)

  hello.8b <- c(hello.8a, "oh my this has 2 elements")
  wrap.csi.3 <- strwrap_ctl(hello.8b, 14, prefix=pre, initial=ini)
  wrap.csi.3
  wrap.nrm.3 <- strwrap(hello.8b, 14, prefix="+ ", initial="> ")

  identical(strip_ctl(wrap.csi.3, "sgr"), wrap.nrm.3)

  # With UTF8

  pre.2 <- "\x1b[32m\xd0\x9f \x1b[0m"
  ini.2 <- "\x1b[33m\xd1\x80 \x1b[0m"
  hello.8c <- "hello Привет world"

  Encoding(pre.2) <- "UTF-8"
  Encoding(ini.2) <- "UTF-8"
  Encoding(hello.8c) <- "UTF-8"

  wrap.csi.4 <- strwrap_ctl(hello.8c, 15, prefix=pre.2, initial=ini.2)
  wrap.csi.4
  wrap.nrm.4 <- strwrap(hello.8c, 15, prefix="\xd0\x9f ", initial="\xd1\x80 ")

  identical(strip_ctl(wrap.csi.4, "sgr"), wrap.nrm.4)
})
unitizer_sect("wrap with wide UTF8 and ESC", {
  wrap.mix <- strwrap_ctl(lorem.mix, 25)
  wrap.mix
  identical(strwrap(strip_ctl(lorem.mix, "sgr"), 25), strip_ctl(wrap.mix, "sgr"))

  string <- "\033[37;48;5;32m國官方認定的民族現有56個\033[39;49m"
  Encoding(string) <- "UTF-8"
  strwrap2_ctl(string, 24, wrap.always=TRUE, pad.end=" ")
})
unitizer_sect("wrap2", {
  # Examples
  hello.9a <- "hello\t\033[41mred\033[49m\tworld"

  strwrap2_ctl(hello.9a, 12)
  strwrap2_ctl(hello.9a, 12, tabs.as.spaces=TRUE)
  strwrap2_ctl(hello.9a, 13, tabs.as.spaces=TRUE)
  strwrap2_ctl(hello.9a, 12, tabs.as.spaces=TRUE, tab.stops=c(6, 12))

  r.thanks <- lorem.r.thanks.2

  ## Generate colors from the 256 color palette
  bg <- ceiling(seq_along(r.thanks) / length(r.thanks) * 215) + 16
  fg <- ifelse((((bg - 16) %/% 18) %% 2), 30, 37)
  tpl <- "\033[%d;48;5;%dm%s\033[49m"

  ## Apply colors to strings and collapse
  nz <- nzchar(r.thanks)
  r.thanks[nz] <- sprintf(tpl, fg[nz], bg[nz], r.thanks[nz])
  r.col <- paste0(r.thanks, collapse="\n")

  ## Wrap and display
  r.wrap <- strwrap2_ctl(r.col, 35, pad.end=" ", wrap.always=TRUE)
  # writeLines(c("", paste(" ", r.wrap[1:27], " ", r.wrap[28:54]), ""))
  r.wrap

  ## Pad paragraph break line when it is colored

  hello.9b <- "\033[41mhello\n\nworld."
  strwrap2_ctl(hello.9b, 8, pad.end=" ")

  ## Leading spaces

  hello.9b <- "  \033[41mhello world."
  strwrap2_ctl(hello.9b, 8, strip.spaces=FALSE)
  hello.9c <- "\033[41m  hello world."
})
unitizer_sect("long words", {
  hello.long <- "\033[31mhelloworld\033[mlongword"
  strwrap_ctl(hello.long, 8)
  strwrap2_ctl(hello.long, 8, wrap.always=TRUE)
})
unitizer_sect("rare escapes", {
  hello.border <- c(
    "hello \033[51mworld woohoo\033[54m woohoo",
    "hello \033[52mworld woohoo\033[54m woohoo",
    "hello \033[53mworld woohoo\033[55m woohoo"
  )
  strwrap_ctl(hello.border, 12)
  hello.ideogram <- c(
    "hello \033[60mworld woohoo\033[65m woohoo",
    "hello \033[61mworld woohoo\033[65m woohoo",
    "hello \033[62mworld woohoo\033[65m woohoo",
    "hello \033[63mworld woohoo\033[65m woohoo",
    "hello \033[64mworld woohoo\033[65m woohoo"
  )
  strwrap_ctl(hello.ideogram, 12)
  hello.font <- c(
    "hello \033[10mworld woohoo\033[10m woohoo",
    "hello \033[11mworld woohoo\033[10m woohoo",
    "hello \033[12mworld woohoo\033[10m woohoo",
    "hello \033[13mworld woohoo\033[10m woohoo",
    "hello \033[14mworld woohoo\033[10m woohoo",
    "hello \033[15mworld woohoo\033[10m woohoo",
    "hello \033[16mworld woohoo\033[10m woohoo",
    "hello \033[17mworld woohoo\033[10m woohoo",
    "hello \033[18mworld woohoo\033[10m woohoo",
    "hello \033[19mworld woohoo\033[10m woohoo"
  )
  strwrap_ctl(hello.font, 12)
})
unitizer_sect("term cap and bright", {
  # default term cap should recognize bright and 256, but not true color.
  getOption('fansi.term.cap')
  hello.bright <- '\033[42mhello \033[103mworld wowza\033[49m'

  strwrap_ctl(hello.bright, 13)
  strwrap_ctl(hello.bright, 13, term.cap=character())

  hello.255 <- '\033[42mhello \033[48;5;47mworld wowza\033[49m'
  strwrap_ctl(hello.255, 13)
  strwrap_ctl(hello.255, 13, term.cap=character())

  hello.tru <- '\033[42mhello \033[48;2;7;41;4mworld wowza\033[m'
  strwrap_ctl(hello.tru, 13, term.cap='truecolor')
  strwrap_ctl(hello.tru, 13)
})
unitizer_sect("corner cases", {
  strwrap_ctl("a", -1)
  strwrap2_ctl("a", -1)
  strwrap2_ctl("a", -1, wrap.always=TRUE)
  strwrap2_ctl("a", 0, wrap.always=TRUE)
  strwrap2_ctl("a", 1, wrap.always=TRUE)
  strwrap2_ctl("\u76F4", 1, wrap.always=TRUE)
  strwrap2_ctl("\u76F4", 2, wrap.always=TRUE)
  strwrap2_ctl("\u76F4", 3, wrap.always=TRUE)

  strwrap_ctl("lovelyday.", 10)
  strwrap2_ctl("lovelyday.", 10, wrap.always=TRUE)

  utf8.bad <- "hello \xF0 world, goodnight moon"
  Encoding(utf8.bad) <- "UTF-8"
  strwrap_ctl(utf8.bad, 10)

  # bad prefix values

  utf8.bad.2 <- "\xF0"
  Encoding(utf8.bad.2) <- "UTF-8"

  tryCatch(
    strwrap_ctl("hello world", 6, prefix=utf8.bad.2),
    warning=conditionMessage
  )
  suppressWarnings(strwrap_ctl("hello world", 6, prefix=utf8.bad.2))
  tryCatch(
    strwrap_ctl("hello world", 6, prefix="\033p"),
    warning=conditionMessage
  )
  suppressWarnings(strwrap_ctl("hello world", 6, prefix="\033p"))

  # Invalid inputs (checks in C)

  tryCatch(
    strwrap2_ctl("hello world", 8, pad.end='\t'), error=conditionMessage
  )
  tryCatch(
    strwrap2_ctl("hello world", 8, pad.end='  '), error=conditionMessage
  )
  strwrap2_ctl("goodbye moon", 8, indent=5, prefix='> hello >')
  strwrap2_ctl(
    "goodbye moon", 16, indent=5, prefix='> hello >', wrap.always=TRUE
  )
  tryCatch(
    strwrap2_ctl(
      "goodbye moon", 15, indent=5, prefix='> hello >', wrap.always=TRUE
    ),
    error=conditionMessage
  )
  # Byte encoded strings not allowed

  bytes <- "\xC0\xB1\xF0\xB1\xC0\xB1\xC0\xB1"
  Encoding(bytes) <- "bytes"
  tce(strwrap_ctl(bytes))
})

# Things to test:
#
# * Special/control characters
