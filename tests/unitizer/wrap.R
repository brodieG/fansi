library(unitizer)
library(fansi)

unitizer_sect("Basic wrap", {
  hello.0 <- "hello world this is a lovely day"

  identical(strwrap_esc(hello.0, width=10), strwrap(hello.0, width=10))

  hello.1 <- "hello  world  this  is.  a lovely day."
  identical(strwrap_esc(hello.1, width=10), strwrap(hello.1, width=10))

  writeLines(strwrap_esc(hello.1, width=10))
  writeLines(strwrap(hello.1, width=10))

  hello.2 <- "hello\rworld\rthis  is.  a lovely day."
  identical(strwrap(hello.2, width=10), strwrap_esc(hello.2, width=10))

  hello.3 <- "hello\rworld\nthis  is.  a lovely\n day."
  identical(strwrap(hello.3, width=10), strwrap_esc(hello.3, width=10))

  hello.4 <- "  hello  world  this  is  a lovely day."
  identical(strwrap(hello.4, width=10), strwrap_esc(hello.4, width=10))

  hello.5 <- "hello.\n\n\nworld"
  identical(strwrap(hello.5, width=10), strwrap_esc(hello.5, width=10))

  hello.5a <- "hello.\n \n \nworld"
  identical(strwrap(hello.5a, width=10), strwrap_esc(hello.5a, width=10))

  # special preserve of double space

  hello.6a <- 'hello."  there'
  identical(strwrap(hello.6a, width=40), strwrap_esc(hello.6a, width=40))

  hello.6b <- 'hello.\'  there'
  identical(strwrap(hello.6b, width=40), strwrap_esc(hello.6b, width=40))

  hello.6c <- 'hello.)  there'
  identical(strwrap(hello.6c, width=40), strwrap_esc(hello.6c, width=40))

})
unitizer_sect("Basic Ansi", {
  hello2.0 <-
    paste0("hello ", red, "world ", grn.bg, " this is a  lovely", end, "day.")
  strwrap_esc(hello2.0, 10)

  identical(
    strwrap_esc(strip_esc(hello2.0), 10), strwrap(strip_esc(hello2.0), 10)
  )
  # turn off tag generic

  hello2.1 <- paste0("hello \033[41mworld\033[m how are you today")
  hello2.2 <- paste0("hello \033[41mworld\033[0m how are you today")

  strwrap_esc(hello2.1, 15)

  # Specific turn off tags - turn off bold and faint

  hello.bold.faint <- paste0(
    "hello \033[1mbolded once upon a time\033[22m ",
    "normal \033[2mfainting in faintness oh no\033[22m normal"
  )
  strwrap_esc(hello.bold.faint, 10)

  # Specific turn off tags - blinking

  hello.blinky <- paste0(
    "hello \033[5mbliking slowly oh my\033[25m ",
    "normal \033[6mblinking quickly oh my\033[25m normal"
  )
  strwrap_esc(hello.blinky, 10)
})
unitizer_sect("Long Wrap", {
  wrap.nrm <- strwrap(strip_esc(lorem.r.thanks), 40)
  wrap.csi <- strwrap_esc(lorem.r.thanks, 40)

  identical(wrap.nrm, strip_esc(wrap.csi))
  nchar(strip_esc(wrap.csi))
  nchar(wrap.csi)
})
unitizer_sect("Other Escapes", {
  strwrap_esc("hello \033kworld yohoo", 12)
  strwrap_esc("hello \033\nworld yohoo", 12)

  # c0 escapes should be treated as zero width

  strwrap_esc("hello\x1F\x1F\x1F\x1F\x1F\x1F world yohoo", 12)
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
    strwrap_esc(lorem.para, indent=2), strwrap(lorem.para, indent=2)
  )
  identical(
    strwrap_esc(lorem.para, exdent=2), strwrap(lorem.para, exdent=2)
  )
  identical(
    strwrap_esc(lorem.para, indent=4, exdent=2),
    strwrap(lorem.para, indent=4, exdent=2)
  )
})
unitizer_sect("prefix / initial with ESC", {
  pre <- "\033[32m+ \033[0m"
  ini <- "\033[33m> \033[0m"

  hello.8a <- "hello world yohoo"

  wrap.csi.2 <- strwrap_esc(hello.8a, 14, prefix=pre, initial=ini)
  wrap.csi.2
  wrap.nrm.2 <- strwrap(hello.8a, 14, prefix="+ ", initial="> ")
  identical(strip_esc(wrap.csi.2), wrap.nrm.2)

  hello.8b <- c(hello.8a, "oh my this has 2 elements")
  wrap.csi.3 <- strwrap_esc(hello.8b, 14, prefix=pre, initial=ini)
  wrap.csi.3
  wrap.nrm.3 <- strwrap(hello.8b, 14, prefix="+ ", initial="> ")

  identical(strip_esc(wrap.csi.3), wrap.nrm.3)

  # With UTF8

  pre.2 <- "\x1b[32m\xd0\x9f \x1b[0m"
  ini.2 <- "\x1b[33m\xd1\x80 \x1b[0m"
  hello.8c <- "hello Привет world"

  Encoding(pre.2) <- "UTF-8"
  Encoding(ini.2) <- "UTF-8"
  Encoding(hello.8c) <- "UTF-8"

  wrap.csi.4 <- strwrap_esc(hello.8c, 15, prefix=pre.2, initial=ini.2)
  wrap.csi.4
  wrap.nrm.4 <- strwrap(hello.8c, 15, prefix="\xd0\x9f ", initial="\xd1\x80 ")

  identical(strip_esc(wrap.csi.4), wrap.nrm.4)
})
unitizer_sect("wrap with wide UTF8 and ESC", {
  wrap.mix <- strwrap_esc(lorem.mix, 25)
  wrap.mix
  identical(strwrap(strip_esc(lorem.mix), 25), strip_esc(wrap.mix))
})
unitizer_sect("wrap2", {
  # Examples

  hello.9a <- "hello\t\033[41mred\033[49m\tworld"

  strwrap2_esc(hello.9a, 12)
  strwrap2_esc(hello.9a, 12, tabs.as.spaces=TRUE)
  strwrap2_esc(hello.9a, 13, tabs.as.spaces=TRUE)
  strwrap2_esc(hello.9a, 12, tabs.as.spaces=TRUE, tab.stops=c(6, 12))

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
  r.wrap <- strwrap2_esc(r.col, 35, pad.end=" ", wrap.always=TRUE)
  # writeLines(c("", paste(" ", r.wrap[1:27], " ", r.wrap[28:54]), ""))
  r.wrap

  ## Pad paragraph break line when it is colored

  hello.9b <- "\033[41mhello\n\nworld."
  strwrap2_esc(hello.9b, 8, pad.end=" ")

  ## Leading spaces

  hello.9b <- "  \033[41mhello world."
  strwrap2_esc(hello.9b, 8, strip.spaces=FALSE)
  hello.9c <- "\033[41m  hello world."
})
unitizer_sect("long words", {
  hello.long <- "\033[31mhelloworld\033[mlongword"
  strwrap_esc(hello.long, 8)
  strwrap2_esc(hello.long, 8, wrap.always=TRUE)
})
unitizer_sect("rare escapes", {
  hello.border <- c(
    "hello \033[51mworld woohoo\033[54m woohoo",
    "hello \033[52mworld woohoo\033[54m woohoo",
    "hello \033[53mworld woohoo\033[55m woohoo"
  )
  strwrap_esc(hello.border, 12)
  hello.ideogram <- c(
    "hello \033[60mworld woohoo\033[65m woohoo",
    "hello \033[61mworld woohoo\033[65m woohoo",
    "hello \033[62mworld woohoo\033[65m woohoo",
    "hello \033[63mworld woohoo\033[65m woohoo",
    "hello \033[64mworld woohoo\033[65m woohoo"
  )
  strwrap_esc(hello.ideogram, 12)
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
  strwrap_esc(hello.font, 12)
})
# Things to test:
#
# * Special/control characters
