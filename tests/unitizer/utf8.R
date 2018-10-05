# Test UTF-8, separate to avoid issues with platforms that don't support it

library(fansi)

unitizer_sect("substr", {
  term.cap <- c('bright', '256', 'truecolor')
  lorem.cn.pieces <-
    substr(rep(lorem.cn, 5), c(1, 11, 21, 31), c(10, 15, 22, 45))

  lorem.cn.col.1 <- paste0(
    red, lorem.cn.pieces[1], inv, lorem.cn.pieces[2], grn.bg,
    lorem.cn.pieces[3], rgb.und, lorem.cn.pieces[4], end
  )
  lor.cn.c.1.5 <- rep(lorem.cn.col.1, 5)

  starts <- seq(1, 17, 4)
  ends <- starts + 3
  substr2_ctl(lor.cn.c.1.5, starts, ends, term.cap=term.cap)

  # These are all six chars wide, but look different due to different width
  # characters

  lorem.cn.col.2 <- paste0(
    red, lorem.cn.pieces[1], "hello", inv, lorem.cn.pieces[2], " there ",
    grn.bg, lorem.cn.pieces[3], rgb.und, lorem.cn.pieces[4], end
  )
  lor.cn.c.2.5 <- rep(lorem.cn.col.2, 5)
  starts <- seq(1, by=6, len=5)
  ends <- starts + 5
  substr2_ctl(lor.cn.c.2.5, starts, ends, term.cap=term.cap)
  substr2_sgr(lor.cn.c.2.5, starts, ends, term.cap=term.cap)

  starts <- seq(1, by=12, len=5)
  ends <- starts + 11
  substr2_ctl(lor.cn.c.2.5, starts, ends, type='width', term.cap=term.cap)

  # with colors that actually work on an OSX terminal

  lorem.cn.col.4 <- paste0(
    red, lorem.cn.pieces[1], "hello", inv, lorem.cn.pieces[2], " there ",
    grn.bg, lorem.cn.pieces[3], rgb.und.256, lorem.cn.pieces[4], end
  )
  lor.cn.c.4.5 <- rep(lorem.cn.col.4, 5)
  substr2_ctl(lor.cn.c.4.5, starts, ends, type='width')

  # All wide characters even number of chars apart

  lorem.cn.col.3 <- paste0(
    red, lorem.cn.pieces[1], "helloo", inv, lorem.cn.pieces[2], " world! ",
    grn.bg, lorem.cn.pieces[3], rgb.und, lorem.cn.pieces[4], end
  )
  lor.cn.c.3.5 <- rep(lorem.cn.col.3, 5)

  starts <- seq(1, by=12, len=5)
  ends <- starts + 10
  ends[2] <- 24

  # This is a bit of an accidental one, but it should be the case that the
  # second line has two extra single width characters because all the others are
  # losing the last character b/c we're ending in the middle, width wise

  substr2_ctl(lor.cn.c.3.5, starts, ends, type='width', term.cap=term.cap)

  # and now we grab those missing chars by allowing the round to happen

  substr2_ctl(
    lor.cn.c.3.5, starts, ends, type='width', round='both', term.cap=term.cap
  )

  # jagged first one leads short, second long

  starts <- seq(1, by=7, length.out=5)
  ends <- starts + 8
  substr2_ctl(lor.cn.c.1.5, starts, ends, type='width', term.cap=term.cap)
  substr2_ctl(
    lor.cn.c.1.5, starts, ends, type='width', round='stop', term.cap=term.cap
  )
  # don't support byte encoded strings

  bytes <- "\xC0\xB1\xF0\xB1\xC0\xB1\xC0\xB1"
  Encoding(bytes) <- "bytes"

  # need trycatch due to instability from C level `error` call in getting the
  # function call

  tce(substr_ctl(bytes, 2, 3))

  # Let's try a latin one string

  latin <- "H\xE9llo W\xD6rld!"
  Encoding(latin) <- "latin1"

  latin.utf8 <- substr_ctl(latin, 1, 9)
  latin.utf8
  Encoding(latin.utf8)
})
unitizer_sect("rounding", {
  # handling of subsetting when we end up in middle of wide display characters

  substr2_ctl(lorem.cn.col.2, 1, 2, type='width')
  substr2_ctl(lorem.cn.col.2, 1, 3, type='width')
  substr2_ctl(lorem.cn.col.2, 2, 3, type='width')
  substr2_ctl(lorem.cn.col.2, 2, 4, type='width')
  substr2_ctl(lorem.cn.col.2, 3, 4, type='width')

  substr2_ctl(lorem.cn.col.2, 1, 2, type='width', round='stop')
  substr2_ctl(lorem.cn.col.2, 1, 3, type='width', round='stop')
  substr2_ctl(lorem.cn.col.2, 2, 3, type='width', round='stop')
  substr2_ctl(lorem.cn.col.2, 2, 4, type='width', round='stop')
  substr2_ctl(lorem.cn.col.2, 3, 4, type='width', round='stop')

  substr2_ctl(lorem.cn.col.2, 1, 2, type='width', round='both')
  substr2_ctl(lorem.cn.col.2, 1, 3, type='width', round='both')
  substr2_ctl(lorem.cn.col.2, 2, 3, type='width', round='both')
  substr2_ctl(lorem.cn.col.2, 2, 4, type='width', round='both')
  substr2_ctl(lorem.cn.col.2, 3, 4, type='width', round='both')

  substr2_ctl(lorem.cn.col.2, 1, 2, type='width', round='neither')
  substr2_ctl(lorem.cn.col.2, 1, 3, type='width', round='neither')
  substr2_ctl(lorem.cn.col.2, 2, 3, type='width', round='neither')
  substr2_ctl(lorem.cn.col.2, 2, 4, type='width', round='neither')
  substr2_ctl(lorem.cn.col.2, 3, 4, type='width', round='neither')
})
unitizer_sect("multi-elem", {
  # Due to preservation of state issues, need to make sure works well with
  # more than one value

  lor.cn.2.2 <- rep(lorem.cn.col.2, 2)

  substr2_ctl(lor.cn.2.2, c(1,3), c(2,4), type='width')
  substr2_ctl(lor.cn.2.2, c(2,4), c(2,4), type='width')
})
unitizer_sect("zero width combining", {
  combo <- "hello\u0300\u035c world"
  Encoding(combo) <- "UTF-8"

  substr2_ctl(combo, 1, 5, type='width')
  substr2_ctl(combo, 5, 8, type='width')
  substr2_ctl(rep(combo, 2), c(1, 5), c(5, 8), type='width')

  combo1 <- "hello\u0300\u035c"
  Encoding(combo1) <- "UTF-8"

  substr2_ctl(combo, 1, 5, type='width')
  substr2_ctl(combo, 2, 6, type='width')

  # zero width with double width

  combo3 <- paste0(substr(lorem.cn.pieces[1], 1, 2), '\u0300')
  Encoding(combo3) <- "UTF-8"
  substr2_ctl(combo3, 3, 4, type='width')
  substr2_ctl(combo3, 2, 4, type='width')
  substr2_ctl(combo3, 4, 4, type='width')
  substr2_ctl(combo3, 4, 5, type='width')

  # start with diacritic

  combo4 <- paste0('\u0300hello')
  substr2_ctl(combo4, 1, 1, type='width')  # no diacritic
  substr2_ctl(combo4, 1, 1)                # diacritic only
})
unitizer_sect("Emoji combining", {
  flags <- "\U0001f1e6\U0001f1f7\U0001f1e6\U0001f1f4\U0001f1e6\U0001f1ee"

  nchar(flags, type='chars')
  nchar(flags, type='width')

  nchar_ctl(flags, type='chars')
  nchar_ctl(flags, type='width')

  # can't lest stuff above BMP output as windows can get messed up by that,
  # otherwise we wouldn't have to use nchar below

  nchar(substr2_ctl(flags, 1, 2))
  nchar(substr2_ctl(flags, 1, 2, type='width'))
})
unitizer_sect("Corner cases", {
  utf8.bad <- "hello \xF0 world, goodnight moon"
  Encoding(utf8.bad) <- 'UTF-8'

  # # have to remove these because of the change in substr behavior, use
  # # state_at_pos instead
  # substr_ctl(utf8.bad, 1, 7)
  # identical(substr_ctl(utf8.bad, 1, 7), substr(utf8.bad, 1, 7))
  # substr_ctl(utf8.bad, 5, 10)

  fansi:::state_at_pos(utf8.bad, 1, 7)
  fansi:::state_at_pos(utf8.bad, 5, 10)
  
  # Need to use `tryCatch` because the warnings vascillate for no rhyme or
  # reason between showing the call and not.  Seems to be triggered by
  # re-installing package. now we're stuff with the try business to circumvent
  # that variability.

  tryCatch(
    substr2_ctl(utf8.bad, 1, 7, type='width'),
    warning=function(e) conditionMessage(e)
  )
  # # need to remove for changes in R3.6.0
  # substr2_ctl(utf8.bad, 1, 7, type='width', warn=FALSE)
  tryCatch(
    substr2_ctl(utf8.bad, 5, 10, type='width'),
    warning=function(e) conditionMessage(e)
  )
  # # need to remove for changes in R3.6.0
  # substr2_ctl(utf8.bad, 5, 10, type='width', warn=FALSE)
  # ends early

  chrs.2 <- "hello\xee"
  Encoding(chrs.2) <- "UTF-8"
  tryCatch(
    substr2_ctl(chrs.2, 1, 10, type='width'),
    warning=function(e) conditionMessage(e)
  )
  # # need to remove for changes in R3.6.0
  # substr2_ctl(chrs.2, 1, 10, type='width', warn=FALSE)

  # boundaries

  b.test <- c(
    "\uc0f6\ubed9",
    "\u0301a\ubed9",  # leading diacritic
    "\ubed9\u0301a",  # trailing diacritic
    "\ubed9a\u0301"   # really trailing diacritic
  )
  identical(substr_ctl(b.test, 0, 3), substr(b.test, 0, 3))
  identical(substr_ctl(b.test, 0, 2), substr(b.test, 0, 2))
  identical(substr_ctl(b.test, 1, 2), substr(b.test, 1, 2))
  identical(substr_ctl(b.test, 0, 4), substr(b.test, 0, 4))
  identical(substr_ctl(b.test, 4, 4), substr(b.test, 4, 4))

  b.t.c <- sprintf("\033[43m%s\033[49m", b.test)

  substr_ctl(b.t.c, 0, 0)
  substr_ctl(b.t.c, 0, 2)
  substr_ctl(b.t.c, 1, 2)
  substr_ctl(b.t.c, 0, 4)
  substr_ctl(b.t.c, 4, 4)

  substr2_ctl(b.t.c, 0, 0, type='width')
  substr2_ctl(b.t.c, 0, 2, type='width')
  substr2_ctl(b.t.c, 1, 4, type='width')
  substr2_ctl(b.t.c, 0, 5, type='width')
  substr2_ctl(b.t.c, 5, 5, type='width')
})
unitizer_sect("nchar", {
  chr.dia <- 'A\u030A'
  nchar_ctl(chr.dia)
  nchar(chr.dia)  # for reference, base gets it wrong too
  nchar_ctl(chr.dia, type='width')

  # Wide chars

  w1 <- "\u4E00\u4E01\u4E03"
  w2 <- "\u4E00\u4E01\u4E03"
  nchar_ctl(w1)
  nchar_ctl(w2, type='width')

  # Allow NA for illegal sequences

  hello.illegal <- c("hello", "\xF0", "\xF0aaaa")
  Encoding(hello.illegal) <- 'UTF-8'

  nchar_ctl(hello.illegal)
  nchar_ctl(hello.illegal, allowNA=TRUE)

  # nzchar doesn't care about multi-byte illegal

  nzchar_ctl(hello.illegal)

  # escapes mixed in

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
  nchar_ctl(c(esc.1, esc.2, 'hello'), warn=FALSE)

  # _sgr

  esc.4 <- c(sprintf("\033[31m%s\thello", w1), NA, hello.illegal)
  nchar_sgr(esc.4, type='width', keepNA=FALSE, warn=FALSE, allowNA=TRUE)
  nzchar_sgr(esc.4, keepNA=FALSE, warn=FALSE)
})
unitizer_sect("unhandled", {
  # a bad utf8 string and other bad stuff

  utf8.bad.0 <- "hello\033\033\033[45p \xF0how wor\ald"
  Encoding(utf8.bad.0) <- "UTF-8"
  unhandled_ctl(utf8.bad.0)
  utf8.bad.1 <- "hello \xF0ho"
  Encoding(utf8.bad.1) <- "UTF-8"
  unhandled_ctl(utf8.bad.1)
})
unitizer_sect("utf8clen", {
  # Can't test directly, but we can check what character lenght we get back from
  # nchar and infer whether things have changed or not
  #
  # These tests are designed to start failing if behavior of utf8clen changes
  #
  # See src/main/valid_utf8.h

  # U+0000..U+007F     | 00..7F |
  # U+0080..U+07FF     | C2..DF | 80..BF
  # U+0800..U+0FFF     | E0     |*A0..BF*| 80..BF
  # U+1000..U+CFFF     | E1..EC | 80..BF | 80..BF
  # U+D000..U+D7FF     | ED     |*80..9F*| 80..BF
  # U+E000..U+FFFF     | EE..EF | 80..BF | 80..BF
  # U+10000..U+3FFFF   | F0     |*90..BF*| 80..BF | 80..BF
  # U+40000..U+FFFFF   | F1..F3 | 80..BF | 80..BF | 80..BF
  # U+100000..U+10FFFF | F4     |*80..8F*| 80..BF | 80..BF

  chrs <- c(
    "\xc2\x80", "\xDF\xBF",
    "\xe0\xA0\x80", "\xE0\xBF\xBF",
    "\xe1\x80\x80", "\xeC\xbf\xbf",
    "\xed\x80\x80", "\xed\x9f\xbf",
    "\xee\x90\x80", "\xef\xbf\xbf",
    "\xf0\x90\x80\x80", "\xf4\x8f\xbf\xbf",
    "\xf8\x80\x80\x80\x80", "\xfb\x80\x80\x80\x80",
    "\xfc\x80\x80\x80\x80\x80", "\xff\x80\x80\x80\x80\x80"
  )
  Encoding(chrs) <- "UTF-8"
  nchar(chrs, allowNA=TRUE)
  nchar_ctl(chrs, allowNA=TRUE)

  # Of the 10xxxxxx variety

  utf8.bad.2 <- "\xBFaaaaaa"
  Encoding(utf8.bad.2) <- "UTF-8"
  nchar(utf8.bad.2, allowNA=TRUE)
  nchar_ctl(utf8.bad.2, allowNA=TRUE)

  ## remove for changes in R3.6.0
  # substr(utf8.bad.2, 1, 1)
  # substr_ctl(utf8.bad.2, 1, 1)

  fansi:::state_at_pos(utf8.bad.2, 1, 1)

})
unitizer_sect("wrap corner cases", {
  # With UTF8

  pre.2 <- "\x1b[32m\xd0\x9f \x1b[0m"
  ini.2 <- "\x1b[33m\xd1\x80 \x1b[0m"
  hello.8c <- "hello Привет world"

  Encoding(pre.2) <- "UTF-8"
  Encoding(ini.2) <- "UTF-8"
  Encoding(hello.8c) <- "UTF-8"

  pre.3 <- "\xd0\x9f "
  ini.3 <- "\xd1\x80 "

  Encoding(pre.3) <- "UTF-8"
  Encoding(ini.3) <- "UTF-8"

  wrap.csi.4 <- strwrap_ctl(hello.8c, 15, prefix=pre.2, initial=ini.2)
  wrap.csi.4

  # wrap.nrm.4 <- strwrap(hello.8c, 15, prefix=pre.3, initial=ini.3)
  # identical(strip_ctl(wrap.csi.4, "sgr"), wrap.nrm.4)

  utf8.chr <- "\u76F4"
  strwrap2_ctl(utf8.chr, 1, wrap.always=TRUE)
  strwrap2_ctl(utf8.chr, 2, wrap.always=TRUE)
  strwrap2_ctl(utf8.chr, 3, wrap.always=TRUE)

  strwrap_ctl("lovelyday.", 10)
  strwrap2_ctl("lovelyday.", 10, wrap.always=TRUE)

  utf8.bad <- "hello \xF0 world, goodnight moon"
  Encoding(utf8.bad) <- "UTF-8"
  strwrap_ctl(utf8.bad, 10)

  # bad prefix values

  utf8.bad.2 <- "\xF0"
  Encoding(utf8.bad.2) <- "UTF-8"

  tcw(strwrap_ctl("hello world", 6, prefix=utf8.bad.2))
  suppressWarnings(strwrap_ctl("hello world", 6, prefix=utf8.bad.2))
  #
  # Byte encoded strings not allowed

  bytes <- "\xC0\xB1\xF0\xB1\xC0\xB1\xC0\xB1"
  Encoding(bytes) <- "bytes"
  tce(strwrap_ctl(bytes))
})
unitizer_sect("wrap with wide UTF8 and ESC", {
  wrap.mix <- strwrap_ctl(lorem.mix, 25)
  wrap.mix
  # identical(
  #   strwrap(strip_ctl(lorem.mix, "sgr"), 25), strip_ctl(wrap.mix, "sgr")
  # )

  string <- "\033[37;48;5;32m國官方認定的民族現有56個\033[39;49m"
  Encoding(string) <- "UTF-8"
  strwrap2_ctl(string, 24, wrap.always=TRUE, pad.end=" ")
})
unitizer_sect("issue 54 ctd", {
  # other issu54 tests are in tohtml.R, but had to move this one here due to the
  # ellipsis utf-8 character.

  string3 <- c(
    "\033[38;5;246m# … with 5 more variables: total_time \033[3m\033[38;5;246m<bch:tm>\033[38;5;246m\033[23m, result \033[3m\033[38;5;246m<list>\033[38;5;246m\033[23m, memory \033[3m\033[38;5;246m<list>\033[38;5;246m\033[23m,",
    "#   time \033[3m\033[38;5;246m<list>\033[38;5;246m\033[23m, gc \033[3m\033[38;5;246m<list>\033[38;5;246m\033[23m\033[39m"
  )
  Encoding(string3) <- "UTF-8"
  fansi::sgr_to_html(string3)

  # head <- "<html><head><meta charset='utf-8'/></head><pre>"
  # f <- paste0(tempfile(), ".html")
  # writeLines(c(head, fansi::sgr_to_html(string3), "</pre></html>"), f)
  # browseURL(f)
  # unlink(f)

  # trigger warnings/errors

  string4 <- c(
    "wow \033[31m then", "hello\033[\x80;wow", "yo \033[m there",
    "boom \033[41m"
  )
  Encoding(string4) <- "UTF-8"
  sgr_to_html(string4)
})
