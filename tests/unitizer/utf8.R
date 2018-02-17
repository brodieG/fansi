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
  substr2_esc(lor.cn.c.1.5, starts, ends, term.cap=term.cap)

  # These are all six chars wide, but look different due to different width
  # characters

  lorem.cn.col.2 <- paste0(
    red, lorem.cn.pieces[1], "hello", inv, lorem.cn.pieces[2], " there ",
    grn.bg, lorem.cn.pieces[3], rgb.und, lorem.cn.pieces[4], end
  )
  lor.cn.c.2.5 <- rep(lorem.cn.col.2, 5)
  starts <- seq(1, by=6, len=5)
  ends <- starts + 5
  substr2_esc(lor.cn.c.2.5, starts, ends, term.cap=term.cap)

  starts <- seq(1, by=12, len=5)
  ends <- starts + 11
  substr2_esc(lor.cn.c.2.5, starts, ends, type='width', term.cap=term.cap)

  # with colors that actually work on an OSX terminal

  lorem.cn.col.4 <- paste0(
    red, lorem.cn.pieces[1], "hello", inv, lorem.cn.pieces[2], " there ",
    grn.bg, lorem.cn.pieces[3], rgb.und.256, lorem.cn.pieces[4], end
  )
  lor.cn.c.4.5 <- rep(lorem.cn.col.4, 5)
  substr2_esc(lor.cn.c.4.5, starts, ends, type='width')

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

  substr2_esc(lor.cn.c.3.5, starts, ends, type='width', term.cap=term.cap)

  # and now we grab those missing chars by allowing the round to happen

  substr2_esc(
    lor.cn.c.3.5, starts, ends, type='width', round='both', term.cap=term.cap
  )

  # jagged first one leads short, second long

  starts <- seq(1, by=7, length.out=5)
  ends <- starts + 8
  substr2_esc(lor.cn.c.1.5, starts, ends, type='width', term.cap=term.cap)
  substr2_esc(
    lor.cn.c.1.5, starts, ends, type='width', round='stop', term.cap=term.cap
  )

})
unitizer_sect("rounding", {
  # handling of subsetting when we end up in middle of wide display characters

  substr2_esc(lorem.cn.col.2, 1, 2, type='width')
  substr2_esc(lorem.cn.col.2, 1, 3, type='width')
  substr2_esc(lorem.cn.col.2, 2, 3, type='width')
  substr2_esc(lorem.cn.col.2, 2, 4, type='width')
  substr2_esc(lorem.cn.col.2, 3, 4, type='width')

  substr2_esc(lorem.cn.col.2, 1, 2, type='width', round='stop')
  substr2_esc(lorem.cn.col.2, 1, 3, type='width', round='stop')
  substr2_esc(lorem.cn.col.2, 2, 3, type='width', round='stop')
  substr2_esc(lorem.cn.col.2, 2, 4, type='width', round='stop')
  substr2_esc(lorem.cn.col.2, 3, 4, type='width', round='stop')

  substr2_esc(lorem.cn.col.2, 1, 2, type='width', round='both')
  substr2_esc(lorem.cn.col.2, 1, 3, type='width', round='both')
  substr2_esc(lorem.cn.col.2, 2, 3, type='width', round='both')
  substr2_esc(lorem.cn.col.2, 2, 4, type='width', round='both')
  substr2_esc(lorem.cn.col.2, 3, 4, type='width', round='both')

  substr2_esc(lorem.cn.col.2, 1, 2, type='width', round='neither')
  substr2_esc(lorem.cn.col.2, 1, 3, type='width', round='neither')
  substr2_esc(lorem.cn.col.2, 2, 3, type='width', round='neither')
  substr2_esc(lorem.cn.col.2, 2, 4, type='width', round='neither')
  substr2_esc(lorem.cn.col.2, 3, 4, type='width', round='neither')
})
unitizer_sect("multi-elem", {
  # Due to preservation of state issues, need to make sure works well with
  # more than one value

  lor.cn.2.2 <- rep(lorem.cn.col.2, 2)

  substr2_esc(lor.cn.2.2, c(1,3), c(2,4), type='width')
  substr2_esc(lor.cn.2.2, c(2,4), c(2,4), type='width')
})
unitizer_sect("zero width combining", {
  combo <- "hello\u0300\u035c world"
  Encoding(combo) <- "UTF-8"

  substr2_esc(combo, 1, 5, type='width')
  substr2_esc(combo, 5, 8, type='width')
  substr2_esc(rep(combo, 2), c(1, 5), c(5, 8), type='width')

  combo1 <- "hello\u0300\u035c"
  Encoding(combo1) <- "UTF-8"

  substr2_esc(combo, 1, 5, type='width')
  substr2_esc(combo, 2, 6, type='width')

  # zero width with double width

  combo3 <- paste0(substr(lorem.cn.pieces[1], 1, 2), '\u0300')
  Encoding(combo3) <- "UTF-8"
  substr2_esc(combo3, 3, 4, type='width')
  substr2_esc(combo3, 2, 4, type='width')
  substr2_esc(combo3, 4, 4, type='width')
  substr2_esc(combo3, 4, 5, type='width')

  # start with diacritic

  combo4 <- paste0('\u0300hello')
  substr2_esc(combo4, 1, 1, type='width')  # no diacritic
  substr2_esc(combo4, 1, 1)                # diacritic only
})
unitizer_sect("Emoji combining", {
  flags <- "\U0001f1e6\U0001f1f7\U0001f1e6\U0001f1f4\U0001f1e6\U0001f1ee"

  nchar(flags, type='chars')
  nchar(flags, type='width')

  substr2_esc(flags, 1, 2)
  substr2_esc(flags, 1, 2, type='width')
})
