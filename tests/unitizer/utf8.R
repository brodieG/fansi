# Test UTF-8, separate to avoid issues with platforms that don't support it

library(fansi)

unitizer_sect("substr", {
  lorem.cn.pieces <-
    substr(rep(lorem.cn, 5), c(1, 11, 21, 31), c(10, 15, 22, 45))

  lorem.cn.col.1 <- paste0(
    red, lorem.cn.pieces[1], inv, lorem.cn.pieces[2], grn.bg,
    lorem.cn.pieces[3], rgb.und, lorem.cn.pieces[4], end
  )
  lor.cn.c.1.5 <- rep(lorem.cn.col.1, 5)

  starts <- seq(1, 17, 4)
  ends <- starts + 3
  ansi_substr2(lor.cn.c.1.5, starts, ends)

  # These are all six chars wide, but look different due to different width
  # characters

  lorem.cn.col.2 <- paste0(
    red, lorem.cn.pieces[1], "hello", inv, lorem.cn.pieces[2], " there ",
    grn.bg, lorem.cn.pieces[3], rgb.und, lorem.cn.pieces[4], end
  )
  lor.cn.c.2.5 <- rep(lorem.cn.col.2, 5)
  starts <- seq(1, by=6, len=5)
  ends <- starts + 5
  ansi_substr2(lor.cn.c.2.5, starts, ends)

  starts <- seq(1, by=12, len=5)
  ends <- starts + 11
  ansi_substr2(lor.cn.c.2.5, starts, ends, type='width')

  # with colors that actually work on an OSX terminal

  lorem.cn.col.4 <- paste0(
    red, lorem.cn.pieces[1], "hello", inv, lorem.cn.pieces[2], " there ",
    grn.bg, lorem.cn.pieces[3], rgb.und.256, lorem.cn.pieces[4], end
  )
  lor.cn.c.4.5 <- rep(lorem.cn.col.4, 5)
  ansi_substr2(lor.cn.c.4.5, starts, ends, type='width')

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

  ansi_substr2(lor.cn.c.3.5, starts, ends, type='width')

  # and now we grab those missing chars by allowing the round to happen

  ansi_substr2(lor.cn.c.3.5, starts, ends, type='width', round='both')

  # jagged first one leads short, second long

  starts <- seq(1, by=7, length.out=5)
  ends <- starts + 8
  ansi_substr2(lor.cn.c.1.5, starts, ends, type='width')
  ansi_substr2(lor.cn.c.1.5, starts, ends, type='width', round='last')

})
unitizer_sect("rounding", {
  # handling of subsetting when we end up in middle of wide display characters

  ansi_substr2(lorem.cn.col.2, 1, 2, type='width')
  ansi_substr2(lorem.cn.col.2, 1, 3, type='width')
  ansi_substr2(lorem.cn.col.2, 2, 3, type='width')
  ansi_substr2(lorem.cn.col.2, 2, 4, type='width')
  ansi_substr2(lorem.cn.col.2, 3, 4, type='width')

  ansi_substr2(lorem.cn.col.2, 1, 2, type='width', round='last')
  ansi_substr2(lorem.cn.col.2, 1, 3, type='width', round='last')
  ansi_substr2(lorem.cn.col.2, 2, 3, type='width', round='last')
  ansi_substr2(lorem.cn.col.2, 2, 4, type='width', round='last')
  ansi_substr2(lorem.cn.col.2, 3, 4, type='width', round='last')

  ansi_substr2(lorem.cn.col.2, 1, 2, type='width', round='both')
  ansi_substr2(lorem.cn.col.2, 1, 3, type='width', round='both')
  ansi_substr2(lorem.cn.col.2, 2, 3, type='width', round='both')
  ansi_substr2(lorem.cn.col.2, 2, 4, type='width', round='both')
  ansi_substr2(lorem.cn.col.2, 3, 4, type='width', round='both')

  ansi_substr2(lorem.cn.col.2, 1, 2, type='width', round='neither')
  ansi_substr2(lorem.cn.col.2, 1, 3, type='width', round='neither')
  ansi_substr2(lorem.cn.col.2, 2, 3, type='width', round='neither')
  ansi_substr2(lorem.cn.col.2, 2, 4, type='width', round='neither')
  ansi_substr2(lorem.cn.col.2, 3, 4, type='width', round='neither')
})
unitizer_sect("multi-elem", {
  # Due to preservation of state issues, need to make sure works well with
  # more than one value

  lor.cn.2.2 <- rep(lorem.cn.col.2, 2)

  ansi_substr2(lor.cn.2.2, c(1,3), c(2,4), type='width')
  ansi_substr2(lor.cn.2.2, c(2,4), c(2,4), type='width')
})
