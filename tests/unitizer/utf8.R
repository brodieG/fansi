# Test UTF-8, separate to avoid issues with platforms that don't support it

library(fansi)

unitizer_sect("substr", {
  lorem.cn.pieces <-
    substr(rep(lorem.cn, 5), c(1, 11, 21, 31), c(10, 15, 22, 45))

  lorem.cn.col.1 <- paste0(
    red, lorem.cn.pieces[1], inv, lorem.cn.pieces[2], grn.bg,
    lorem.cn.pieces[3], rgb.und, lorem.cn.pieces[4], end
  )
  starts <- seq(1, 17, 4)
  ends <- starts + 3
  ansi_substr2(rep(lorem.cn.col.1, 5), starts, ends)

  # These are all six chars wide, but look different due to different width
  # characters

  lorem.cn.col.2 <- paste0(
    red, lorem.cn.pieces[1], "hello", inv, lorem.cn.pieces[2], " there ",
    grn.bg, lorem.cn.pieces[3], rgb.und, lorem.cn.pieces[4], end
  )
  starts <- seq(1, by=6, len=5)
  ends <- starts + 5
  ansi_substr2(rep(lorem.cn.col.2, 5), starts, ends)

  starts <- seq(1, by=12, len=5)
  ends <- starts + 11
  ansi_substr2(rep(lorem.cn.col.2, 5), starts, ends, type='width')

  # All wide characters even number of chars apart

  lorem.cn.col.3 <- paste0(
    red, lorem.cn.pieces[1], "hellso", inv, lorem.cn.pieces[2], " therre ",
    grn.bg, lorem.cn.pieces[3], rgb.und, lorem.cn.pieces[4], end
  )

  starts <- seq(1, by=12, len=5)
  ends <- starts + 10
  ends[2] <- 24 # because we end in a single width char on this line
  ansi_substr2(rep(lorem.cn.col.3, 5), starts, ends, type='width')
})
