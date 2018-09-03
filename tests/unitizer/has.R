library(fansi)

unitizer_sect("has", {

  has_ctl(paste0(red, "hello", end))
  has_ctl(paste0("hello", end))
  has_ctl(paste0("hello"))

  in.middle <- c("world", paste0("hello", red), "wow")
  in.end <- c("world", "wow", paste0("hello", red))
  in.start <- c(paste0("hello", red), "wow", "world")

  has_ctl(in.middle)
  has_ctl(in.end)
  has_ctl(in.start)

  has_ctl(c(in.start, NA))

  has_ctl("hello\nworld")
  has_sgr("hello\nworld")
  has_sgr(in.end)
  # no warning from has_ctl
  has_ctl("hello\033p world")
})
unitizer_sect("corner cases", {
  tryCatch(has_ctl("hello\033[31#0mworld"), warning=conditionMessage)
  suppressWarnings(has_ctl("hello\033[31#0mworld"))
  has_ctl("hello world", ctl=c('sgr', 'sgr'))
})
unitizer_sect("select ctl", {
  has_ctl("hello\033[31mworld", ctl=c('sgr'))
  has_ctl("hello\033[31mworld", ctl=c('csi'))
  has_ctl("hello\033[31!mworld", ctl=c('csi'))
  has_ctl("hello\033[31lworld", ctl=c('csi'))
  has_ctl("hello\nworld", ctl=c('all', 'nl'))
  has_ctl("hello\nworld", ctl=c('all', 'c0'))
  has_ctl("hello\tworld", ctl=c('all', 'c0'))
  has_ctl("hello\tworld", ctl=c('c0'))
  has_ctl("hello\033pworld", ctl=c('esc'))
  has_ctl("hello\033pworld", ctl=c('all', 'esc'))
})
unitizer_sect("bad inputs", {
  has_ctl("hello world", warn=NULL)

  has_ctl("hello world", ctl=1:3)
  has_ctl("hello world", ctl="bananas")
  has_ctl("hello world", ctl=NA_character_)
  has_ctl(c("\033[31mhello",  "wo\nrld"), ctl=character())
})
unitizer_sect("deprecation", {
  has_ctl("hello world", which="sgr")
})
