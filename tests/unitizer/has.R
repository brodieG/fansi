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
})
