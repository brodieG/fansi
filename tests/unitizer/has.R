library(fansi)

unitizer_sect("has", {

  has_esc(paste0(red, "hello", end))
  has_esc(paste0("hello", end))
  has_esc(paste0("hello"))

  in.middle <- c("world", paste0("hello", red), "wow")
  in.end <- c("world", "wow", paste0("hello", red))
  in.start <- c(paste0("hello", red), "wow", "world")

  has_esc(in.middle)
  has_esc(in.end)
  has_esc(in.start)

  has_esc(c(in.start, NA))
})
