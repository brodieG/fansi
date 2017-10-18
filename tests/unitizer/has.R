library(fansi)

unitizer_sect("has", {

  has_csi(paste0(red, "hello", end))
  has_csi(paste0("hello", end))
  has_csi(paste0("hello"))

  in.middle <- c("world", paste0("hello", red), "wow")
  in.end <- c("world", "wow", paste0("hello", red))
  in.start <- c(paste0("hello", red), "wow", "world")

  has_csi(in.middle)
  has_csi(in.end)
  has_csi(in.start)

  has_csi(c(in.start, NA))
})
