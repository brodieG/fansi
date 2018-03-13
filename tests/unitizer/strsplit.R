library(fansi)

unitizer_sect("basic splits", {
  str.1 <- "hello\033[31m world"
  str.2 <- "\033[42m hello\033[m world, Goodbye Moon"
  strsplit_ctl(str.1, " ")
  strsplit_ctl(str.1, "hello")
  strsplit_ctl(str.2, ", ")
  strsplit_ctl(c(str.1, "hello world", str.2), "hello")
})
# unitizer_sect("corner cases", {
#   # strsplit_ctl("hello\033[31m world", "")
#   # strsplit_ctl("hello\033[31m world", "[", fixed=TRUE)
# })
