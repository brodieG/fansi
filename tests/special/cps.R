library(unitizer)
library(fansi)

unitizer_sect('Test all Code Points', {
  SURROGATE_START <- 0xD800
  SURROGATE_END <- 0xDFFF
  surrogate_range <- SURROGATE_START:SURROGATE_END
  all_codepoints <- setdiff(0:0x10FFFF, c(surrogate_range, 0x1B))
  all_chars <- intToUtf8(all_codepoints, multiple = TRUE)
  fansi_widths <- nchar_ctl(all_chars, type = 'width')
  table(fansi_widths, useNA='ifany')
})
