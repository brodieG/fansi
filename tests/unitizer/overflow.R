library(fansi)

## remember that when reviewing these int_max will be reset to
## the original value

old_max <- fansi:::set_int_max(15)

unitizer_sect('tabs', {
  tabs_as_spaces("\t1234567")
  tryCatch(tabs_as_spaces("\t12345678"), error=conditionMessage)

  # we're trying to trigger the failover allocation mode for
  # FANSI_size_buff, where double the requested size is over the max size

  invisible(fansi:::set_int_max(12))
  tabs_as_spaces(c("\t", "\t123"))
})
unitizer_sect('wrap', {
  invisible(fansi:::set_int_max(15))
  string <- '0123456789'
  strwrap_ctl(string, 16)
  strwrap2_ctl(string, 16, pad.end=' ')
  tce(strwrap2_ctl(string, 17, pad.end=' '))
  strwrap_ctl(string, 16, prefix='-----')
  tce(strwrap_ctl(string, 16, prefix='------'))
  strwrap_ctl(string, 16, indent=5)
  tce(strwrap_ctl(string, 16, indent=6))
  strwrap_ctl(string, 16, indent=2, prefix='---')
  tce(strwrap_ctl(string, 16, indent=3, prefix='---'))

  string2 <- '012345678901234'
  string3 <- '0123456789012345'
  strwrap_ctl(string2, 16)
  tce(strwrap_ctl(string3, 16))

  string4 <- '\033[31m0123456789'
  tce(strwrap_ctl(string4, 16))
})
unitizer_sect('html', {
  invisible(fansi:::set_int_max(37))

  sgr_to_html("\033[31m")
  # whole string over
  tce(sgr_to_html("\033[31ma"))
  # Sequences alone over
  tce(sgr_to_html("\033[31m\033[42mhello"))
})
unitizer_sect('unhandled', {
  invisible(fansi:::set_int_max(10))
  string <- paste0(rep("\a", 10), collapse="")
  unhandled_ctl(string)
  tcw(unhandled_ctl(c('\a', string)))
  suppressWarnings(unhandled_ctl(c('\a', string)))
})

new_max <- fansi:::set_int_max(old_max)
