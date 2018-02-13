library(unitizer)
library(fansi)

unitizer_sect('simple tabs', {
  string <- '1\t12\t123\t1234\t12345678'
  tabs_as_spaces(string)
  tabs_as_spaces(string, c(2, 3, 4, 5, 8))
  tabs_as_spaces(string, c(2, 8))
})
unitizer_sect('newlines', {
  string.n <- paste0(
    '1\t12\t123\t1234\t12345678\n',
    '1\t12\t123\t1234\t12345678'
  )
  tabs_as_spaces(string.n)
  tabs_as_spaces(string.n, c(2, 3, 4, 8))
  tabs_as_spaces(string.n, c(2, 8))
})
unitizer_sect('corner cases', {
  tabs_as_spaces('')
  tabs_as_spaces('\t')
  tabs_as_spaces('\n')
  tabs_as_spaces('\x80')
  tabs_as_spaces(c(string, string, string))
})