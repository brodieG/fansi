
check_assumptions <- function() .Call(FANSI_check_assumptions)  # nocov

digits_in_int <- function(x) .Call(FANSI_digits_in_int, x)

#' Replace Tabs With Corresponding Spaces
#'
#' Finds horizontal tab characters (0x09) in a string and replaces them with the
#' spaces that produce the same horizontal offset.
#'
#' Since we do not know of a reliable cross platform means of detecting tab
#' stops you will need to provide them yourself if you are using anything
#' outside of the standard tab stop every 8 characters that is the default.
#'
#' @seealso [string-parsing] for important details on how strings are
#'   interpreted
#' @export
#' @param x character vector to replace tabs in.
#' @param tab.stops integer distance between tab stops, the last tab stop
#'   repeats as necessary.
#' @return character, `x` with tabs replaced by spaces, with elements
#'   possibly converted to UTF-8.
#' @examples
#' tabs_as_spaces('1\t1234\t12345678\t123456789\t')
#' tabs_as_spaces('1\t1234\t12345678\t123456789\t', tab.stops=c(2,8))

tabs_as_spaces <- function(x, tab.stops=getOption('fansi.tab.stops')) {
  vetr(
    character(),
    tab.stops=INT.POS.STR && length(.) > 0
  )
  .Call(FANSI_tabs_as_spaces, x, as.integer(tab.stops))
}
