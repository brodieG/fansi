
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
#' @param tab.stops integer(1:n) indicating position of tab stops to use when
#'   converting tabs to spaces.  If there are more tabs in a line than defined
#'   tab stops the last tab stop is re-used.  For the purposes of applying tab
#'   stops, each input line is considered a line and the character count begins
#'   from the beginning of the input line.  
#' @return character, `x` with tabs replaced by spaces, with elements
#'   possibly converted to UTF-8.
#' @examples
#' string <- '1\t12\t123\t1234\t12345678'
#' tabs_as_spaces(string)
#' writeLines(
#'   c(
#'     '-------|-------|-------|-------|-------|',
#'     tabs_as_spaces(string)
#' ) )
#' writeLines(
#'   c(
#'     '-|--|--|--|--|--|--|--|--|--|--|',
#'     tabs_as_spaces(string, tab.stops=c(2, 3))
#' ) )
#' writeLines(
#'   c(
#'     '-|--|-------|-------|-------|',
#'     tabs_as_spaces(string, tab.stops=c(2, 3, 8))
#' ) )

tabs_as_spaces <- function(x, tab.stops=getOption('fansi.tab.stops')) {
  vetr(
    character(),
    tab.stops=INT.POS.STR && length(.) > 0
  )
  .Call(FANSI_tabs_as_spaces, x, as.integer(tab.stops))
}
