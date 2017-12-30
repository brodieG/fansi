#' Strip Ansi Escape Sequences
#'
#' ...explain exactly what gets striped...
#'
#' @param x character vector
#' @return character vector of same length as x with ANSI escape sequences
#'   stripped
#' @export

strip_ansi <- function(x) .Call(FANSI_strip_csi, x)

## Process String by Removing Unwanted Characters
##
## @param strip_spc remove extraneous spaces (leading, trailing, multiples,
##   although two spaces after .?! are allowed
## @param strip_tab self-explanatory
## @param strip_ctl strip all other ASCII control characters, except newlines

process <- function(
  x, strip_spc=TRUE, strip_tab=TRUE, strip_ctl=FALSE
) .Call(FANSI_process, x, strip_spc, strip_tab, strip_ctl)

