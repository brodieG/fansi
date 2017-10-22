#' Strip Ansi Escape Sequences
#'
#' ...explain exactly what gets striped...
#'
#' @param x character vector
#' @return character vector of same length as x with ANSI escape sequences
#'   stripped
#' @export

strip_ansi <- function(x) .Call(FANSI_strip_csi, x)


