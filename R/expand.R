## Copyright (C) 2021  Brodie Gaslam
##
## This file is part of "fansi - ANSI Control Sequence Aware String Functions"
##
## This program is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 2 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## Go to <https://www.r-project.org/Licenses/GPL-2> for a copy of the license.

#' Expand CSI SGR Sequences
#'
#' Converts compound SGR sequences into their component sequences for better
#' compatibility with [`crayon`][1].  The closing sequence "ESC&#91;0m" (or
#' "ESC&#91;m") is expanded to match whatever styles are active at the point in
#' the string in which it appears, which includes styles accumulate from
#' previous vector elements.
#'
#' [1]: https://cran.r-project.org/package=crayon
#'
#' @export
#' @param x character vector to expand the SGR control sequences of.
#' @seealso [`fansi`] for details on how _Control Sequences_ are
#'   interpreted, particularly if you are getting unexpected results.
#' @inheritParams strip_ctl
#' @return `x`, with all SGRs expanded.
#' @examples
#' expand_sgr("hello\033[42;33m world")
#' expand_sgr("hello\033[42;33m world\033[m")
#' expand_sgr("\033[4mhello\033[42;33m world\033[m")

expand_sgr <- function(
  x, warn=getOption('fansi.warn'), term.cap=getOption('fansi.term.cap')
) {
  if(!is.logical(warn)) warn <- as.logical(warn)
  if(!is.character(x)) stop("Argument `x` should be a character vector.")
  if(!is.character(term.cap))
    stop("Argument `term.cap` must be character.")
  if(anyNA(term.cap.int <- match(term.cap, VALID.TERM.CAP)))
    stop(
      "Argument `term.cap` may only contain values in ",
      deparse(VALID.TERM.CAP)
    )

  .Call(FANSI_expand_sgr, enc2utf8(x), warn, term.cap.int)
}

