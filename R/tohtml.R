## Copyright (C) 2020  Brodie Gaslam
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

#' Convert ANSI CSI SGR Escape Sequence to HTML Equivalents
#'
#' Only the colors, background-colors, and basic styles (CSI SGR codes 1-9) are
#' translated.  Others are dropped silently.
#'
#' If `class.prefix` is specified as a string, then HTML output affected by
#' color or background color CSI SGR sequences  will be tagged respectively with
#' classes of form "<prefix>-color-##" and "<prefix>-bgcol-##".  "<prefix>" is
#' the value of `class.prefix` and "##" is a two digit number in 00-15, where
#' 00-07 are the standard colors (i.e. CSI SGR codes 30-37 or 40-47), and 08-15
#' are the bright colors (i.e. CSI SGR codes 90-97 or 100-107).  Colors
#' specified either by the 256 or true color schemes (e.g. those starting with
#' code 38 or 48) will always be specified as inline styles.
#'
#' @note Non-ASCII strings are converted to and returned in UTF-8 encoding.
#' @export
#' @inheritParams substr_ctl
#' @seealso [fansi] for details on how _Control Sequences_ are
#'   interpreted, particularly if you are getting unexpected results,
#'   [set_knit_hooks()] for how to use ANSI CSI styled text with knitr and HTML
#'   output.
#' @param fansi.class.prefix character(1L) specify a non empty string to cause
#'   colors and background-colors to be specified via classes instead of inline
#'   styles (see details).
#' @return a character vector with all escape sequences removed and any basic
#'   ANSI CSI SGR escape sequences applied via SPAN html objects with
#'   inline css styles (see details).
#' @examples
#' sgr_to_html("hello\033[31;42;1mworld\033[m")
#' sgr_to_html("hello\033[31;42;1mworld\033[m", class.prefix='fansi')

sgr_to_html <- function(
  x, warn=getOption('fansi.warn'),
  term.cap=getOption('fansi.term.cap'),
  class.prefix=getOption('fansi.class.prefix', '')
) {
  if(!is.character(x)) x <- as.character(x)
  if(!is.logical(warn)) warn <- as.logical(warn)
  if(length(warn) != 1L || is.na(warn))
    stop("Argument `warn` must be TRUE or FALSE.")

  if(!is.character(term.cap))
    stop("Argument `term.cap` must be character.")
  if(anyNA(term.cap.int <- match(term.cap, VALID.TERM.CAP)))
    stop(
      "Argument `term.cap` may only contain values in ",
      deparse(VALID.TERM.CAP)
    )
  if(
    !is.character(class.prefix) || length(class.prefix) != 1L ||
    is.na(class.prefix)
  )
    stop("Argument `class.prefix` must be scalar character and not NA.")

  .Call(FANSI_esc_to_html, enc2utf8(x), warn, term.cap.int, class.prefix)
}

