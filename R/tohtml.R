## Copyright (C) 2018  Brodie Gaslam
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
#' @note Non-ASCII strings are converted to and returned in UTF-8 encoding.
#' @export
#' @inheritParams substr_ctl
#' @seealso [fansi] for details on how _Control Sequences_ are
#'   interpreted, particularly if you are getting unexpected results.
#' @return a character vector with all escape sequences removed and any basic
#'   ANSI CSI SGR escape sequences applied via SPAN html objects with
#'   inline css styles.
#' @examples
#' sgr_to_html("hello\033[31;42;1mworld\033[m")

sgr_to_html <- function(
  x, warn=getOption('fansi.warn'), term.cap=getOption('fansi.term.cap')
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

  .Call(FANSI_esc_to_html, enc2utf8(x), warn, term.cap.int)
}

