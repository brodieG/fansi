## Copyright (C) 2018  Brodie Gaslam
##
## This file is part of "fansi - ANSI Escape Aware String Functions"
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

#' Strip ANSI Control Sequences
#'
#' Removes _Control Sequences_ from strings.  By default it will
#' strip all known _Control Sequences_, including ANSI CSI
#' sequences, two character sequences starting with ESC, and all C0 control
#' characters, including newlines.  You can fine tune this behavior with the
#' `strip` parameter.
#'
#' The `strip` value contains the names of **non-overlapping** subsets of the
#' known _Control Sequences_ (e.g. "csi" does not contain "sgr", and "c0" does
#' not contain newlines).  The one exception is "all" which means strip every
#' known sequence.  If you combine "all" with any other option then everything
#' **but** that option will be stripped.
#'
#' @seealso [fansi] for details on how _Control Sequences_ are
#'   interpreted, particularly if you are getting unexpected results.
#' @inheritParams substr_esc
#' @export
#' @param strip character, any combination of the following values (see details):
#'   * "nl": strip newlines
#'   * "c0": strip all other "C0" control characters (i.e. x01-x1f), except for
#'     newlines and the actual ESC character
#'   * "sgr": strip ANSI CSI SGR sequences
#'   * "csi": strip all non-SGR csi sequences
#'   * "esc": strip all other escape sequences
#'   * "all": all of the above
#' @return character vector of same length as x with ANSI escape sequences
#'   stripped
#' @examples
#' string <- "hello\033k\033[45p world\n\033[31mgoodbye\a moon"
#' strip_esc(string)
#' strip_esc(string, c("nl", "c0", "sgr", "csi", "esc")) # equivalently
#' strip_esc(string, "sgr")
#' strip_esc(string, c("c0", "esc"))
#' ## everything but C0 controls (recall "nl" is not part of "c0")
#' ## as far as the `strip` argument is concerned
#' strip_esc(string, c("all", "nl", "c0"))
#'
#' ## convenience function, same as `strip_esc(strip='sgr')`
#' strip_sgr(string)

strip_esc <- function(x, strip='all', warn=getOption('fansi.warn')) {
  vetr(warn=LGL.1, strip=CHR)
  if(length(strip)) {
    if(anyNA(strip.int <- match(strip, VALID.STRIP)))
      stop(
        "Argument `strip` may contain only values in `",
        deparse(VALID.STRIP), "`"
      )
    .Call(FANSI_strip_csi, x, strip.int, warn)
  } else x
}
#' @export
#' @rdname strip_esc

strip_sgr <- function(x, warn=getOption('fansi.warn')) {
  vetr(warn=LGL.1)
  strip.int <- match("sgr", VALID.STRIP)
  if(anyNA(strip.int)) stop("Internal Error: invalid strip type")
  .Call(FANSI_strip_csi, x, strip.int, warn)
}

## Process String by Removing Unwanted Characters
##
## This is to simulate what `strwrap` does, exposed for testing purposes.

process <- function(x) .Call(FANSI_process, x)

