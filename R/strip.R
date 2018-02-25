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

#' Strip Control Characters and Sequences
#'
#' Removes control characters and sequences from strings.  By default only
#' strips valid ANSI CSI SGR sequences, but can be made to strip C0 control
#' characters and all escape sequences.
#'
#' @seealso [fansi] for important details on how strings are interpreted,
#'   particularly if you are getting unexpected results.
#' @inheritParams substr_esc
#' @export
#' @param what character, any combination of the following values, where each
#'   value represents a distinct set of control characters/sequences to strip,
#'   except for "all" which is the equivalent to specifying all the others:
#'
#'   * "nl": strip newlines
#'   * "c0": strip all other "C0" control characters (i.e. x01-x1f), except for
#'     newlines and the actual ESC character
#'   * "sgr": strip ANSI CSI SGR sequences
#'   * "csi": strip all non-SGR csi sequences
#'   * "esc": strip all other escape sequences, including invalid SGR/CSI
#'     sequences
#'   * "all": all of the above
#' @return character vector of same length as x with ANSI escape sequences
#'   stripped
#' @examples
#' string <- "hello\033k\033[45p world\n\033[31mgoodbye\a moon"
#' strip_esc(string)
#' strip_esc(string, c("nl", "c0", "sgr", "csi", "esc"))
#' strip_esc(string, "all")  # equivalently
#' strip_esc(string, c("c0", "esc"))

strip_esc <- function(x, what='sgr', warn=getOption('fansi.warn')) {
  vetr(warn=LGL.1, what=CHR)
  if(length(what)) {
    if(anyNA(what.int <- match(what, VALID.WHAT)))
      stop(
        "Argument `what` may contain only values in `", deparse(VALID.WHAT), "`"
      )
    .Call(FANSI_strip_csi, x, what.int, warn)
  } else x
}

## Process String by Removing Unwanted Characters
##
## This is to simulate what `strwrap` does

process <- function(x) .Call(FANSI_process, x)

