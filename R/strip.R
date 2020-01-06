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

#' Strip ANSI Control Sequences
#'
#' Removes _Control Sequences_ from strings.  By default it will
#' strip all known _Control Sequences_, including ANSI CSI
#' sequences, two character sequences starting with ESC, and all C0 control
#' characters, including newlines.  You can fine tune this behavior with the
#' `ctl` parameter.  `strip_sgr` only strips ANSI CSI SGR sequences.
#'
#' The `ctl` value contains the names of **non-overlapping** subsets of the
#' known _Control Sequences_ (e.g. "csi" does not contain "sgr", and "c0" does
#' not contain newlines).  The one exception is "all" which means strip every
#' known sequence.  If you combine "all" with any other option then everything
#' **but** that option will be stripped.
#'
#' @note Non-ASCII strings are converted to and returned in UTF-8 encoding.
#' @seealso [fansi] for details on how _Control Sequences_ are
#'   interpreted, particularly if you are getting unexpected results.
#' @inheritParams substr_ctl
#' @inheritSection substr_ctl _ctl vs. _sgr
#' @export
#' @param ctl character, any combination of the following values (see details):
#'   * "nl": strip newlines.
#'   * "c0": strip all other "C0" control characters (i.e. x01-x1f, x7F), 
#'     except for newlines and the actual ESC character.
#'   * "sgr": strip ANSI CSI SGR sequences.
#'   * "csi": strip all non-SGR csi sequences.
#'   * "esc": strip all other escape sequences.
#'   * "all": all of the above, except when used in combination with any of the
#'     above, in which case it means "all but" (see details).
#' @param strip character, deprecated in favor of `ctl`.
#' @return character vector of same length as x with ANSI escape sequences
#'   stripped
#' @examples
#' string <- "hello\033k\033[45p world\n\033[31mgoodbye\a moon"
#' strip_ctl(string)
#' strip_ctl(string, c("nl", "c0", "sgr", "csi", "esc")) # equivalently
#' strip_ctl(string, "sgr")
#' strip_ctl(string, c("c0", "esc"))
#'
#' ## everything but C0 controls, we need to specify "nl"
#' ## in addition to "c0" since "nl" is not part of "c0"
#' ## as far as the `strip` argument is concerned
#' strip_ctl(string, c("all", "nl", "c0"))
#'
#' ## convenience function, same as `strip_ctl(ctl='sgr')`
#' strip_sgr(string)

strip_ctl <- function(x, ctl='all', warn=getOption('fansi.warn'), strip) {
  if(!missing(strip)) {
    message("Parameter `strip` has been deprecated; use `ctl` instead.")
    ctl <- strip
  }
  if(!is.character(x)) x <- as.character(x)

  if(!is.logical(warn)) warn <- as.logical(warn)
  if(length(warn) != 1L || is.na(warn))
    stop("Argument `warn` must be TRUE or FALSE.")

  if(!is.character(ctl))
    stop("Argument `ctl` must be character.")
  if(length(ctl)) {
    # duplicate values in `ctl` are okay, so save a call to `unique` here
    if(anyNA(ctl.int <- match(ctl, VALID.CTL)))
      stop(
        "Argument `ctl` may contain only values in `",
        deparse(VALID.CTL), "`"
      )
    .Call(FANSI_strip_csi, enc2utf8(x), ctl.int, warn)
  } else x
}
#' @export
#' @rdname strip_ctl

strip_sgr <- function(x, warn=getOption('fansi.warn')) {
  if(!is.character(x)) x <- as.character(x)
  if(!is.logical(warn)) warn <- as.logical(warn)
  if(length(warn) != 1L || is.na(warn))
    stop("Argument `warn` must be TRUE or FALSE.")

  ctl.int <- match("sgr", VALID.CTL)
  if(anyNA(ctl.int))
    stop("Internal Error: invalid ctl type; contact maintainer.") # nocov

  .Call(FANSI_strip_csi, enc2utf8(x), ctl.int, warn)
}

## Process String by Removing Unwanted Characters
##
## This is to simulate what `strwrap` does, exposed for testing purposes.

process <- function(x) .Call(FANSI_process, enc2utf8(x))

