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
  VAL_IN_ENV(x=x, ctl=ctl, warn=warn)

  if(length(ctl)) .Call(FANSI_strip_csi, enc2utf8(x), ctl.int, warn)
  else x
}
#' @export
#' @rdname strip_ctl

strip_sgr <- function(x, warn=getOption('fansi.warn')) {
  VAL_IN_ENV(x=x, warn=warn)
  ctl.int <- match("sgr", VALID.CTL)
  .Call(FANSI_strip_csi, x, ctl.int, warn)
}

#' Checks for Presence of Control Sequences
#'
#' `has_ctl` checks for any _Control Sequence_, whereas `has_sgr` checks only
#' for ANSI CSI SGR sequences.  You can check for different types of sequences
#' with the `ctl` parameter.
#'
#' @export
#' @seealso [fansi] for details on how _Control Sequences_ are
#'   interpreted, particularly if you are getting unexpected results.
#' @inheritParams substr_ctl
#' @inheritParams strip_ctl
#' @inheritSection substr_ctl _ctl vs. _sgr
#' @param which character, deprecated in favor of `ctl`.
#' @return logical of same length as `x`; NA values in `x` result in NA values
#'   in return
#' @examples
#' has_ctl("hello world")
#' has_ctl("hello\nworld")
#' has_ctl("hello\nworld", "sgr")
#' has_ctl("hello\033[31mworld\033[m", "sgr")
#' has_sgr("hello\033[31mworld\033[m")
#' has_sgr("hello\nworld")

has_ctl <- function(x, ctl='all', warn=getOption('fansi.warn'), which) {
  if(!missing(which)) {
    message("Parameter `which` has been deprecated; use `ctl` instead.")
    ctl <- which
  }
  VAL_IN_ENV(x=x, ctl=ctl, warn=warn)
  if(length(ctl.int)) {
    .Call(FANSI_has_csi, x, ctl.int, warn)
  } else rep(FALSE, length(x))
}
#' @export
#' @rdname has_ctl

has_sgr <- function(x, warn=getOption('fansi.warn'))
  has_ctl(x, ctl="sgr", warn=warn)

#' Utilities for Managing SGR In Strings
#'
#' `sgr_at_end` read input strings computing the accumulated SGR codes until the
#' end of the string and outputs the active SGR code at the end of it.
#'
#' `close_sgr` produces the ANSI CSI SGR sequence that closes active SGR codes
#' at the end of the input string.  If `normalize = FALSE` (default), it will
#' issue the global closing SGR "ESC[0m", so it is only interesting if
#' `normalize = TRUE`.  Unlike `sgr_at_end` and other functions `close_sgr` has
#' no concept of `carry`: it will only close SGR codes activated within each
#' element.
#'
#' @export
#' @inheritParams substr_ctl
#' @return character vector same length as `x`.
#' @examples
#' x <- c("\033[44mhello", "\033[33mworld")
#' sgr_at_end(x)
#' sgr_at_end(x, carry=TRUE)
#' (close <- close_sgr(sgr_at_end(x, carry=TRUE), normalize=TRUE))
#' writeLines(paste0(x, close, " no style"))

sgr_at_end <- function(
  x,
  warn=getOption('fansi.warn'),
  term.cap=getOption('fansi.term.cap'),
  normalize=getOption('fansi.normalize', FALSE),
  carry=getOption('fansi.carry', FALSE)
) {
  VAL_IN_ENV(x=x, ctl='sgr', warn=warn, term.cap=term.cap, carry=carry)
  .Call(
    FANSI_sgr_at_end,
    x,
    0L,             # character type
    warn,
    term.cap.int,
    ctl.int,
    normalize,
    carry
  )
}

# Given an SGR, compute the sequence that closes it

#' @export
#' @rdname sgr_at_end

close_sgr <- function(
  x,
  warn=getOption('fansi.warn'),
  normalize=getOption('fansi.normalize', FALSE)
) {
  VAL_IN_ENV(x=x, warn=warn, normalize=normalize)
  .Call(FANSI_close_sgr, x, warn, seq_along(VALID.TERM.CAP), normalize)
}


## Process String by Removing Unwanted Characters
##
## This is to simulate what `strwrap` does, exposed for testing purposes.

process <- function(x) .Call(FANSI_process, enc2utf8(x))

