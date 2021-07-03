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

#' Normalize CSI SGR Sequences
#'
#' Re-encodes SGR and OSC encoded URL sequences into a unique decomposed form.
#' Strings containing semantically identical SGR and OSC sequences that are
#' encoded differently should compare equal after normalization.
#'
#' Each compound SGR sequence is broken up into individual tokens, superfluous
#' tokens are removed, and the SGR reset sequence "ESC&#91;0m" (or "ESC&#91;m")
#' is replaced by the closing codes for whatever SGR styles are active at the
#' point in the string in which it appears.
#'
#' Unrecognized SGR codes will be dropped from the output with a warning.  The
#' specific order of SGR codes associated with any given SGR sequence is not
#' guaranteed to remain the same across different versions of `fansi`, but
#' should remain unchanged except for the addition of previously uninterpreted
#' codes to the list of interpretable codes.  There is no special significance
#' to the order the SGR codes are emitted in other than it should be consistent
#' for any given SGR state.  URLs adjacent to SGR codes are always emitted after
#' the SGR codes irrespective of what side they were on originally.
#'
#' OSC encoded URL sequences are always terminated by "ESC&#93;\\", and those
#' between abutting URLs are omitted.  Identical abutting URLs are merged.  In
#' order for URLs to be considered identical both the URL and the "id" parameter
#' must be specified and be the same.  OSC URL parameters other than "id" are
#' dropped with a warning.
#'
#' The underlying assumption is that each element in the vector is
#' unaffected by SGR or OSC URLs in any other element or elsewhere.  This may
#' lead to surprising outcomes if these assumptions are untrue (see examples).
#' You may adjust this assumption with the `carry` parameter.
#'
#' Normalization was implemented primarily for better compatibility with
#' [`crayon`][1] which emits SGR codes individually and assumes that each
#' opening code is paired up with its specific closing code, but it can also be
#' used to reduce the probability that strings processed with future versions of
#' `fansi` will produce different results than the current version.
#'
#' [1]: https://cran.r-project.org/package=crayon
#'
#' @export
#' @inheritParams substr_ctl
#' @inherit has_ctl seealso
#' @return `x`, with all SGRs normalized.
#' @examples
#' normalize_sgr("hello\033[42;33m world")
#' normalize_sgr("hello\033[42;33m world\033[m")
#' normalize_sgr("\033[4mhello\033[42;33m world\033[m")
#'
#' ## Superflous codes removed
#' normalize_sgr("\033[31;32mhello\033[m")      # only last color prevails
#' normalize_sgr("\033[31\033[32mhello\033[m")  # only last color prevails
#' normalize_sgr("\033[31mhe\033[49mllo\033[m") # unused closing
#'
#' ## Equivalent normalized sequences compare identical
#' identical(
#'   normalize_sgr("\033[31;32mhello\033[m"),
#'   normalize_sgr("\033[31mhe\033[49mllo\033[m")
#' )
#' ## External SGR will defeat normalization, unless we `carry` it
#' red <- "\033[41m"
#' writeLines(
#'   c(
#'     paste(red, "he\033[0mllo", "\033[0m"),
#'     paste(red, normalize_sgr("he\033[0mllo"), "\033[0m")
#'     paste(red, normalize_sgr("he\033[0mllo", carry=red), "\033[0m")
#' ) )

normalize_sgr <- function(
  x, warn=getOption('fansi.warn'), term.cap=getOption('fansi.term.cap'),
  carry=getOption('fansi.carry', FALSE)
) {
  VAL_IN_ENV(x=x, warn=warn, term.cap=term.cap, carry=carry)
  .Call(FANSI_normalize_sgr, x, warn, term.cap.int, carry)
}
# To reduce overhead of applying this in `strwrap_ctl`

normalize_sgr_list <- function(x, warn, term.cap.int, carry)
  .Call(FANSI_normalize_sgr_list, x, warn, term.cap.int, carry)

