## Copyright (C) Brodie Gaslam
##
## This file is part of "fansi - ANSI Control Sequence Aware String Functions"
##
## This program is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 2 or 3 of the License.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## Go to <https://www.r-project.org/Licenses> for copies of the licenses.

#' Identify Unhandled Control Sequences
#'
#' Will return position and types of unhandled _Control Sequences_ in a
#' character vector.  Unhandled sequences may cause `fansi` to interpret strings
#' in a way different to your display.  See [fansi] for details.  Functions that
#' interpret _Special Sequences_ (CSI SGR or OSC hyperlinks) might omit bad
#' _Special Sequences_ or some of their components in output substrings,
#' particularly if they are leading or trailing.  Some functions are more
#' tolerant of bad inputs than others.  For example [`nchar_ctl`] will not
#' report unsupported colors because it only cares about counts or widths.
#' `unhandled_ctl` will report all potentially problematic sequences.
#'
#' To work around tabs present in input, you can use [`tabs_as_spaces`] or the
#' `tabs.as.spaces` parameter on functions that have it, or the [`strip_ctl`]
#' function to remove the troublesome sequences.  Alternatively, you can use
#' `warn=FALSE` to suppress the warnings.
#'
#' This is a debugging function that is not optimized for speed and the precise
#' output of which might change with `fansi` versions.
#'
#' The return value is a data frame with five columns:
#'
#' * index: integer the index in `x` with the unhandled sequence
#' * start: integer the start position of the sequence (in characters)
#' * stop: integer the end of the sequence (in characters), but note that if
#'   there are multiple ESC sequences abutting each other they will all be
#'   treated as one, even if some of those sequences are valid.
#' * error: the reason why the sequence was not handled:
#'     * unknown-substring: SGR substring with a value that does not correspond
#'       to a known SGR code or OSC hyperlink with unsupported parameters.
#'     * invalid-substr: SGR contains uncommon characters in ":<=>",
#'       intermediate bytes, other invalid characters, or there is an invalid
#'       subsequence (e.g. "ESC&#91;38;2m" which should specify an RGB triplet
#'       but does not).  OSCs contain invalid bytes, or OSC hyperlinks contain
#'       otherwise valid OSC bytes in 0x08-0x0d.
#'     * exceed-term-cap: contains color codes not supported by the terminal
#'       (see [term_cap_test]).  Bright colors with color codes in the 90-97 and
#'       100-107 range in terminals that do not support them are not considered
#'       errors, whereas 256 or truecolor codes in terminals that do not support
#'       them are.  This is because the latter are often misinterpreted by
#'       terminals that do not support them, whereas the former are typically
#'       silently ignored.
#'     * CSI/OSC: a non-SGR CSI sequence, or non-hyperlink OSC sequence.
#'     * CSI/OSC-bad-substr: a CSI or OSC sequence containing invalid
#'       characters.
#'     * malformed-CSI/OSC: a malformed CSI or OSC sequence, typically one that
#'       never encounters its closing sequence before the end of a string.
#'     * non-CSI/OSC: a non-CSI or non-OSC escape sequence, i.e. one where the
#'       ESC is followed by something other than "&#91;" or "&#93;".  Since we
#'       assume all non-CSI sequences are only 2 characters long include the
#'       ESC, this type of sequence is the most likely to cause problems as some
#'       are not actually two characters long.
#'     * malformed-ESC: a malformed two byte ESC sequence (i.e. one not ending
#'       in 0x40-0x7e).
#'     * C0: a "C0" control character (e.g. tab, bell, etc.).
#'     * malformed-UTF8: illegal UTF8 encoding.
#'     * non-ASCII: non-ASCII bytes in escape sequences.
#' * translated: whether the string was translated to UTF-8, might be helpful in
#'   odd cases were character offsets change depending on encoding.  You should
#'   only worry about this if you cannot tie out the `start`/`stop` values to
#'   the escape sequence shown.
#' * esc: character the unhandled escape sequence
#'
#' @note Non-ASCII strings are converted to UTF-8 encoding.
#' @export
#' @inherit has_ctl seealso
#' @param x character vector
#' @inheritParams substr_ctl
#' @return Data frame with as many rows as there are unhandled escape
#'   sequences and columns containing useful information for debugging the
#'   problem.  See details.
#'
#' @examples
#' string <- c(
#'   "\033[41mhello world\033[m", "foo\033[22>m", "\033[999mbar",
#'   "baz \033[31#3m", "a\033[31k", "hello\033m world"
#' )
#' unhandled_ctl(string)

unhandled_ctl <- function(
  x, term.cap=getOption('fansi.term.cap', dflt_term_cap())
) {
  ## modifies / creates NEW VARS in fun env
  VAL_IN_ENV(x=x, term.cap=term.cap)
  res <- .Call(FANSI_unhandled_esc, x, TERM.CAP.INT)
  names(res) <- c("index", "start", "stop", "error", "translated", "esc")
  errors <- c(
    'unknown-substr', 'invalid-substr', 'exceed-term-cap', 'non-SGR/hyperlink',
    'CSI/OSC-bad-substr', 'malformed-CSI/OSC', 'non-CSI/OSC',
    'malformed-ESC', 'C0', 'malformed-UTF8', 'non-ASCII'
  )
  res[['error']] <- errors[res[['error']]]
  as.data.frame(res, stringsAsFactors=FALSE)
}

