#' Identify Unhandled Escape Sequences
#'
#' Will return position and types of unhandled sequences in a character vector.
#' Unhandled sequences may cause `fansi` to interpret strings in a way
#' different to your display.  See [fansi] for details.
#'
#' This is a debugging function and as such is not optimized for speed.
#'
#' The return value is a data frame with five columns:
#'
#' * index: integer the index in `x` with the unhandled sequence
#' * start: integer the start position of the sequence (in characters)
#' * stop: integer the end of the sequence (in characters), but note that if
#'   there are multiple ESC sequences abutting each other they will all be
#'   treated as one, even if some of those sequences are valid.
#' * error: the reason why the sequence was not handled:
#'     * exceed-term-cap: contains color codes not supported by the terminal
#'       (see [term_cap_test]).
#'     * special: contains uncommon characters in ":<=>".
#'     * unknown: a substring with a value that does not correspond to a known
#'       SGR code.
#'     * non-SGR: a non-SGR CSI sequence.
#'     * non-CSI: a non-CSI escape sequence, i.e. one where the ESC is
#'       followed by something other than "[".  Since we assume all non-CSI
#'       sequences are only 2 characters long include the ESC, this type of
#'       sequence is the most likely to cause problems as many are not actually
#'       two characters long.
#'     * malformed-CSI: a malformed CSI sequence.
#'     * malformed-ESC: a malformed ESC sequence (i.e. one not ending in
#'       0x40-0x7e).
#'     * C0: a "C0" control character (e.g. tab, bell, etc.).
#' * translated: whether the string was translated to UTF-8, might be helpful in
#'   odd cases were character offsets change depending on encoding.  You should
#'   only worry about this if you cannot tie out the `start`/`stop` values to
#'   the escape sequence shown.
#' * esc: character the unhandled escape sequence
#'
#' @export
#' @seealso [fansi] for details on how control characters and sequences are
#'   interpreted, and [term_cap_test] to ensure `fansi` is correctly
#'   interpreting your terminal capabilities, particularly if you are getting
#'   unexpected results.
#' @param x character vector
#' @return data frame with as many rows as there are unhandled escape
#'   sequences and columns containing useful information for debugging the
#'   problem.  See details.
#'
#' @examples
#' string <- c(
#'   "\033[41mhello world\033[m", "foo\033[22>m", "\033[999mbar",
#'   "baz \033[31#3m", "a\033[31k", "hello\033m world"
#' )
#' unhandled_esc(string)

unhandled_esc <- function(x) {
  res <- .Call(FANSI_unhandled_esc, x)
  names(res) <- c("index", "start", "stop", "error", "translated")
  errors <- c(
    'exceed-term-cap', 'special', 'unknown', 'non-SGR', 'malformed-CSI',
    'non-CSI', 'malformed-ESC', 'C0'
  )
  res[['error']] <- errors[res[['error']]]
  res[['esc']] <- substring(
    x[res[['index']]],
    res[['start']],
    res[['stop']]
  )
  as.data.frame(res, stringsAsFactors=FALSE)
}

