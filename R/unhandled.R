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
#' * stop: integer the start position of the sequence (in characters)
#' * error: the reason why the sequence was not handled:
#'     * special: contains uncommon characters in ":<=>"
#'     * unknown: a substring with a value that does not correspond to a known
#'       SGR code
#'     * non-SGR: a non-SGR CSI sequence
#'     * malformed: a malformed CSI sequence
#'     * non-CSI: a non-CSI escape sequence, i.e. one where the ESC is
#'       followed by something other than "["
#'     * C0: a "C0" control character (e.g. tab, bell, etc.)
#' * translated: whether the string was translated to UTF-8, might be helpful in
#'   odd cases were character offsets change depending on encoding.  You should
#'   only worry about this if you cannot tie out the `start`/`stop` values to
#'   the escape sequence shown.
#' * esc: character the unhandled escape sequence
#'
#' @export
#' @seealso [string-parsing] for important details on how strings are
#'   interpreted and how character width is computed, [term_cap_test] to ensure
#'   `fansi` is correctly interpreting your terminal capabilities.
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
  errors <- c('special', 'unknown', 'non-SGR', 'malformed', 'non-CSI', 'C0')
  res[['error']] <- errors[res[['error']]]
  res[['esc']] <- substring(
    x[res[['index']]],
    res[['start']],
    res[['stop']]
  )
  as.data.frame(res, stringsAsFactors=FALSE)
}

