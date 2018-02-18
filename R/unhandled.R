#' Identify Unhandled Escape Sequences
#'
#' Will record position and types of unhandled sequences in a character vector.
#'
#' This is intended as a debugging function and as such is not optimized for
#' speed.
#'
#' All byte offsets are computed on UTF-8 converted strings.  As such, you may
#' want to translate your character vector to UTF-8 if it contains non-ASCII
#' characters and it is in a different encoding.  This function will warn you if
#' it encounters such strings.
#'
#' The return value is a data frame with five columns:
#'
#' * index: integer the index in `x` with the unhandled sequence
#' * esc: character the unhandled escape sequence
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
#'
#' @export
#' @seealso [string-parsing] for details of how `fansi` interprets escape
#'   sequences, [iconv] for how to translate strings across encodings.
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
  errors <- c('special', 'unknown', 'non-SGR', 'malformed', 'non-CSI')
  res[['error']] <- errors[res[['error']]]
  as.data.frame(res, stringsAsFactors=FALSE)
}

