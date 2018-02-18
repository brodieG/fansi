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
#' @export
#' @seealso [string-parsing] for details of how `fansi` interprets escape
#'   sequences, [iconv] for how to translate strings across encodings.
#' @param x character vector
#' @return list of same length as `x` containing either a zero length vector,
#'   or a two row integer matrix for row one has the 1-indexed byte location of
#'   the beginning of the problematic sequence, and row two an integer code
#'   describing the type of problem where the codes are:
#'
#'   * 1: Valid CSI SGR with uncommon characters ":<=>".
#'   * 2: Valid CSI SGR with an unknown SGR substring.
#'   * 3: Valid non-SGR CSI sequence
#'   * 4: Malformed CSI sequences
#'   * 5: Other type of escape sequence
#'
#' @examples
#' string <- c(
#'   "\033[41mhello world\033[m", "foo\033[22>m", "\033[999mbar",
#'   "baz \033[31#3m", "a\033[31k", "hello\033m world"
#' )
#' unhandled_esc(string)
#' which(lengths(unhandled_esc(string)) == 0) # valid escapes

unhandled_esc <- function(x) {
}

