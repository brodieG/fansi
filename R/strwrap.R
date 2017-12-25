#' ANSI CSI Aware Version of strwrap
#'
#' Unlike [base::strwrap], this function will re-encode any non-ASCII strings to
#' UTF8.
#'
#' Don't expect t
#'
#' @export
#' @inheritParams base::strwrap
#' @param strict TRUE or FALSE (default), whether to hard wrap at requested
#'   width if no word breaks are detected within a line (NOT IMPLEMENTED).
#' @param preserve.whitespace TRUE or FALSE (default), whether white spaces
#'   should be preserved.  If FALSE behaves like [base::strwrap].  If TRUE
#'   spaces are preserved, and tabs are converted to spaces according to
#'   `tab.stops` (NOT IMPLEMENTED).  For simplicity whitespace stripping is done
#'   in a separate pass so running with `preserve.whitespace=FALSE` (the
#'   default) is slower than the alternate.
#' @param tab.stops integer vector of strictly positive integer values that
#'   represents the tab stops you wish to use to convert tabs into spaces.  The
#'   vector is recycled for the entire length of each line, so the default value
#'   translates into a tab stop every 8 characters.
#' @param strip.control strip zero-width control characters like '\r', '\a',
#'   etc..  [base::strwrap] treats these normally as one width characters.

strwrap_csi <- function(
  x, width = 0.9 * getOption("width"), indent = 0,
  exdent = 0, prefix = "", simplify = TRUE, initial = prefix,
  strict=FALSE, preserve.whitespace=FALSE, tab.stops=8L,
  strip.control=TRUE
) {
  if(!is.character(x)) x <- as.character(x)
  vetr(
    character(), NUM.1.POS && . >= 1, INT.1.POS,
    INT.1.POS, character(1), simplify=LGL.1, character(1)
  )
  width <- as.integer(width) - 1L
  indent <- as.integer(indent)
  exdent <- as.integer(exdent)
  strict <- as.logical(strict)

  res <- .Call(
    FANSI_strwrap_csi,
    x, width, indent, exdent,
    prefix, initial, strict
  )
  if(simplify) unlist(res) else res
}
