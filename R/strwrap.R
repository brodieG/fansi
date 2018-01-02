#' ANSI CSI Aware Versions of strwrap
#'
#' Unlike [base::strwrap], this function will re-encode any non-ASCII strings to
#' UTF8 if they are not encoded in UTF8.
#'
#' `strwrap2_csi` provides additional features and also has different default
#' behavior to `strwrap`.
#'
#' Special character stripping as controlled by `strip.spaces`, `strip.tabs`,
#' and `strip.control` are done in a separate pass and as a result will cause
#' the wrapping to be slower due to the need to allocate memory and populate
#' the global string cache for the stripping pass in additional to the wrapping
#' pass.
#'
#' Width computations are done internally by using `R_nchar` on non-ASCII bytes
#' and counting ASCII bytes in normal ranges as being 1 wide.  This is subject
#' to all the limitations of `R_nchar`, but additionally will provide the wrong
#' answer when `R_nchar` correctly computes the width of a non-standard
#' multi-character sequence if passed the whole sequence, but not if each
#' character is passed individually as done here.  This will typically manifest
#' with things like combining emoji, though as of this writing we have not found
#' any combining sequences that `R_nchar` computes correctly when provided in
#' full and incorrectly when provided character by character.
#'
#' @inheritParams base::strwrap
#' @param wrap.always TRUE or FALSE (default), whether to hard wrap at requested
#'   width if no word breaks are detected within a line.
#' @pad.end TRUE or FALSE (default), whether to fill lines with spaces up to
#'   `width`.
#' @param strip.spaces TRUE (default) or FALSE, if TRUE, extraneous spaces
#'   (leading, trailing, repeated - except that after sentence end up to two
#'   spaces are kept) are removed.
#' @param strip.tabs TRUE (default) or FALSE, if TRUE, ASCII tab control
#'   characters (i.e. '\t') are stripped.
#' @param strip.control TRUE or FALSE (default), if TRUE, ASCII control
#'   characters are stripped, except for tabs and newlines.  Newlines always
#'   are implicitly stripped because CHARSXPs containing them are split at the
#'   newline.
#' @param tabs.as.spaces TRUE or FALSE (default) whether to convert tabs to
#'   spaces
#' @param tab.stops integer(1:n) indicating position of tab stops to use when
#'   converting tabs to spaces.  If there are more tabs in a line than defined
#'   tab stops the last tab stop is re-used.  For the purposes of applying tab
#'   stops, each input line is considered a line and the character count begins
#'   from the beginning of the input line.  Thus, if an input line is wrapped
#'   the tab stops will not reset at the wrap point.  Additionally,`indent`,
#'   `exdent`, `initial`, and `prefix` will be ignore when computing tab
#'   positions.  If you wish to use non-default values for those parameters in
#'   conjunction with `tabs.as.spaces=TRUE`, you are probably better off adding
#'   those modifications manually to the string after wrapping it.
#' @export

strwrap_csi <- function(
  x, width = 0.9 * getOption("width"), indent = 0,
  exdent = 0, prefix = "", simplify = TRUE, initial = prefix
) {
  if(!is.character(x)) x <- as.character(x)
  vetr(
    x=character(), width=NUM.1.POS && . >= 1, indent=INT.1.POS,
    exdent=INT.1.POS, prefix=character(1), simplify=LGL.1, initial=character(1)
  )
  width <- as.integer(width) - 1L
  indent <- as.integer(indent)
  exdent <- as.integer(exdent)

  res <- .Call(
    FANSI_strwrap_csi,
    x, width, indent, exdent,
    prefix, initial,
    FALSE, FALSE,
    TRUE, TRUE, FALSE,
    FALSE, 8L
  )
  if(simplify) unlist(res) else res
}

#' @export
#' @rdname strwrap_csi

strwrap2_csi <- function(
  x, width = 0.9 * getOption("width"), indent = 0,
  exdent = 0, prefix = "", simplify = TRUE, initial = prefix,
  wrap.always=TRUE, pad.end=TRUE,
  strip.spaces=FALSE, strip.tabs=FALSE, strip.control=TRUE,
  tabs.as.spaces=FALSE, tab.stops=8L
) {
  if(!is.character(x)) x <- as.character(x)
  vetr(
    x=character(), width=NUM.1.POS && . >= 1, indent=INT.1.POS,
    exdent=INT.1.POS, prefix=character(1), simplify=LGL.1, initial=character(1),
    pad.end=LGL.1, wrap.always=LGL.1, strip.spaces=LGL.1,
    strip.control=LGL.1, tabs.as.spaces=LGL.1,
    tab.stops=INT.POS.STR && length(.) > 0
  )
}

