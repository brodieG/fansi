#' ANSI Escape Sequence Aware Versions of strwrap
#'
#' Wraps strings to a specified width accounting for zero display width ANSI
#' escape sequences and control characters.  `strwrap_esc` is intended to
#' emulate `strwrap` exactly except with respect to the escape sequences, while
#' `strwrap2_esc` adds features and changes the processing of whitespace.
#'
#' `strwrap2_esc` can convert tabs to spaces, pad strings up to `width`, and
#' hard-break words if single words are wider than `width`.
#'
#' Unlike [base::strwrap], both these functions will re-encode any non-ASCII
#' strings to UTF8 if they are not encoded in UTF8.
#'
#' @seealso [string-parsing] for important details on how strings are
#'   interpreted and how character width is computed.
#' @inheritParams base::strwrap
#' @param wrap.always TRUE or FALSE (default), whether to hard wrap at requested
#'   width if no word breaks are detected within a line.
#' @param pad.end character(1L), a single character to use as padding at the
#'   end of each line until the line is `width` wide.  This must be a printable
#'   ASCII character or an empty string (default).  If you set it to an empty
#'   string the line remains unpadded.
#' @param strip.spaces TRUE (default) or FALSE, if TRUE, extraneous white spaces
#'   (spaces, newlines, tabs) are removed in the same way as [base::strwrap]
#'   does.
#' @param tabs.as.spaces FALSE (default) or TRUE, whether to convert tabs to
#'   spaces.  This can only be set to TRUE if `strip.spaces` is FALSE.
#' @param tab.stops integer(1:n) indicating position of tab stops to use when
#'   converting tabs to spaces.  If there are more tabs in a line than defined
#'   tab stops the last tab stop is re-used.  For the purposes of applying tab
#'   stops, each input line is considered a line and the character count begins
#'   from the beginning of the input line.  Thus, if an input line is wrapped
#'   the tab stops will not reset at the wrap point.  Additionally,`indent`,
#'   `exdent`, `initial`, and `prefix` will be ignored when computing tab
#'   positions.
#' @export
#' @examples
#' hello.1 <- "hello \033[41mred\033[49m world"
#' hello.2 <- "hello\t\033[41mred\033[49m\tworld"
#'
#' strwrap_esc(hello.1, 12)
#' strwrap_esc(hello.2, 12)
#'
#' ## In default mode strwrap2_esc is the same as strwrap_esc
#' strwrap2_esc(hello.2, 12)
#'
#' ## But you can leave whitespace unchanged
#' strwrap2_esc(hello.2, 12, strip.spaces=FALSE)
#'
#' ## And convert tabs to spaces
#' strwrap2_esc(hello.2, 12, tabs.as.spaces=TRUE)
#'
#' ## If your display has 8 wide tab stops the following two
#' ## outputs should look the same
#' writeLines(strwrap2_esc(hello.2, 80, tabs.as.spaces=TRUE))
#' writeLines(hello.2)
#'
#'
#' ## tab stops are NOT auto-detected, but you may provide
#' ## your own
#' strwrap2_esc(hello.2, 12, tabs.as.spaces=TRUE, tab.stops=c(6, 12))
#'
#' ## You can also force padding at the end to equal width
#' writeLines(strwrap2_esc("hello how are you today", 10, pad.end="."))
#'
#' ## And a more involved example
#'

strwrap_esc <- function(
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
    FALSE, "",
    TRUE,
    FALSE, 8L
  )
  if(simplify) unlist(res) else res
}
#' @export
#' @rdname strwrap_esc

strwrap2_esc <- function(
  x, width = 0.9 * getOption("width"), indent = 0,
  exdent = 0, prefix = "", simplify = TRUE, initial = prefix,
  wrap.always=FALSE, pad.end="",
  strip.spaces=!tabs.as.spaces, tabs.as.spaces=FALSE, tab.stops=8L
) {
  if(!is.character(x)) x <- as.character(x)
  vetr(
    x=character(), width=NUM.1.POS && . >= 1, indent=INT.1.POS,
    exdent=INT.1.POS, prefix=character(1), simplify=LGL.1, initial=character(1),
    pad.end=CHR.1 && nchar(.) < 2, wrap.always=LGL.1, strip.spaces=LGL.1,
    tabs.as.spaces=LGL.1,
    tab.stops=INT.POS.STR && length(.) > 0
  )
  if(tabs.as.spaces && strip.spaces)
    stop("`tabs.as.spaces` and `strip.spaces` should not both be TRUE.")

  width <- as.integer(width) - 1L
  indent <- as.integer(indent)
  exdent <- as.integer(exdent)
  tab.stops <- as.integer(tab.stops)

  res <- .Call(
    FANSI_strwrap_csi,
    x, width,
    indent, exdent,
    prefix, initial,
    wrap.always, pad.end,
    strip.spaces,
    tabs.as.spaces, tab.stops
  )
  if(simplify) unlist(res) else res
}

