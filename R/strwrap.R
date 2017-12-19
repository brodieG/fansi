#' ANSI CSI Aware Version of strwrap
#'
#' @export
#' @inheritParams base::strwrap
#' @param strict TRUE or FALSE (default), whether to hard wrap at requested
#'   width if no word breaks are detected.
#' @import vetr

strwrap_csi <- function(
  x, width = 0.9 * getOption("width"), indent = 0,
  exdent = 0, prefix = "", simplify = TRUE, initial = prefix,
  strict=FALSE
) {
  if(!is.character(x)) x <- as.character(x)
  vetr(
    character(), NUM.1.POS && . >= 1, INT.1.POS,
    INT.POS.1, character(1), simplify=LGL.1, character(1)
  )
  width <- as.integer(width)
  indent <- as.integer(indent)
  exdent <- as.integer(exdent)

  .Call(
    FANSI_strwrap_csi, x, width, indent, exdent, prefix, simplify, initial,
    strict
  )
}
