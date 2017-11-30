#' ANSI CSI Aware Version of strwrap
#'
#' @inheritParams base::strwrap
#' @import vetr

strwrap_csi <- function(
  x, width = 0.9 * getOption("width"), indent = 0,
  exdent = 0, prefix = "", simplify = TRUE, initial = prefix
) {
  if(!is.character(x)) x <- as.character(x)
  vetr(
    character(), numeric(1), INT.POS.1,
    INT.POS.1, character(1), simplify=LGL.1, character(1)
  )
  width <- as.integer(width)
  indent <- as.integer(indent)
  exdent <- as.integer(exdent)

  .Call(FANSI_strwrap_ext, x, width, indent, exdent, prefix, simplify, initial)
}
