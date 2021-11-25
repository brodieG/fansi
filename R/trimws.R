#' Control Sequence Aware Version of trimws
#'
#' Removes any whitespace before the first and/or after the last non-_Control
#' Sequence_ character.  Unlike with the [`base::trimws`], only the default
#' `whitespace` specification is supported.
#'
#' @export
#' @inheritSection substr_ctl Output Stability
#' @inheritParams base::trimws
#' @inheritParams substr_ctl
#' @param whitespace must be set to the default value, in the future it may
#'   become possible to change this parameter.
#' @return The input with white space removed as described.
#' @examples
#' trimw_ws(" \033[31m\thello world\t\033[39m  ")

trimws_ctl <- function(
  x, which = c("both", "left", "right"), whitespace = "[ \t\r\n]",
  ctl='all', warn=getOption('fansi.warn', TRUE)
) {
  if(!identical(whitespace,  "[ \t\r\n]"))
    stop("Argument `whitespace` may only be set to \"[ \\t\\r\\n]\".")
  VAL_IN_ENV(x=x, ctl=ctl, warn=warn);
  valid.which <- c("both", "left", "right")
  if(
    !is.character(which) || length(which) != 1 ||
    is.na(which.int <- pmatch(which, valid.which))
  )
    stop("Argument `which` must partial match one of ", deparse(valid.which))

  .Call(FANSI_trimws, x, which.int, CTL.INT, WARN.INT)
}
