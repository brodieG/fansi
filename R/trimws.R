## Copyright (C) 2022 Brodie Gaslam
##
## This file is part of "fansi - ANSI Control Sequence Aware String Functions"
##
## This program is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 2 or 3 of the License.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## Go to <https://www.r-project.org/Licenses> for copies of the licenses.

#' Control Sequence Aware Version of trimws
#'
#' Removes any whitespace before the first and/or after the last non-_Control
#' Sequence_ character.  Unlike with the [`base::trimws`], only the default
#' `whitespace` specification is supported.
#'
#' @export
#' @inheritSection substr_ctl Control and Special Sequences
#' @inheritSection substr_ctl Output Stability
#' @inheritParams base::trimws
#' @inheritParams substr_ctl
#' @param whitespace must be set to the default value, in the future it may
#'   become possible to change this parameter.
#' @return The input with white space removed as described.
#' @examples
#' trimws_ctl(" \033[31m\thello world\t\033[39m  ")

trimws_ctl <- function(
  x, which = c("both", "left", "right"), whitespace = "[ \t\r\n]",
  warn=getOption('fansi.warn', TRUE),
  term.cap=getOption('fansi.term.cap', dflt_term_cap()),
  ctl='all', normalize=getOption('fansi.normalize', FALSE)
) {
  if(!identical(whitespace,  "[ \t\r\n]"))
    stop("Argument `whitespace` may only be set to \"[ \\t\\r\\n]\".")
  # modifies/adds vars in env
  VAL_IN_ENV(x=x, ctl=ctl, warn=warn, term.cap=term.cap, normalize=normalize);
  valid.which <- c("both", "left", "right")
  if(
    !is.character(which) || length(which[1]) != 1 ||
    is.na(which.int <- pmatch(which[1], valid.which))
  )
    stop(
      "Argument `which` must partial match one of ", deparse(valid.which), "."
    )

  .Call(
    FANSI_trimws, x, which.int - 1L, WARN.INT, TERM.CAP.INT, CTL.INT, normalize
  )
}
