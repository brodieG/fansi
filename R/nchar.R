## Copyright (C) 2018  Brodie Gaslam
##
## This file is part of "fansi - ANSI Escape Aware String Functions"
##
## This program is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 2 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## Go to <https://www.r-project.org/Licenses/GPL-2> for a copy of the license.

#' ANSI Control Sequence Aware Version of nchar
#'
#' `nchar_esc` counts all non control character / sequence characters.
#' `nzchar_esc` returns TRUE for each input vector element that has non control
#' chacater / sequence characters.  Pay attention to the `what` parameter as
#' that will influence what is and is not counted.
#'
#' These functions are faster than the semantically equivalent
#' `nchar(strip_esc(x, what='all')).  If you wish to control which control
#' characters and sequences are stripped you will need to use
#' `nchar(strip_esc(x, what=...))`.
#'
#' @inheritParams nchar
#' @seealso [strip_esc]

nchar_esc <- function(
  x, type='chars', allowNA=FALSE, keepNA=NA, what=getOption('fansi.what'),
  warn=getOption('fansi.warn')
) {
  # TODO IMPLEMENT PARTIAL MATCHING
  vetr(
    warn=LGL.1, type=CHR.1 && . %in% c('chars', 'width', 'bytes'), what=CHR
  )
  if(anyNA(what.int <- match(what, VALID.WHAT)))
    stop(
      "Argument `what` may contain only values in `", deparse(VALID.WHAT), "`"
    )
  term.cap.int <- seq_along(VALID.TERM.CAP)
  .Call(FANSI_nchar, x, type, allowNA, keepNA, warn, term.cap.int)

}
nzchar_esc <- function(x, keepNA=NA, what=getOption('fansi.what'))
