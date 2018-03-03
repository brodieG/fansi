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
#' These functions should be faster than the semantically equivalent
#' `nchar(strip_esc(...))`.
#'
#' @inheritParams nchar
#' @param what character(1L), as in [strip_esc], except here it is what control
#'   characters and sequences to exclude from the count.
#'

nchar_esc <- function(
  x, type='chars', allowNA=FALSE, keepNA=NA, what=getOption('fansi.what'),
  warn=getOption('fansi.warn')
) {
  vetr(warn=LGL.1, what=CHR)
  if(anyNA(what.int <- match(what, VALID.WHAT)))
    stop(
      "Argument `what` may contain only values in `", deparse(VALID.WHAT), "`"
    )
  .Call(FANSI_nchar, x, what.int, warn)

}
nzchar_esc <- function(x, keepNA=NA, what=getOption('fansi.what'))
