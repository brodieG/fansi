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

#' Checks for Control Characters or Sequences
#'
#' By default, checks for ANSI CSI SGR sequences only.
#'
#' @export
#' @seealso [fansi] for details on how control characters and sequences are
#'   interpreted, particularly if you are getting unexpected results.
#' @inheritParams strip_esc
#' @return logical of same length as `x`; NA values in `x` result in NA values
#'   in return

has_esc <- function(x, strip='sgr', warn=getOption('fansi.warn')) {
  vetr(warn=LGL.1, strip=CHR)
  if(length(strip)) {
    if(anyNA(strip.int <- match(strip, VALID.STRIP)))
      stop(
        "Argument `strip` may contain only values in `", deparse(VALID.STRIP), "`"
      )
    .Call(FANSI_has_csi, x, strip.int, warn)
  } else rep(FALSE, length(x))
}
