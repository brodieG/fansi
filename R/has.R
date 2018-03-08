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

#' Checks for Presence of Control Sequences
#'
#' `has_esc` checks for any _Control Sequence_, whereas `has_sgr` checks only
#' for ANSI CSI SGR sequences.  You can check for different types of sequences
#' with the `which` parameter.
#'
#' @export
#' @seealso [fansi] for details on how _Control Sequences_ are
#'   interpreted, particularly if you are getting unexpected results.
#' @inheritParams strip_esc
#' @param which character, what Control Sequences to check for; see `strip`
#'   parameter for [strip_esc] for details.
#' @return logical of same length as `x`; NA values in `x` result in NA values
#'   in return

has_esc <- function(x, which='all', warn=getOption('fansi.warn')) {
  vetr(warn=LGL.1, which=CHR)
  if(length(which)) {
    if(anyNA(which.int <- match(which, VALID.STRIP)))
      stop(
        "Argument `which` may contain only values in `",
        deparse(VALID.STRIP), "`"
      )
    .Call(FANSI_has_csi, x, which.int, warn)
  } else rep(FALSE, length(x))
}
#' @export
#' @rdname has_esc

has_sgr <- function(x, warn=getOption('fansi.warn'))
  has_esc(x, which="sgr", warn=warn)
