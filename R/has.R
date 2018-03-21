## Copyright (C) 2018  Brodie Gaslam
##
## This file is part of "fansi - ANSI Control Sequence Aware String Functions"
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
#' `has_ctl` checks for any _Control Sequence_, whereas `has_sgr` checks only
#' for ANSI CSI SGR sequences.  You can check for different types of sequences
#' with the `which` parameter.
#'
#' @export
#' @seealso [fansi] for details on how _Control Sequences_ are
#'   interpreted, particularly if you are getting unexpected results.
#' @inheritParams strip_ctl
#' @param which character, what _Control Sequences_ to check for; see `strip`
#'   parameter for [strip_ctl] for details.
#' @return logical of same length as `x`; NA values in `x` result in NA values
#'   in return

has_ctl <- function(x, which='all', warn=getOption('fansi.warn')) {
  if(!is.logical(warn)) warn <- as.logical(warn)
  if(length(warn) != 1L || is.na(warn))
    stop("Argument `warn` must be TRUE or FALSE.")
  if(!is.character('which')) stop("Argument `which` must be character.")

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
#' @rdname has_ctl

has_sgr <- function(x, warn=getOption('fansi.warn'))
  has_ctl(x, which="sgr", warn=warn)
