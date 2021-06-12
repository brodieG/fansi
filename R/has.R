## Copyright (C) 2021  Brodie Gaslam
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
#' with the `ctl` parameter.
#'
#' @export
#' @seealso [fansi] for details on how _Control Sequences_ are
#'   interpreted, particularly if you are getting unexpected results.
#' @inheritParams substr_ctl
#' @inheritParams strip_ctl
#' @inheritSection substr_ctl _ctl vs. _sgr
#' @param which character, deprecated in favor of `ctl`.
#' @return logical of same length as `x`; NA values in `x` result in NA values
#'   in return
#' @examples
#' has_ctl("hello world")
#' has_ctl("hello\nworld")
#' has_ctl("hello\nworld", "sgr")
#' has_ctl("hello\033[31mworld\033[m", "sgr")
#' has_sgr("hello\033[31mworld\033[m")
#' has_sgr("hello\nworld")

has_ctl <- function(x, ctl='all', warn=getOption('fansi.warn'), which) {
  if(!missing(which)) {
    message("Parameter `which` has been deprecated; use `ctl` instead.")
    ctl <- which
  }
  args <- validate(x=x, ctl=ctl, warn=warn)
  if(length(ctl.int)) {
    with(args, .Call(FANSI_has_csi, x, ctl.int, warn))
  } else rep(FALSE, length(x))
}
#' @export
#' @rdname has_ctl

has_sgr <- function(x, warn=getOption('fansi.warn'))
  has_ctl(x, ctl="sgr", warn=warn)
