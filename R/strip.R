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

#' Strip Ansi Escape Sequences
#'
#' ...explain exactly what gets striped...
#'
#' @inheritParams substr_esc
#' @param what character(1L) in:
#'   * "most": strip all "C0" control characters except newlines, and all
#'     other escape sequences.
#'   * "all": strip all "C0" control characters and other other escape
#'     sequences.
#'   * "esc": strip escape sequences.
#' @return character vector of same length as x with ANSI escape sequences
#'   stripped
#' @export

strip_esc <- function(x, what='most', warn=getOption('fansi.warn')) {
  vetr(warn=LGL.1, what=CHR.1 && . %in% c("most", "all", "esc"))
  .Call(FANSI_strip_csi, x, warn)
}

## Process String by Removing Unwanted Characters
##
## This is to simulate what `strwrap` does

process <- function(x) .Call(FANSI_process, x)

