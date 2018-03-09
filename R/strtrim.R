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

#' ANSI Control Sequence Aware Version of strtrim
#'
#' One difference with [base::strtrim] is that all C0 control characters such as
#' newlines, carriage returns, etc., are treated as zero width.
#'
#' `strtrim2_ctl` adds the option of converting tabs to spaces before trimming.
#' This is the only difference between `strtrim_ctl` and `strtrim2_ctl`.
#'
#' @export
#' @seealso [fansi] for details on how _Control Sequences_ are
#'   interpreted, particularly if you are getting unexpected results.
#' @inheritParams base::strtrim
#' @inheritParams strwrap_ctl
#' @examples
#' strtrim_ctl("\033[42mHello world\033[m", 6)

strtrim_ctl <- function(x, width, warn=getOption('fansi.warn')){
  if(!is.character(x)) x <- as.character(x)
  vetr(width=NUM.1.POS && . >= 1, warn=LGL.1)

  # can assume all term cap available for these purposes

  term.cap.int <- seq_along(VALID.TERM.CAP)
  width <- as.integer(width)

  # a bit inefficient to rely on strwrap, but oh well

  res <- .Call(
    FANSI_strwrap_csi,
    x, width,
    0L, 0L,    # indent, exdent
    "", "",    # prefix, initial
    TRUE, "",  # wrap always
    FALSE,     # strip spaces
    FALSE, 8L,
    warn, term.cap.int,
    TRUE       # first only
  )
  res
}
#' @export
#' @rdname strtrim_ctl

strtrim2_ctl <- function(
  x, width, warn=getOption('fansi.warn'), tabs.as.spaces=FALSE, tab.stops=8L
) {
  if(!is.character(x)) x <- as.character(x)
  vetr(width=NUM.1.POS && . >= 1, warn=LGL.1)

  # can assume all term cap available for these purposes

  term.cap.int <- seq_along(VALID.TERM.CAP)
  width <- as.integer(width)
  tab.stops <- as.integer(tab.stops)

  # a bit inefficient to rely on strwrap, but oh well

  res <- .Call(
    FANSI_strwrap_csi,
    x, width,
    0L, 0L,    # indent, exdent
    "", "",    # prefix, initial
    TRUE, "",  # wrap always
    FALSE,     # strip spaces
    tabs.as.spaces, tab.stops,
    warn, term.cap.int,
    TRUE       # first only
  )
  res
}
