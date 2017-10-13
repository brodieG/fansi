# Copyright (C) 2017  Brodie Gaslam
#
# This file is part of "fansi - ANSI-aware String Functions"
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# Go to <https://www.r-project.org/Licenses/GPL-2> for a copy of the license.

#' Compute String ANSI State at a Given Position
#'
#' @export

ansi_state <- function(text, pos) {
  stopifnot(
    is.character(text), length(text) == 1L,
    is.numeric(pos), min(pos, 0L, na.rm=TRUE) >= 0L
  )
  .Call(
    "FANSI_state_at_raw_pos_ext", text, as.integer(pos) - 1L,
    PACKAGE = "fansi"
  )
}
#' Alternate substr version
#'
#' @export

ansi_substr2 <- function(x, start, stop) {
  x <- as.character(x)
  x.len <- length(x)

  # Add special case for length(x) == 1

  # Silently recycle start/stop like substr does

  start <- rep(as.integer(start), length.out=x.len)
  stop <- rep(as.integer(stop), length.out=x.len)

  # For each unique string, compute the state at each start and stop position
  # and re-map the positions to "ansi" space

  res <- character(length(x))
  x.u <- unique(x)

  for(u in x.u) {
    elems <- which(x == u)
    e.start <- start[elems]
    e.stop <- stop[elems]
    e.sort <- unique.default(sort.int(c(e.start, e.stop), method='shell'))
    state <- ansi_state(u, e.sort)

    # if any positions are greater than max position set them to those

    max.pos <- max(state[[2]][2, ])
    e.start[e.start > max.pos] <- max.pos
    e.stop[e.stop > max.pos] <- max.pos

    start.ansi.idx <- match(e.start, state[[2]][2, ])
    stop.ansi.idx <- match(e.stop, state[[2]][2, ])
    start.ansi <- state[[2]][3, start.ansi.idx]
    stop.ansi <- state[[2]][3, stop.ansi.idx]
    start.tag <- state[[1]][start.ansi.idx]

    res[elems] <- paste0(
      start.tag, substr(x[elems], start.ansi, stop.ansi), '\033[0m'
    )
  }
  res
}
