# Copyright (C) 2018  Brodie Gaslam
#
# This file is part of "fansi - ANSI Escape Aware String Functions"
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

state_esc <- function(
  text, pos, type='chars', lag, ends, tabs.as.spaces=FALSE, tab.stops=8L
) {
  stopifnot(
    is.character(text), length(text) == 1L,
    is.numeric(pos), min(pos, 0L, na.rm=TRUE) >= 0L,
    is.character(type),
    !is.na(type.match <- match(type, c('chars', 'width', 'bytes')))
  )
  .Call(
    FANSI_state_at_pos_ext, text, as.integer(pos) - 1L, type.match - 1L,
    lag, ends, tabs.as.spaces, tab.stops
  )
}
#' ANSI Escape Sequence Aware Version of `substr`
#'
#' `substr_esc` is a drop-in replacement for `substr`.  Performance is
#' slightly slower than `substr`.
#'
#' `substr2_esc` adds the ability to retrieve substrings based on display width,
#' and byte width in addition to the normal character width.  #'
#' `substr2_esc` also provides the option to convert tabs to spaces with
#' [tabs_as_spaces] prior to taking substrings.
#
#' Because exact substrings on anything other than character width cannot be
#' guaranteed (e.g.  because of multi-byte encodings, or double display-width
#' characters) `substr2_esc` must make assumptions on how to resolve provided
#' `start`/`stop` values that are infeasible and does so via the `round`
#' parameter.  If we use "start" as the `round` value, then any time the `start`
#' value corresponds to the middle of a multi-byte or a wide character, then
#' that character is included in the substring, while any similar partially
#' included character via the `stop` is left out.  The converse is true if we
#' use "stop" as the `round` value.  "neither" would cause all partial
#' characters to be dropped irrespective whether they correspond to `start` or
#' `stop`, and "both" could cause all of them to be included.
#'
#' @inheritParams base::substr
#' @inheritParams tabs_as_spaces
#' @param type character(1L) in `c("char", "width", "bytes")`
#' @param round character(1L) in `c("start", "stop", "both", "neither")`,
#'   controls how to resolve ambiguities when a `start` or `stop` value in
#'   "bytes" or "width" `type` mode falls within a multi-byte character or a
#'   wide display character.  See details.
#' @export

substr_esc <- function(x, start, stop) substr2_esc(x=x, start=start, stop=stop)

#' @rdname substr_esc
#' @export

substr2_esc <- function(
  x, start, stop, type='chars', round='first', tabs.as.spaces=FALSE,
  tab.stops=getOption('fansi.tab.stops')
) {
  x <- as.character(x)
  vetr(
    character(), integer(), integer(),
    type=CHR.1 && . %in% c('chars', 'bytes', 'width'),
    round=CHR.1 && . %in% c('start', 'stop', 'both', 'neither'),
    tabs.as.spaces=LGL.1, tab.stops=INT && length(.) >= 1L
  )
  x.len <- length(x)

  # Add special case for length(x) == 1

  # Silently recycle start/stop like substr does

  start <- rep(as.integer(start), length.out=x.len)
  stop <- rep(as.integer(stop), length.out=x.len)

  # For each unique string, compute the state at each start and stop position
  # and re-map the positions to "ansi" space

  res <- character(length(x))
  x.u <- unique(x)
  offset <- c(chars=2, bytes=1, width=4)[type]

  for(u in x.u) {
    elems <- which(x == u)
    e.start <- start[elems]
    e.stop <- stop[elems]
    # note, for expediency we're currently assuming that there is no overlap
    # between starts and stops

    e.order <- order(c(e.start, e.stop), method='shell')

    e.lag <- c(
      rep(round %in% c('start', 'both'), length(start)),
      rep(round %in% c('stop', 'both'), length(stop))
    )[e.order]

    e.ends <- c(rep(FALSE, length(start)), rep(TRUE, length(start)))[e.order]
    e.sort <- c(e.start, e.stop)[e.order]

    state <- state_esc(
      u, e.sort, type, e.lag, e.ends, tabs.as.spaces=tabs.as.spaces,
      tab.stops=tab.stops
    )
    # Recover the matching values for e.sort

    e.unsort.idx <- match(seq_along(e.order), e.order)
    start.ansi.idx <- head(e.unsort.idx, length(e.start))
    stop.ansi.idx <- tail(e.unsort.idx, length(e.stop))

    # And use those to substr with

    start.ansi <- state[[2]][3, start.ansi.idx]
    stop.ansi <- state[[2]][3, stop.ansi.idx]
    start.tag <- state[[1]][start.ansi.idx]
    stop.tag <- state[[1]][stop.ansi.idx]

    # if there is any ANSI CSI then add a terminating CSI

    end.csi <- character(length(start.tag))
    end.csi[nzchar(start.tag) | nzchar(stop.tag)] <- '\033[0m'

    res[elems] <- paste0(
      start.tag, substr(x[elems], start.ansi, stop.ansi), end.csi
    )
  }
  res
}

