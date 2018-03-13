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

#' ANSI Control Sequence Aware Version of substr
#'
#' `substr_ctl` is a drop-in replacement for `substr`.  Performance is
#' slightly slower than `substr`.
#'
#' `substr2_ctl` adds the ability to retrieve substrings based on display width,
#' and byte width in addition to the normal character width.  `substr2_ctl` also
#' provides the option to convert tabs to spaces with [tabs_as_spaces] prior to
#' taking substrings.
#
#' Because exact substrings on anything other than character width cannot be
#' guaranteed (e.g.  because of multi-byte encodings, or double display-width
#' characters) `substr2_ctl` must make assumptions on how to resolve provided
#' `start`/`stop` values that are infeasible and does so via the `round`
#' parameter.
#'
#' If we use "start" as the `round` value, then any time the `start`
#' value corresponds to the middle of a multi-byte or a wide character, then
#' that character is included in the substring, while any similar partially
#' included character via the `stop` is left out.  The converse is true if we
#' use "stop" as the `round` value.  "neither" would cause all partial
#' characters to be dropped irrespective whether they correspond to `start` or
#' `stop`, and "both" could cause all of them to be included.
#'
#' @inheritParams base::substr
#' @inheritParams tabs_as_spaces
#' @export
#' @seealso [fansi] for details on how _Control Sequences_ are
#'   interpreted, particularly if you are getting unexpected results.
#' @param type character(1L) in `c("char", "width")`
#' @param round character(1L) in `c("start", "stop", "both", "neither")`,
#'   controls how to resolve ambiguities when a `start` or `stop` value in
#'   "width" `type` mode falls within a multi-byte character or a wide display
#'   character.  See details.
#' @param tabs.as.spaces FALSE (default) or TRUE, whether to convert tabs to
#'   spaces.  This can only be set to TRUE if `strip.spaces` is FALSE.
#' @param warn TRUE (default) or FALSE, whether to warn when potentially
#'   problematic escape sequences are encountered.  These could cause the
#'   assumptions `fansi` makes about how strings are rendered on your display
#'   to be incorrect, for example by moving the cursor (see [fansi]).
#' @param term.cap character a vector of the capabilities of the terminal, can
#'   be any combination "bright" (SGR codes 90-97, 100-107), "256" (SGR codes
#'   starting with "38;5" or "48;5"), and "truecolor" (SGR codes starting with
#'   "38;2" or "48;2"). Changing this parameter changes how `fansi` interprets
#'   escape sequences, so you should ensure that it matches your terminal
#'   capabilities. See [term_cap_test] for details.
#' @examples
#' substr_ctl("\033[42mhello\033[m world", 1, 9)
#' substr_ctl("\033[42mhello\033[m world", 3, 9)
#'
#' ## Width 2 and 3 are in the middle of an ideogram as
#' ## start and stop positions respectively, so we control
#' ## what we get with `round`
#'
#' cn.string <- paste0("\033[42m", "\u4E00\u4E01\u4E03", "\033[m")
#'
#' substr2_ctl(cn.string, 2, 3, type='width')
#' substr2_ctl(cn.string, 2, 3, type='width', round='both')
#' substr2_ctl(cn.string, 2, 3, type='width', round='start')
#' substr2_ctl(cn.string, 2, 3, type='width', round='stop')

substr_ctl <- function(
  x, start, stop,
  warn=getOption('fansi.warn'),
  term.cap=getOption('fansi.term.cap')
) substr2_ctl(x=x, start=start, stop=stop, warn=warn, term.cap=term.cap)

#' @importFrom utils head tail
#' @rdname substr_ctl
#' @export

substr2_ctl <- function(
  x, start, stop, type='chars', round='start', tabs.as.spaces=FALSE,
  tab.stops=getOption('fansi.tab.stops'),
  warn=getOption('fansi.warn'),
  term.cap=getOption('fansi.term.cap')
) {
  x <- enc2utf8(as.character(x))
  vetr(
    character(), start=numeric() && !anyNA(.), stop=NUM,
    type=CHR.1 && . %in% c('chars', 'width'),
    round=CHR.1 && . %in% c('start', 'stop', 'both', 'neither'),
    tabs.as.spaces=LGL.1, tab.stops=INT && length(.) >= 1L,
    warn=LGL.1, term.cap=CHR
  )
  if(anyNA(term.cap.int <- match(term.cap, VALID.TERM.CAP)))
    stop(
      "Argument `term.cap` may only contain values in ",
      deparse(VALID.TERM.CAP)
    )
  x.len <- length(x)

  # Add special case for length(x) == 1

  # Silently recycle start/stop like substr does

  start <- rep(as.integer(start), length.out=x.len)
  stop <- rep(as.integer(stop), length.out=x.len)
  start[start < 1L] <- 1L
  s.s.valid <- stop >= start & stop

  # For each unique string, compute the state at each start and stop position
  # and re-map the positions to "ansi" space

  res <- character(length(x))
  x.u <- unique_chr(x)
  type.m <- match(type, c('chars', 'width'))

  for(u in x.u) {
    elems <- which(x == u & s.s.valid)
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

    state <- .Call(
      FANSI_state_at_pos_ext,
      u, e.sort - 1L, type.m - 1L,
      e.lag, e.ends,
      tabs.as.spaces, tab.stops,
      warn, term.cap.int
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

    # if there is any ANSI CSI at end then add a terminating CSI

    end.csi <- character(length(start.tag))
    end.csi[nzchar(stop.tag)] <- '\033[0m'

    res[elems] <- paste0(
      start.tag, substr(x[elems], start.ansi, stop.ansi), end.csi
    )
  }
  res
}

