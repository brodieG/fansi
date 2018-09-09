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
#' slightly slower than `substr`.  ANSI CSI SGR sequences will be included in
#' the substrings to reflect the format of the substring when it was embedded in
#' the source string.  Additionally, other _Control Sequences_ specified in
#' `ctl` are treated as zero-width.
#'
#' `substr2_ctl` and `substr2_sgr` add the ability to retrieve substrings based
#' on display width, and byte width in addition to the normal character width.
#' `substr2_ctl` also provides the option to convert tabs to spaces with
#' [tabs_as_spaces] prior to taking substrings.
#'
#' Because exact substrings on anything other than character width cannot be
#' guaranteed (e.g. as a result of multi-byte encodings, or double display-width
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
#' @section _ctl vs. _sgr:
#'
#' The `*_ctl` versions of the functions treat all _Control Sequences_ specially
#' by default.  Special treatment is context dependent, and may include
#' detecting them and/or computing their display/character width as zero.  For
#' the SGR subset of the ANSI CSI sequences, `fansi` will also parse, interpret,
#' and reapply the text styles they encode if needed.  You can modify whether a
#' _Control Sequence_ is treated specially with the `ctl` parameter.  You can
#' exclude a type of _Control Sequence_ from special treatment by combining
#' "all" with that type of sequence (e.g. `ctl=c("all", "nl")` for special
#' treatment of all _Control Sequences_ **but** newlines).  The `*_sgr` versions
#' only treat ANSI CSI SGR sequences specially, and are equivalent to the
#' `*_ctl` versions with the `ctl` parameter set to "sgr".
#'
#' @note Non-ASCII strings are converted to and returned in UTF-8 encoding.
#' @inheritParams base::substr
#' @export
#' @seealso [fansi] for details on how _Control Sequences_ are
#'   interpreted, particularly if you are getting unexpected results.
#' @param x a character vector or object that can be coerced to character.
#' @param type character(1L) partial matching `c("chars", "width")`, although
#'   `type="width"` only works correctly with R >= 3.2.2.
#' @param round character(1L) partial matching
#'   `c("start", "stop", "both", "neither")`, controls how to resolve
#'   ambiguities when a `start` or `stop` value in "width" `type` mode falls
#'   within a multi-byte character or a wide display character.  See details.
#' @param tabs.as.spaces FALSE (default) or TRUE, whether to convert tabs to
#'   spaces.  This can only be set to TRUE if `strip.spaces` is FALSE.
#' @param tab.stops integer(1:n) indicating position of tab stops to use
#'   when converting tabs to spaces.  If there are more tabs in a line than
#'   defined tab stops the last tab stop is re-used.  For the purposes of
#'   applying tab stops, each input line is considered a line and the character
#'   count begins from the beginning of the input line.
#' @param ctl character, which _Control Sequences_ should be treated
#'   specially. See the "_ctl vs. _sgr" section for details.
#'
#'   * "nl": newlines.
#'   * "c0": all other "C0" control characters (i.e. 0x01-0x1f, 0x7F), except
#'     for newlines and the actual ESC (0x1B) character.
#'   * "sgr": ANSI CSI SGR sequences.
#'   * "csi": all non-SGR ANSI CSI sequences.
#'   * "esc": all other escape sequences.
#'   * "all": all of the above, except when used in combination with any of the
#'     above, in which case it means "all but".
#' @param warn TRUE (default) or FALSE, whether to warn when potentially
#'   problematic _Control Sequences_ are encountered.  These could cause the
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
#'
#' ## the _sgr variety only treat as special CSI SGR,
#' ## compare the following:
#'
#' substr_sgr("\033[31mhello\tworld", 1, 6)
#' substr_ctl("\033[31mhello\tworld", 1, 6)
#' substr_ctl("\033[31mhello\tworld", 1, 6, ctl=c('all', 'c0')

substr_ctl <- function(
  x, start, stop,
  warn=getOption('fansi.warn'),
  term.cap=getOption('fansi.term.cap'),
  ctl='all'
)
  substr2_ctl(
    x=x, start=start, stop=stop, warn=warn, term.cap=term.cap, ctl=ctl
  )

#' @rdname substr_ctl
#' @export

substr2_ctl <- function(
  x, start, stop, type='chars', round='start',
  tabs.as.spaces=getOption('fansi.tabs.as.spaces'),
  tab.stops=getOption('fansi.tab.stops'),
  warn=getOption('fansi.warn'),
  term.cap=getOption('fansi.term.cap'),
  ctl='all'
) {
  if(!is.character(x)) x <- as.character(x)
  x <- enc2utf8(x)
  if(any(Encoding(x) == "bytes"))
    stop("BYTE encoded strings are not supported.")

  if(!is.logical(tabs.as.spaces)) tabs.as.spaces <- as.logical(tabs.as.spaces)
  if(length(tabs.as.spaces) != 1L || is.na(tabs.as.spaces))
    stop("Argument `tabs.as.spaces` must be TRUE or FALSE.")
  if(!is.numeric(tab.stops) || !length(tab.stops) || any(tab.stops < 1))
    stop("Argument `tab.stops` must be numeric and strictly positive")

  if(!is.logical(warn)) warn <- as.logical(warn)
  if(length(warn) != 1L || is.na(warn))
    stop("Argument `warn` must be TRUE or FALSE.")

  if(!is.character(term.cap))
    stop("Argument `term.cap` must be character.")
  if(anyNA(term.cap.int <- match(term.cap, VALID.TERM.CAP)))
    stop(
      "Argument `term.cap` may only contain values in ",
      deparse(VALID.TERM.CAP)
    )
  if(!is.character(ctl))
    stop("Argument `ctl` must be character.")
  ctl.int <- integer()
  if(length(ctl)) {
    # duplicate values in `ctl` are okay, so save a call to `unique` here
    if(anyNA(ctl.int <- match(ctl, VALID.CTL)))
      stop(
        "Argument `ctl` may contain only values in `",
        deparse(VALID.CTL), "`"
      )
  }

  valid.round <- c('start', 'stop', 'both', 'neither')
  if(
    !is.character(round) || length(round) != 1 ||
    is.na(round.int <- pmatch(round, valid.round))
  )
    stop("Argument `round` must partial match one of ", deparse(valid.round))

  round <- valid.round[round.int]

  valid.types <- c('chars', 'width')
  if(
    !is.character(type) || length(type) != 1 ||
    is.na(type.int <- pmatch(type, valid.types))
  )
    stop("Argument `type` must partial match one of ", deparse(valid.types))

  type.m <- type.int - 1L
  x.len <- length(x)

  # Silently recycle start/stop like substr does

  start <- rep(as.integer(start), length.out=x.len)
  stop <- rep(as.integer(stop), length.out=x.len)
  start[start < 1L] <- 1L

  res <- x
  no.na <- !(is.na(x) | is.na(start & stop))

  res[no.na] <- substr_ctl_internal(
    x[no.na], start=start[no.na], stop=stop[no.na],
    type.int=type.m,
    tabs.as.spaces=tabs.as.spaces, tab.stops=tab.stops, warn=warn,
    term.cap.int=term.cap.int,
    round.start=round == 'start' || round == 'both',
    round.stop=round == 'stop' || round == 'both',
    x.len=length(x),
    ctl.int=ctl.int
  )
  res[!no.na] <- NA_character_
  res
}
#' @rdname substr_ctl
#' @export

substr_sgr <- function(
  x, start, stop,
  warn=getOption('fansi.warn'),
  term.cap=getOption('fansi.term.cap')
)
  substr2_ctl(
    x=x, start=start, stop=stop, warn=warn, term.cap=term.cap, ctl='sgr'
  )

#' @rdname substr_ctl
#' @export

substr2_sgr <- function(
  x, start, stop, type='chars', round='start',
  tabs.as.spaces=getOption('fansi.tabs.as.spaces'),
  tab.stops=getOption('fansi.tab.stops'),
  warn=getOption('fansi.warn'),
  term.cap=getOption('fansi.term.cap')
)
  substr2_ctl(
    x=x, start=start, stop=stop, type=type, round=round,
    tabs.as.spaces=tabs.as.spaces,
    tab.stops=tab.stops, warn=warn, term.cap=term.cap, ctl='sgr'
  )

## Lower overhead version of the function for use by strwrap
##
## @x must already have been converted to UTF8
## @param type.int is supposed to be the matched version of type, minus 1

substr_ctl_internal <- function(
  x, start, stop, type.int, round, tabs.as.spaces,
  tab.stops, warn, term.cap.int, round.start, round.stop,
  x.len, ctl.int
) {
  # For each unique string, compute the state at each start and stop position
  # and re-map the positions to "ansi" space

  if(tabs.as.spaces)
    x <- .Call(FANSI_tabs_as_spaces, x, tab.stops, warn, term.cap.int, ctl.int)

  res <- character(x.len)
  s.s.valid <- stop >= start & stop

  x.scalar <- length(x) == 1
  x.u <- if(x.scalar) x else unique_chr(x)

  for(u in x.u) {
    elems <- which(x == u & s.s.valid)
    elems.len <- length(elems)
    e.start <- start[elems]
    e.stop <- stop[elems]
    x.elems <- if(x.scalar) rep(x, length.out=elems.len) else x[elems]

    # note, for expediency we're currently assuming that there is no overlap
    # between starts and stops

    e.order <- forder(c(e.start, e.stop))

    e.lag <- rep(c(round.start, round.stop), each=elems.len)[e.order]
    e.ends <- rep(c(FALSE, TRUE), each=elems.len)[e.order]
    e.sort <- c(e.start, e.stop)[e.order]

    state <- .Call(
      FANSI_state_at_pos_ext,
      u, e.sort - 1L, type.int,
      e.lag, e.ends,
      warn, term.cap.int,
      ctl.int
    )
    # Recover the matching values for e.sort

    e.unsort.idx <- match(seq_along(e.order), e.order)
    start.stop.ansi.idx <- .Call(FANSI_cleave, e.unsort.idx)
    start.ansi.idx <- start.stop.ansi.idx[[1L]]
    stop.ansi.idx <- start.stop.ansi.idx[[2L]]

    # And use those to substr with

    start.ansi <- state[[2]][3, start.ansi.idx]
    stop.ansi <- state[[2]][3, stop.ansi.idx]
    start.tag <- state[[1]][start.ansi.idx]
    stop.tag <- state[[1]][stop.ansi.idx]

    # if there is any ANSI CSI at end then add a terminating CSI

    end.csi <- character(length(start.tag))
    end.csi[nzchar(stop.tag)] <- '\033[0m'

    res[elems] <- paste0(
      start.tag, substr(x.elems, start.ansi, stop.ansi), end.csi
    )
  }
  res
}

## Need to expose this so we can test bad UTF8 handling because substr will
## behave different with bad UTF8 pre and post R 3.6.0

state_at_pos <- function(x, starts, ends, warn=getOption('fansi.warn')) {
  is.start <- c(rep(TRUE, length(starts)), rep(FALSE, length(ends)))
  .Call(
    FANSI_state_at_pos_ext,
    x, as.integer(c(starts, ends)) - 1L,
    0L,      # character type
    is.start,  # lags
    !is.start, # ends
    warn,
    seq_along(VALID.TERM.CAP),
    seq_along(VALID.CTL)
  )
}
