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

#' Control Sequence Aware Version of substr
#'
#' `substr_ctl` is a drop-in replacement for `substr`.  Performance is
#' slightly slower than `substr`.  CSI SGR sequences will be included in the
#' substrings to reflect the format of the substring when it was embedded in
#' the source string.  Additionally, other _Control Sequences_ specified in
#' `ctl` are treated as zero-width.
#'
#' `substr2_ctl` adds the ability to retrieve substrings based on display width
#' in addition to the normal character width.  `substr2_ctl` also provides the
#' option to convert tabs to spaces with [`tabs_as_spaces`] prior to taking
#' substrings.
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
#' These functions map string lengths accounting for CSI SGR sequence semantics
#' to the naive length calculations, and then use the mapping in conjunction
#' with [base::substr()] to extract the string.  This concept is borrowed
#' directly from Gábor Csárdi's `crayon` package, although the implementation of
#' the calculation is different.
#'
#' Replacement functions are implemented as three substring operations, so:
#' ```
#' x <- "ABC"
#' y <- "_."
#' substr_ctl(x, 2, 2, ...) <- y
#' ```
#' Is treated roughly as:
#' ```
#' x <- paste0(
#'   substr(x, 1, 1, ...),
#'   substr(y, 1, 1, ...),
#'   substr(x, 3, 3, terminate=FALSE, ...)
#' )
#' ```
#' Except for the `terminate` parameter for the trailing substring, all other
#' parameters are passed from `substr_ctl<-` to the internal substring calls.
#' If you wish for the whole return value to be terminated you must manually add
#' terminating sequences.  `substr_ctl` refrains from doing so to maintain the
#' illusion of a string modified in place.
#'
#' Another implication of the three substring approach is that the `carry`
#' parameter causes state to carry within the original string and the
#' replacement values independently, as if they were columns of text cut from
#' different pages and pasted together.
#'
#' @note Non-ASCII strings are converted to and returned in UTF-8 encoding.
#'   Width calculations will not work properly in R < 3.2.2.
#' @note If `stop` < `start`, the return value is always an empty string.
#' @inheritParams base::substr
#' @export
#' @seealso [`?fansi`][fansi] for details on how _Control Sequences_ are
#'   interpreted, particularly if you are getting unexpected results,
#'   [`normalize_state`] for more details on what the `normalize` parameter does,
#'   [`state_at_end`] to compute active state at the end of strings,
#'   [`close_state`] to compute the sequence required to close active state.
#' @param x a character vector or object that can be coerced to such.
#' @param type character(1L) partial matching `c("chars", "width")`, although
#'   `type="width"` only works correctly with R >= 3.2.2.  See
#'   [`?nchar`][base::nchar]. With "width", the results might be affected by
#'   locale changes, Unicode database updates, and logic changes for processing
#'   of complex graphemes.  Generally you should not rely on a specific output
#'   e.g. by embedding it in unit tests.  For the most part `fansi` (currently)
#'   uses the internals of `base::nchar(type='width')`, but there are exceptions
#'   and this may change in the future.
#' @param round character(1L) partial matching
#'   `c("start", "stop", "both", "neither")`, controls how to resolve
#'   ambiguities when a `start` or `stop` value in "width" `type` mode falls
#'   within a wide display character.  See details.
#' @param tabs.as.spaces FALSE (default) or TRUE, whether to convert tabs to
#'   spaces.  This can only be set to TRUE if `strip.spaces` is FALSE.
#' @param tab.stops integer(1:n) indicating position of tab stops to use
#'   when converting tabs to spaces.  If there are more tabs in a line than
#'   defined tab stops the last tab stop is re-used.  For the purposes of
#'   applying tab stops, each input line is considered a line and the character
#'   count begins from the beginning of the input line.
#' @param ctl character, which _Control Sequences_ should be treated
#'   specially.  Special treatment is context dependent, and may include
#'   detecting them and/or computing their display/character width as zero.  For
#'   the SGR subset of the ANSI CSI sequences, and OSC-anchored URLs, `fansi`
#'   will also parse, interpret, and reapply the sequences as needed.  You can
#'   modify whether a _Control Sequence_ is treated specially with the `ctl`
#'   parameter.
#'
#'   * "nl": newlines.
#'   * "c0": all other "C0" control characters (i.e. 0x01-0x1f, 0x7F), except
#'     for newlines and the actual ESC (0x1B) character.
#'   * "sgr": ANSI CSI SGR sequences.
#'   * "csi": all non-SGR ANSI CSI sequences.
#'   * "url": OSC-anchored URLs
#'   * "osc": all non-OSC-anchored URL OSC sequences.
#'   * "esc": all other escape sequences.
#'   * "all": all of the above, except when used in combination with any of the
#'     above, in which case it means "all but".
#' @param warn TRUE (default) or FALSE, whether to warn when potentially
#'   problematic _Control Sequences_ are encountered.  These could cause the
#'   assumptions `fansi` makes about how strings are rendered on your display
#'   to be incorrect, for example by moving the cursor (see [`?fansi`][fansi]).
#'   If the problematic sequence is a tab, you can use the `tabs.as.spaces`
#'   parameter on functions that have it, or the `tabs_as_spaces` function, to
#'   turn the tabs to spaces and resolve the warning that way.
#' @param term.cap character a vector of the capabilities of the terminal, can
#'   be any combination of "bright" (SGR codes 90-97, 100-107), "256" (SGR codes
#'   starting with "38;5" or "48;5"), "truecolor" (SGR codes starting with
#'   "38;2" or "48;2"), and "all". Changing this parameter changes how `fansi`
#'   interprets escape sequences, so you should ensure that it matches your
#'   terminal capabilities. See [`term_cap_test`] for details.  "all" behaves as
#'   it does for the `ctl` parameter: "all" combined with any other value means
#'   all terminal capabilities except that one.
#' @param normalize TRUE or FALSE (default) whether SGR sequence should be
#'   normalized out such that there is one distinct sequence for each SGR code.
#'   normalized strings will occupy more space (e.g. "\033[31;42m" becomes
#'   "\033[31m\033[42m"), but will work better with code that assumes each SGR
#'   code will be in its own escape as `crayon` does.
#' @param carry TRUE, FALSE (default), or a scalar string, controls whether to
#'   interpret the character vector as a "single document" (TRUE or string) or
#'   as independent elements (FALSE).  In "single document" mode, active state
#'   at the end of an input element is considered active at the beginning of the
#'   next vector element, simulating what happens with a document with active
#'   state at the end of a line.  If FALSE each vector element is interpreted as
#'   if there were no active state when it begins.  If character, then the
#'   active state at the end of the `carry` string is carried into the first
#'   element of `x`.  See the "State Interactions" section of [`?fansi`][fansi]
#'   for details.
#' @param terminate TRUE (default) or FALSE whether substrings should have
#'   active state closed to avoid it bleeding into other strings they may be
#'   prepended onto.  This does not stop state from carrying if `carry = TRUE`.
#'   See the "State Interactions" section of [`?fansi`][fansi] for details.
#' @param value a character vector or object that can be coerced to such.
#' @return a character vector of the same length and with the same attributes as
#'   x (after possible coercion and re-encoding to UTF-8).
#' @examples
#' substr_ctl("\033[42mhello\033[m world", 1, 9)
#' substr_ctl("\033[42mhello\033[m world", 3, 9)
#'
#' ## Width 2 and 3 are in the middle of an ideogram as
#' ## start and stop positions respectively, so we control
#' ## what we get with `round`
#' cn.string <- paste0("\033[42m", "\u4E00\u4E01\u4E03", "\033[m")
#' substr2_ctl(cn.string, 2, 3, type='width')
#' substr2_ctl(cn.string, 2, 3, type='width', round='both')
#' substr2_ctl(cn.string, 2, 3, type='width', round='start')
#' substr2_ctl(cn.string, 2, 3, type='width', round='stop')
#'
#' ## We can specify which escapes are considered special:
#' substr_ctl("\033[31mhello\tworld", 1, 6, ctl='sgr', warn=FALSE)
#' substr_ctl("\033[31mhello\tworld", 1, 6, ctl=c('all', 'c0'), warn=FALSE)
#'
#' ## `carry` allows SGR to carry from one element to the next
#' substr_ctl(c("\033[33mhello", "world"), 1, 3)
#' substr_ctl(c("\033[33mhello", "world"), 1, 3, carry=TRUE)
#' substr_ctl(c("\033[33mhello", "world"), 1, 3, carry="\033[44m")
#'
#' ## We can omit the termination
#' bleed <- substr_ctl(c("\033[41mhello", "world"), 1, 3, terminate=FALSE)
#' writeLines(bleed)      # Style will bleed out of string
#' end <- "\033[0m\n"
#' writeLines(end)        # Stanch bleeding
#'
#' ## Replacement functions
#' x0<- x1 <- x2 <- x3 <- c("\033[42mABC", "\033[34mDEF")
#' substr_ctl(x1, 2, 2) <- "_"
#' substr_ctl(x2, 2, 2) <- "\033[m_"
#' substr_ctl(x3, 2, 2) <- "\033[45m_"
#' writeLines(c(x0, end, x1, end, x2, end, x3, end))
#'
#' ## With `carry = TRUE` strings look like original
#' x0<- x1 <- x2 <- x3 <- c("\033[42mABC", "\033[34mDEF")
#' substr_ctl(x0, 2, 2, carry=TRUE) <- "_"
#' substr_ctl(x1, 2, 2, carry=TRUE) <- "\033[m_"
#' substr_ctl(x2, 2, 2, carry=TRUE) <- "\033[45m_"
#' writeLines(c(x0, end, x1, end, x2, end, x3, end))

substr_ctl <- function(
  x, start, stop,
  warn=getOption('fansi.warn'),
  term.cap=getOption('fansi.term.cap'),
  ctl='all', normalize=getOption('fansi.normalize', FALSE),
  carry=getOption('fansi.carry', FALSE),
  terminate=getOption('fansi.terminate', TRUE)
)
  substr2_ctl(
    x=x, start=start, stop=stop, warn=warn, term.cap=term.cap, ctl=ctl,
    normalize=normalize, carry=carry, terminate=terminate
  )

#' @rdname substr_ctl
#' @export

substr2_ctl <- function(
  x, start, stop, type='chars', round='start',
  tabs.as.spaces=getOption('fansi.tabs.as.spaces'),
  tab.stops=getOption('fansi.tab.stops'),
  warn=getOption('fansi.warn'),
  term.cap=getOption('fansi.term.cap'),
  ctl='all', normalize=getOption('fansi.normalize', FALSE),
  carry=getOption('fansi.carry', FALSE),
  terminate=getOption('fansi.terminate', TRUE)
) {
  ## modifies / creates NEW VARS in fun env
  VAL_IN_ENV(
    x=x, warn=warn, term.cap=term.cap, ctl=ctl, normalize=normalize,
    carry=carry, terminate=terminate, tab.stops=tab.stops,
    tabs.as.spaces=tabs.as.spaces, type=type, round=round,
    start=start, stop=stop
  )
  res <- x
  no.na <- !(is.na(x) | is.na(start & stop))

  res[no.na] <- substr_ctl_internal(
    x[no.na], start=start[no.na], stop=stop[no.na],
    type.int=TYPE.INT,
    tabs.as.spaces=tabs.as.spaces, tab.stops=tab.stops, warn=warn,
    term.cap.int=TERM.CAP.INT,
    round.start=round == 'start' || round == 'both',
    round.stop=round == 'stop' || round == 'both',
    x.len=X.LEN,
    ctl.int=CTL.INT, normalize=normalize,
    carry=carry, terminate=terminate
  )
  res[!no.na] <- NA_character_
  res
}
#' @rdname substr_ctl
#' @export

`substr_ctl<-` <- function(
  x, start, stop, value,
  warn=getOption('fansi.warn'),
  term.cap=getOption('fansi.term.cap'),
  ctl='all', normalize=getOption('fansi.normalize', FALSE),
  carry=getOption('fansi.carry', FALSE),
  terminate=getOption('fansi.terminate', TRUE)
) {
  substr2_ctl(
    x=x, start=start, stop=stop, warn=warn, term.cap=term.cap, ctl=ctl,
    normalize=normalize, carry=carry, terminate=terminate
  ) <- value
  x
}
#' @rdname substr_ctl
#' @export

`substr2_ctl<-` <- function(
  x, start, stop, value, type='chars', round='start',
  tabs.as.spaces=getOption('fansi.tabs.as.spaces'),
  tab.stops=getOption('fansi.tab.stops'),
  warn=getOption('fansi.warn'),
  term.cap=getOption('fansi.term.cap'),
  ctl='all', normalize=getOption('fansi.normalize', FALSE),
  carry=getOption('fansi.carry', FALSE),
  terminate=getOption('fansi.terminate', TRUE)
) {
  ## modifies / creates NEW VARS in fun env
  VAL_IN_ENV(
    x=x, warn=warn, term.cap=term.cap, ctl=ctl, normalize=normalize,
    carry=carry, terminate=terminate, tab.stops=tab.stops,
    tabs.as.spaces=tabs.as.spaces, round=round, start=start, stop=stop,
    type=type
  )
  # Need to translate start/stop and remap round
  round.a <- switch(
    round, start='stop', stop='start', both='neither', neither='both'
  )
  round.b <- round

  # Adjust `stop` to be no longer than end of string, also need to make sure the
  # overall string length is unchanged.
  nc <- nchar_ctl(x, type=type, ctl=ctl, warn=warn)
  stop <- pmin(stop, nc)
  value <- rep_len(enc2utf8(as.character(value)), X.LEN)
  ncv <- nchar_ctl(value, type=type, ctl=ctl, warn=warn)

  # All warnings should have been emitted by `nchar_ctl` above
  x[] <- paste0(
    substr_ctl_internal(
      x, rep(1L, X.LEN), start - 1L, type.int=TYPE.INT,
      round.start=round.a == 'start' || round.a == 'both',
      round.stop=round.a == 'stop' || round.a == 'both',
      tabs.as.spaces=tabs.as.spaces, tab.stops=tab.stops, warn=FALSE,
      term.cap.int=TERM.CAP.INT, ctl.int=CTL.INT, normalize=normalize,
      carry=carry, terminate=terminate
    ),
    substr_ctl_internal(
      value, rep(1L, X.LEN), stop - start + 1L,
      type.int=TYPE.INT,
      round.start=round == 'start' || round == 'both',
      round.stop=round == 'stop' || round == 'both',
      tabs.as.spaces=tabs.as.spaces, tab.stops=tab.stops, warn=FALSE,
      term.cap.int=TERM.CAP.INT, ctl.int=CTL.INT, normalize=normalize,
      carry=carry, terminate=terminate
    ),
    # This last one should not terminate ever as it preserves whatever the
    # original string did.
    substr_ctl_internal(
      x, pmin(stop + 1L, start + ncv),
      rep(.Machine[['integer.max']], X.LEN), type.int=TYPE.INT,
      round.start=round.b == 'start' || round.b == 'both',
      round.stop=round.b == 'stop' || round.b == 'both',
      tabs.as.spaces=tabs.as.spaces, tab.stops=tab.stops, warn=FALSE,
      term.cap.int=TERM.CAP.INT, ctl.int=CTL.INT, normalize=normalize,
      carry=carry, terminate=FALSE
    )
  )
  x
}
#' SGR Control Sequence Aware Version of substr
#'
#' These functions are deprecated in favor of the [`_ctl` flavors][substr_ctl].
#'
#' @keywords internal
#' @inheritParams substr_ctl
#' @inherit substr_ctl return
#' @export

substr_sgr <- function(
  x, start, stop,
  warn=getOption('fansi.warn'),
  term.cap=getOption('fansi.term.cap'),
  normalize=getOption('fansi.normalize', FALSE),
  carry=getOption('fansi.carry', FALSE),
  terminate=getOption('fansi.terminate', TRUE)
)
  substr2_ctl(
    x=x, start=start, stop=stop, warn=warn, term.cap=term.cap, ctl='sgr',
    normalize=normalize, carry=carry, terminate=terminate
  )

#' @rdname substr_sgr
#' @export

substr2_sgr <- function(
  x, start, stop, type='chars', round='start',
  tabs.as.spaces=getOption('fansi.tabs.as.spaces'),
  tab.stops=getOption('fansi.tab.stops'),
  warn=getOption('fansi.warn'),
  term.cap=getOption('fansi.term.cap'),
  normalize=getOption('fansi.normalize', FALSE),
  carry=getOption('fansi.carry', FALSE),
  terminate=getOption('fansi.terminate', TRUE)
)
  substr2_ctl(
    x=x, start=start, stop=stop, type=type, round=round,
    tabs.as.spaces=tabs.as.spaces,
    tab.stops=tab.stops, warn=warn, term.cap=term.cap, ctl=c('sgr', 'url'),
    normalize=normalize,
    carry=carry, terminate=terminate
  )

## @x must already have been converted to UTF8
## @param type.int is supposed to be the matched version of type, minus 1
##
## Increasingly, it seems trying to re-use the crayon method instead of doing
## everything in C was a big mistake...

substr_ctl_internal <- function(
  x, start, stop, type.int, round, tabs.as.spaces,
  tab.stops, warn, term.cap.int, round.start, round.stop,
  x.len, ctl.int, normalize, carry, terminate
) {
  # For each unique string, compute the state at each start and stop position
  # and re-map the positions to "ansi" space

  if(tabs.as.spaces)
    x <- .Call(FANSI_tabs_as_spaces, x, tab.stops, warn, term.cap.int, ctl.int)
  warn <- warn && !tabs.as.spaces

  res <- character(length(x))
  s.s.valid <- stop >= start & stop

  # If we want to carry, we'll do this manually as too much work to try to do it
  # in C given the current structure using ordered indices into each string.
  # Do before `unique` as this to equal strings may become different.

  x.carry <- character(length(x))
  if(!is.na(carry)) {
    ends <- .Call(
      FANSI_state_at_end, x, warn, term.cap.int, ctl.int, normalize, carry
    )
    x.carry <- c(carry, ends[-length(ends)])
    x <- paste0(x.carry, x)
  }
  warn <- warn && is.na(carry)

  # We compute style at each start and stop position by getting all those
  # positions into a vector and then ordering them by position, keeping track of
  # original order and whether they are starting or ending positions (affects
  # how multi-byte characters are trimmed/kept).

  # We do this for each unique string in `x` as the indices must be incrementing
  # for each of them.

  # x.scalar is likely needed for strsplit (but not sure, this is after the fact
  # documentation)
  x.scalar <- length(x) == 1
  x.u <- if(x.scalar) x else unique_chr(x)
  ids <- if(x.scalar) seq_along(s.s.valid) else seq_along(x)

  for(u in x.u) {
    elems <- which(x == u & s.s.valid)
    elems.len <- length(elems)
    # we want to specify minimum number of position/width elements
    e.start <- start[elems] - 1L
    e.stop <- stop[elems]
    e.ids <- ids[elems]
    x.elems <- if(x.scalar)
      rep(x, length.out=elems.len) else x[elems]
    x.carries <- if(x.scalar)
      rep(x.carry, length.out=elems.len) else x.carry[elems]

    # note, for expediency we're currently assuming that there is no overlap
    # between starts and stops
    e.order <- forder(c(e.start, e.stop))

    e.keep <- rep(c(!round.start, round.stop), each=elems.len)[e.order]
    e.sort <- c(e.start, e.stop)[e.order]

    state <- .Call(
      FANSI_state_at_pos_ext,
      u, e.sort, type.int,
      e.keep,  # whether to include a partially covered multi-byte character
      rep(c(TRUE, FALSE), each=length(elems))[e.order], # start or end of string
      warn, term.cap.int,
      ctl.int, normalize, terminate,
      c(e.ids, e.ids)[e.order]
    )
    # Recover the matching values for e.sort
    e.unsort.idx <- match(seq_along(e.order), e.order)  # e.order[e.order]?
    start.stop.ansi.idx <- .Call(FANSI_cleave, e.unsort.idx)
    start.ansi.idx <- start.stop.ansi.idx[[1L]]
    stop.ansi.idx <- start.stop.ansi.idx[[2L]]

    # And use those to substr with
    start.ansi <- state[[2]][3, start.ansi.idx] + 1L
    stop.ansi <- state[[2]][3, stop.ansi.idx]
    start.tag <- state[[1]][start.ansi.idx]
    stop.tag <- state[[1]][stop.ansi.idx]

    # It's possible to end up with starts after stops because starts always
    # ingest trailing SGR.
    empty.req <- e.start >= e.stop
    empty.res <- !empty.req & start.ansi > stop.ansi
    if(!terminate) res[elems[empty.res]] <- start.tag[empty.res]

    # Finalize real substrings
    full <- !empty.res & !empty.req
    if(any(full)) {
      # if there is active state at end then add a terminating CSI, warnings
      # should have been issued on first read
      end.csi <-
        if(terminate) close_state(stop.tag[full], warn=FALSE, normalize)
        else ""

      substring <- substr(x.elems[full], start.ansi[full], stop.ansi[full])
      term.cap <- VALID.TERM.CAP[term.cap.int]
      tmp <- paste0(
        if(!is.na(carry)) {
          bridge(
            x.carries[full], start.tag[full], term.cap=term.cap,
            normalize=normalize
          )
        } else start.tag[full],
        substring
      )
      res[elems[full]] <- paste0(
        if(normalize) normalize_state(tmp, warn=FALSE, term.cap=term.cap)
        else tmp,
        end.csi
  ) } }
  res
}

## Need to expose this so we can test bad UTF8 handling because substr will
## behave different with bad UTF8 pre and post R 3.6.0.  Make sure things
## are sorted properly given starts are input -1L.

state_at_pos <- function(
  x, starts, ends, warn=getOption('fansi.warn'),
  normalize=getOption('fansi.normalize', FALSE),
  terminate=getOption('fansi.terminate', FALSE)
) {
  is.start <- c(rep(TRUE, length(starts)), rep(FALSE, length(ends)))
  .Call(
    FANSI_state_at_pos_ext,
    x, as.integer(c(starts - 1L, ends)),
    0L,        # character type
    is.start,  # keep if is.start
    is.start,  # indicate that it's a start
    warn,
    1L,        # term.cap="all"
    1L,        # ctl="all"
    normalize,
    terminate,
    rep(seq_along(starts), 2)
  )
}
