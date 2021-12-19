## Copyright (C) 2021  Brodie Gaslam
##
## This file is part of "fansi - ANSI Control Sequence Aware String Functions"
##
## This program is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 2 or 3 of the License.
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
#' slightly slower than `substr`, and more so for `type = 'width'`.  CSI SGR
#' sequences will be included in the substrings to reflect the format of the
#' substring when it was embedded in the source string.  `substr2_ctl` adds the
#' ability to retrieve substrings based on display width in addition to the
#' normal character width.  `substr2_ctl` also provides the option to convert
#' tabs to spaces with [`tabs_as_spaces`] prior to taking substrings.
#'
#' @section Position Semantics:
#'
#' When computing substrings, _Normal_ (non-control) characters are considered
#' to occupy positions in strings, whereas _Control Sequences_ occupy the
#' interstices between them.  The string
#' `"hello-\033&lsqb;31mworld\033&lsqb;m!"` is interpreted as:
#'
#' ```
#'                    1 1 1
#'  1 2 3 4 5 6 7 8 9 0 1 2
#'  h e l l o -|w o r l d|!
#'             ^         ^
#'             \033[31m  \033[m
#' ```
#'
#' `start` and `stop` reference character positions so they never explicitly
#' select for the interstitial _Control Sequences_.  The latter are implicitly
#' selected if they appear in interstices after the first character and before
#' the last.  Additionally, because _Special Sequences_ (CSI SGR and OSC
#' hyperlinks) affect all subsequent characters in a string, any active _Special
#' Sequence_, whether opened just before a character or much before, will be
#' reflected in the state `fansi` prepends to the beginning of each substring.
#'
#' It is possible to select _Control Sequences_ at the end of a string by
#' specifying `stop` values past the end of the string, although for _Special
#' Sequences_ this only produces visible results if `terminate` is set to
#' `FALSE`.  Similarly, it is possible to select _Control Sequences_ preceding
#' the beginning of a string by specifying `start` values less than one,
#' although as noted earlier this is unnecessary for _Special Sequences_ as
#' those are output by `fansi` before each substring.
#'
#' Because exact substrings on anything other than character count cannot be
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
#' `stop`, and "both" could cause all of them to be included.  See examples.
#'
#' A number of _Normal_ characters such as combining diacritic marks have
#' reported width of zero.  These are typically displayed overlaid on top of the
#' preceding glyph, as in the case of `"e\u301"` forming `"é"`.  Unlike _Control
#' Sequences_, which also have reported width of zero, `fansi` groups zero-width
#' _Normal_ characters with the last preceding non-zero width _Normal_
#' character.  This is incorrect for some rare zero-width _Normal_ characters
#' such as prepending marks (see "Output Stability" and "Graphemes").
#'
#' @section Output Stability:
#'
#' Several factors could affect the exact output produced by `fansi`
#' functions across versions of `fansi`, `R`, and/or across systems.
#' **In general it is likely best not to rely on the exact `fansi` output,
#' e.g. by embedding it in tests.**
#'
#' Width and grapheme calculations depend on locale, Unicode database
#' version, and grapheme processing logic (which is still in development), among
#' other things.  For the most part `fansi` (currently) uses the internals of
#' `base::nchar(type='width')`, but there are exceptions and this may change in
#' the future.
#'
#' How a particular display format is encoded in _Control Sequences_ is
#' not guaranteed to be stable across `fansi` versions.  Additionally, which
#' _Special Sequences_ are re-encoded vs transcribed untouched may change.
#' In general we will strive to keep the rendered appearance stable.
#'
#' To maximize the odds of getting stable output set `normalize_state` to
#' `TRUE` and `type` to `"chars"` in functions that allow it, and
#' set `term.cap` to a specific set of capabilities.
#'
#' @section Replacement Functions:
#'
#' Semantics for replacement functions have the additional requirement that the
#' result appear as if it is the input modified in place between the positions
#' designated by `start` and `stop`.  `terminate` only affects the boundaries
#' between the original substring and the spliced one, `normalize` only affects
#' the same boundaries and `value`, `tabs.as.spaces` only affects `value`, and
#' `x` must be ASCII only or marked "UTF-8".
#'
#' `terminate = FALSE` only makes sense in replacement mode if only one of `x`
#' or `value` contains _Control Sequences_.  `fansi` will not account for any
#' interactions of state in `x` and `value`.
#'
#' The `carry` parameter causes state to carry within the original string and
#' the replacement values independently, as if they were columns of text cut
#' from different pages and pasted together.  String values for `carry` are
#' disallowed in replacement mode as it is ambiguous which of `x` or `value`
#' they would modify (see examples).
#'
#' When in `type = 'width'` mode, it is only guaranteed that the result will be
#' no wider than the original `x`.  Narrower strings may result if a mixture
#' of narrow and wide graphemes cannot be replaced exactly with the same `width`
#' value, possibly because the provided `start` and `stop` values (or the
#' implicit ones generated for `value`) do not align with grapheme boundaries.
#'
#' @section Graphemes:
#'
#' `fansi` approximates grapheme widths and counts by using heuristics for
#' grapheme breaks that work for most common graphemes, including emoji
#' combining sequences.  The heuristic is known to work incorrectly with
#' invalid combining sequences, prepending marks, and sequence interruptors.
#' `fansi` does not provide a full implementation of grapheme break detection to
#' avoid carrying a copy of the Unicode grapheme breaks table, and also because
#' the hope is that R will add the feature eventually itself.
#'
#' The [`utf8`](https://cran.r-project.org/package=utf8) package provides a
#' conforming grapheme parsing implementation.
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
#' @param type character(1L) partial matching
#'   `c("chars", "width", "graphemes")`, although types other than "chars" only
#'   work correctly with R >= 3.2.2.  See [`?nchar`][base::nchar].
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
#'   turn the tabs to spaces and resolve the warning that way.  At most one
#'   warning will be issued per element in each input vector.  Will also warn
#'   about some badly encoded UTF-8 strings, but a lack of UTF-8 warnings is not
#'   a guarantee of correct encoding (use `[validUTF8]` for that).
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
#'   element of `x` (see "Replacement Functions" for differences there).
#'   Semantically, the carried state is injected in the interstice between an
#'   imaginary zeroeth character and the first character of a vector element.
#'   See the "Position Semantics" section of [`substr_ctl`] and the "State
#'   Interactions" section of [`?fansi`][fansi]
#'   for details.
#' @param terminate TRUE (default) or FALSE whether substrings should have
#'   active state closed to avoid it bleeding into other strings they may be
#'   prepended onto.  This does not stop state from carrying if `carry = TRUE`.
#'   See the "State Interactions" section of [`?fansi`][fansi] for details.
#' @param value a character vector or object that can be coerced to such.
#' @return A character vector of the same length and with the same attributes as
#'   x (after possible coercion and re-encoding to UTF-8).
#' @examples
#' substr_ctl("\033[42mhello\033[m world", 1, 9)
#' substr_ctl("\033[42mhello\033[m world", 3, 9)
#'
#' ## Positions 2 and 4 are in the middle of the full width Ｗ for
#' ## the `start` and `stop` positions respectively. Use `round`
#' ## to control result:
#' ##    12345
#' x <- "ＷnＷ"
#' substr2_ctl(x, 2, 4, type='width', round='start')
#' substr2_ctl(x, 2, 4, type='width', round='stop')
#' substr2_ctl(x, 2, 4, type='width', round='neither')
#' substr2_ctl(x, 2, 4, type='width', round='both')
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
#' ## Trailing sequences omitted unless `stop` past end.
#' substr_ctl("ABC\033[42m", 1, 3, terminate=FALSE)
#' substr_ctl("ABC\033[42m", 1, 4, terminate=FALSE)
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
#'
#' ## Work-around to specify carry strings in replacement mode
#' x <- c("ABC", "DEF")
#' val <- "#"
#' x2 <- c("\033[42m", x)
#' val2 <- c("\033[45m", rep_len(val, length(x)))
#' substr_ctl(x2, 2, 2, carry=TRUE) <- val2
#' (x <- x[-1])

substr_ctl <- function(
  x, start, stop,
  warn=getOption('fansi.warn', TRUE),
  term.cap=getOption('fansi.term.cap', dflt_term_cap()),
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
  tabs.as.spaces=getOption('fansi.tabs.as.spaces', FALSE),
  tab.stops=getOption('fansi.tab.stops', 8L),
  warn=getOption('fansi.warn', TRUE),
  term.cap=getOption('fansi.term.cap', dflt_term_cap()),
  ctl='all', normalize=getOption('fansi.normalize', FALSE),
  carry=getOption('fansi.carry', FALSE),
  terminate=getOption('fansi.terminate', TRUE)
) {
  ## So warning are issues here
  start <- as.integer(start)
  stop <- as.integer(stop)
  ## modifies / creates NEW VARS in fun env
  VAL_IN_ENV(
    x=x, warn=warn, term.cap=term.cap,
    ctl=ctl, normalize=normalize,
    carry=carry, terminate=terminate,
    tab.stops=tab.stops,
    tabs.as.spaces=tabs.as.spaces, type=type, round=round,
    start=start, stop=stop
  )
  res <- x
  no.na <- !(is.na(x) | is.na(start & stop))

  res[no.na] <- substr_ctl_internal(
    x[no.na], start=start[no.na], stop=stop[no.na],
    type.int=TYPE.INT,
    tabs.as.spaces=tabs.as.spaces, tab.stops=tab.stops,
    warn.int=WARN.INT, term.cap.int=TERM.CAP.INT,
    round.int=ROUND.INT,
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
  warn=getOption('fansi.warn', TRUE),
  term.cap=getOption('fansi.term.cap', dflt_term_cap()),
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
  tabs.as.spaces=getOption('fansi.tabs.as.spaces', FALSE),
  tab.stops=getOption('fansi.tab.stops', 8L),
  warn=getOption('fansi.warn', TRUE),
  term.cap=getOption('fansi.term.cap', dflt_term_cap()),
  ctl='all', normalize=getOption('fansi.normalize', FALSE),
  carry=getOption('fansi.carry', FALSE),
  terminate=getOption('fansi.terminate', TRUE)
) {
  # So warning are issued here
  start <- as.integer(start)
  stop <- as.integer(stop)
  # modifies / creates NEW VARS in fun env
  x0 <- x
  VAL_IN_ENV(
    x=x, warn=warn, term.cap=term.cap, ctl=ctl, normalize=normalize,
    tab.stops=tab.stops, tabs.as.spaces=tabs.as.spaces, round=round, start=start,
    stop=stop, type=type, carry=carry, value=value
  )
  # In replace mode we shouldn't change the encoding
  if(!all(enc.diff <- Encoding(x) == Encoding(x0)))
    stop(
      "`x` may only contain ASCII or marked UTF-8 encoded strings; ",
      "you can use `enc2utf8` to convert `x` prior to use with ",
      "`substr_ctl<-` (replacement form).  Illegal value at position [",
      min(which(!enc.diff)), "]."
    )

  value <- as.character(value)
  if(tabs.as.spaces)
    value <- .Call(
      FANSI_tabs_as_spaces, value, tab.stops,
      0L,  # turn off warning, will be reported later
      TERM.CAP.INT, CTL.INT
    )
  value <- rep_len(enc_to_utf8(value), X.LEN)

  res <- .Call(FANSI_substr,
    x,
    start, stop, value,
    TYPE.INT, ROUND.INT,
    WARN.INT, TERM.CAP.INT,
    CTL.INT, normalize,
    carry, terminate
  )
  attributes(res) <- attributes(x)
  res
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
  warn=getOption('fansi.warn', TRUE),
  term.cap=getOption('fansi.term.cap', dflt_term_cap()),
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
  tabs.as.spaces=getOption('fansi.tabs.as.spaces', FALSE),
  tab.stops=getOption('fansi.tab.stops', 8L),
  warn=getOption('fansi.warn', TRUE),
  term.cap=getOption('fansi.term.cap', dflt_term_cap()),
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

substr_ctl_internal <- function(
  x, start, stop, type.int, round.int, tabs.as.spaces,
  tab.stops, warn.int, term.cap.int,
  x.len, ctl.int, normalize, carry, terminate
) {
  if(tabs.as.spaces)
    x <- .Call(
      FANSI_tabs_as_spaces, x, tab.stops,
      0L,  # turn off warning, will be reported later
      term.cap.int, ctl.int
    )

  .Call(FANSI_substr,
    x,
    start, stop, NULL,
    type.int, round.int,
    warn.int, term.cap.int,
    ctl.int, normalize,
    carry, terminate
  )
}

