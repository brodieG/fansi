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

#' Control Sequence Aware Version of strwrap
#'
#' Wraps strings to a specified width accounting for zero display width _Control
#' Sequences_.  `strwrap_ctl` is intended to emulate `strwrap` closely except
#' with respect to the _Control Sequences_ (see details for other minor
#' differences), while `strwrap2_ctl` adds features and changes the processing
#' of whitespace.
#'
#' `strwrap2_ctl` can convert tabs to spaces, pad strings up to `width`, and
#' hard-break words if single words are wider than `width`.
#'
#' Unlike [base::strwrap], both these functions will translate any non-ASCII
#' strings to UTF-8 and return them in UTF-8.  Additionally, invalid UTF-8
#' always causes errors, and `prefix` and `indent` must be scalar.
#'
#' When replacing tabs with spaces the tabs are computed relative to the
#' beginning of the input line, not the most recent wrap point.
#' Additionally,`indent`, `exdent`, `initial`, and `prefix` will be ignored when
#' computing tab positions.
#'
#' @inheritSection substr_ctl Graphemes
#' @inheritSection substr_ctl Output Stability
#' @inheritParams base::strwrap
#' @inheritParams tabs_as_spaces
#' @inheritParams substr_ctl
#' @inherit substr_ctl seealso
#' @note Non-ASCII strings are converted to and returned in UTF-8 encoding.
#'   Width calculations will not work properly in R < 3.2.2.
#' @param wrap.always TRUE or FALSE (default), whether to hard wrap at requested
#'   width if no word breaks are detected within a line.  If set to TRUE then
#'   `width` must be at least 2.
#' @param pad.end character(1L), a single character to use as padding at the
#'   end of each line until the line is `width` wide.  This must be a printable
#'   ASCII character or an empty string (default).  If you set it to an empty
#'   string the line remains unpadded.
#' @param strip.spaces TRUE (default) or FALSE, if TRUE, extraneous white spaces
#'   (spaces, newlines, tabs) are removed in the same way as [base::strwrap]
#'   does.  When FALSE, whitespaces are preserved, except for newlines as those
#'   are implicit boundaries between output vector elements.
#' @param tabs.as.spaces FALSE (default) or TRUE, whether to convert tabs to
#'   spaces.  This can only be set to TRUE if `strip.spaces` is FALSE.
#' @note For the `strwrap*` functions the `carry` parameter affects whether
#'   styles are carried across _input_ vector elements.  Styles always carry
#'   within a single wrapped vector element (e.g. if one of the input elements
#'   gets wrapped into three lines, the styles will carry through those three
#'   lines even if `carry=FALSE`, but not across input vector elements).
#' @export
#' @examples
#' hello.1 <- "hello \033[41mred\033[49m world"
#' hello.2 <- "hello\t\033[41mred\033[49m\tworld"
#'
#' strwrap_ctl(hello.1, 12)
#' strwrap_ctl(hello.2, 12)
#'
#' ## In default mode strwrap2_ctl is the same as strwrap_ctl
#' strwrap2_ctl(hello.2, 12)
#'
#' ## But you can leave whitespace unchanged, `warn`
#' ## set to false as otherwise tabs causes warning
#' strwrap2_ctl(hello.2, 12, strip.spaces=FALSE, warn=FALSE)
#'
#' ## And convert tabs to spaces
#' strwrap2_ctl(hello.2, 12, tabs.as.spaces=TRUE)
#'
#' ## If your display has 8 wide tab stops the following two
#' ## outputs should look the same
#' writeLines(strwrap2_ctl(hello.2, 80, tabs.as.spaces=TRUE))
#' writeLines(hello.2)
#'
#' ## tab stops are NOT auto-detected, but you may provide
#' ## your own
#' strwrap2_ctl(hello.2, 12, tabs.as.spaces=TRUE, tab.stops=c(6, 12))
#'
#' ## You can also force padding at the end to equal width
#' writeLines(strwrap2_ctl("hello how are you today", 10, pad.end="."))
#'
#' ## And a more involved example where we read the
#' ## NEWS file, color it line by line, wrap it to
#' ## 25 width and display some of it in 3 columns
#' ## (works best on displays that support 256 color
#' ## SGR sequences)
#'
#' NEWS <- readLines(file.path(R.home('doc'), 'NEWS'))
#' NEWS.C <- fansi_lines(NEWS, step=2)  # color each line
#' W <- strwrap2_ctl(NEWS.C, 25, pad.end=" ", wrap.always=TRUE)
#' writeLines(c("", paste(W[1:20], W[100:120], W[200:220]), ""))

strwrap_ctl <- function(
  x, width = 0.9 * getOption("width"), indent = 0,
  exdent = 0, prefix = "", simplify = TRUE, initial = prefix,
  warn=getOption('fansi.warn', TRUE),
  term.cap=getOption('fansi.term.cap', dflt_term_cap()),
  ctl='all', normalize=getOption('fansi.normalize', FALSE),
  carry=getOption('fansi.carry', FALSE),
  terminate=getOption('fansi.terminate', TRUE)
) {
  ## modifies / creates NEW VARS in fun env
  VAL_IN_ENV(
    x=x, warn=warn, term.cap=term.cap, ctl=ctl, normalize=normalize,
    carry=carry, terminate=terminate
  )
  VAL_WRAP_IN_ENV(width, indent, exdent, prefix, initial, pad.end="")
  res <- .Call(
    FANSI_strwrap_csi,
    x, width, indent, exdent,
    prefix, initial,
    FALSE, "",
    TRUE,
    FALSE, 8L,
    WARN.INT, TERM.CAP.INT,
    FALSE,   # first_only
    CTL.INT, normalize,
    carry, terminate
  )
  if(simplify) {
    if(normalize) normalize_state(unlist(res), warn=FALSE, term.cap)
    else unlist(res)
  } else {
    if(normalize)
      normalize_state_list(
        res, warn.int=0L, term.cap.int=TERM.CAP.INT, carry=carry
      )
    else res
  }
}
#' @export
#' @rdname strwrap_ctl

strwrap2_ctl <- function(
  x, width = 0.9 * getOption("width"), indent = 0,
  exdent = 0, prefix = "", simplify = TRUE, initial = prefix,
  wrap.always=FALSE, pad.end="",
  strip.spaces=!tabs.as.spaces,
  tabs.as.spaces=getOption('fansi.tabs.as.spaces', FALSE),
  tab.stops=getOption('fansi.tab.stops', 8L),
  warn=getOption('fansi.warn', TRUE),
  term.cap=getOption('fansi.term.cap', dflt_term_cap()),
  ctl='all', normalize=getOption('fansi.normalize', FALSE),
  carry=getOption('fansi.carry', FALSE),
  terminate=getOption('fansi.terminate', TRUE)
) {
  if(!is.logical(wrap.always)) wrap.always <- as.logical(wrap.always)
  if(length(wrap.always) != 1L || is.na(wrap.always))
    stop("Argument `wrap.always` must be TRUE or FALSE.")
  if(!is.logical(tabs.as.spaces)) tabs.as.spaces <- as.logical(tabs.as.spaces)
  if(wrap.always && width < 2L)
    stop("Width must be at least 2 in `wrap.always` mode.")
  ## modifies / creates NEW VARS in fun env
  VAL_IN_ENV (
    x=x, warn=warn, term.cap=term.cap, ctl=ctl, normalize=normalize,
    carry=carry, terminate=terminate, tab.stops=tab.stops,
    tabs.as.spaces=tabs.as.spaces, strip.spaces=strip.spaces
  )
  if(tabs.as.spaces && strip.spaces)
    stop("`tabs.as.spaces` and `strip.spaces` should not both be TRUE.")
  # This changes `width`, so needs to happen after the first width validation
  VAL_WRAP_IN_ENV(width, indent, exdent, prefix, initial, pad.end)

  res <- .Call(
    FANSI_strwrap_csi,
    x, width,
    indent, exdent,
    prefix, initial,
    wrap.always, pad.end,
    strip.spaces,
    tabs.as.spaces, tab.stops,
    WARN.INT, TERM.CAP.INT,
    FALSE,   # first_only
    CTL.INT, normalize,
    carry, terminate
  )
  if(simplify) {
    if(normalize) normalize_state(unlist(res), warn=FALSE, term.cap)
    else unlist(res)
  } else {
    if(normalize) normalize_state_list(res, 0L, TERM.CAP.INT) else res
  }
}
#' Control Sequence Aware Version of strwrap
#'
#' These functions are deprecated in favor of the [`_ctl` flavors][strwrap_ctl].
#'
#' @inheritParams strwrap_ctl
#' @inherit strwrap_ctl return
#' @keywords internal
#' @export

strwrap_sgr <- function(
  x, width = 0.9 * getOption("width"), indent = 0,
  exdent = 0, prefix = "", simplify = TRUE, initial = prefix,
  warn=getOption('fansi.warn', TRUE),
  term.cap=getOption('fansi.term.cap', dflt_term_cap()),
  normalize=getOption('fansi.normalize', FALSE),
  carry=getOption('fansi.carry', FALSE),
  terminate=getOption('fansi.terminate', TRUE)
)
  strwrap_ctl(
    x=x, width=width, indent=indent,
    exdent=exdent, prefix=prefix, simplify=simplify, initial=initial,
    warn=warn, term.cap=term.cap, ctl='sgr', normalize=normalize,
    carry=carry, terminate=terminate
  )
#' @export
#' @rdname strwrap_sgr

strwrap2_sgr <- function(
  x, width = 0.9 * getOption("width"), indent = 0,
  exdent = 0, prefix = "", simplify = TRUE, initial = prefix,
  wrap.always=FALSE, pad.end="",
  strip.spaces=!tabs.as.spaces,
  tabs.as.spaces=getOption('fansi.tabs.as.spaces', FALSE),
  tab.stops=getOption('fansi.tab.stops', 8L),
  warn=getOption('fansi.warn', TRUE),
  term.cap=getOption('fansi.term.cap', dflt_term_cap()),
  normalize=getOption('fansi.normalize', FALSE),
  carry=getOption('fansi.carry', FALSE),
  terminate=getOption('fansi.terminate', TRUE)
)
  strwrap2_ctl(
    x=x, width=width, indent=indent,
    exdent=exdent, prefix=prefix, simplify=simplify, initial=initial,
    wrap.always=wrap.always, pad.end=pad.end,
    strip.spaces=strip.spaces,
    tabs.as.spaces=tabs.as.spaces,
    tab.stops=tab.stops,
    warn=warn, term.cap=term.cap, ctl='sgr', normalize=normalize,
    carry=carry, terminate=terminate
  )

VAL_WRAP_IN_ENV <- function(
  width, indent, exdent, prefix, initial, pad.end
) {
  call <- sys.call(-1)
  env <- parent.frame()
  stop2 <- function(x) stop(simpleError(x, call))
  is_scl_int_pos <- function(x, name, strict=FALSE) {
    x <- as.integer(x)
    if(
      !is.numeric(x) || length(x) != 1L || is.na(x) ||
      if(strict) x <= 0 else x < 0
    )
      stop2(
        sprintf(
          "Argument `%s` %s.", name,
          "must be a positive scalar numeric representable as integer"
      ) )
    x
  }
  exdent <- is_scl_int_pos(exdent, 'exdent', strict=FALSE)
  indent <- is_scl_int_pos(indent, 'indent', strict=FALSE)
  if(is.numeric(width))
    width <- as.integer(min(c(max(c(min(width), 2L)), .Machine$integer.max)))
  else stop2("Argument `width` must be numeric.")
  # technically
  width <- is_scl_int_pos(width, 'width', strict=TRUE)
  width <- width - 1L

  if(!is.character(prefix)) prefix <- as.character(prefix)
  if(length(prefix) != 1L)
    stop2("Argument `prefix` must be a scalar character.")
  prefix <- enc_to_utf8(prefix)
  if(Encoding(prefix) == "bytes")
    stop2("Argument `prefix` cannot be \"bytes\" encoded.")

  if(!is.character(initial)) initial <- as.character(initial)
  if(length(initial) != 1L)
    stop2("Argument `initial` must be a scalar character.")
  initial <- enc_to_utf8(initial)
  if(Encoding(initial) == "bytes")
    stop2("Argument `initial` cannot be \"bytes\" encoded.")

  if(!is.character(pad.end)) pad.end <- as.character(pad.end)
  if(length(pad.end) != 1L)
    stop2("Argument `pad.end` must be a scalar character.")
  pad.end <- enc_to_utf8(pad.end)
  if(Encoding(pad.end) == "bytes")
    stop2("Argument `pad.end` cannot be \"bytes\" encoded.")
  if(nchar(pad.end, type='bytes') > 1L)
    stop2("Argument `pad.end` must be at most one byte long.")

  list2env(
    list(
      width=width, indent=indent, exdent=exdent, prefix=prefix, initial=initial,
      pad.end=pad.end
    ),
    env
  )
}
