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

#' ANSI Control Sequence Aware Version of strwrap
#'
#' Wraps strings to a specified width accounting for zero display width _Control
#' Sequences_.  `strwrap_ctl` is intended to emulate `strwrap` exactly except
#' with respect to the _Control Sequences_, while `strwrap2_ctl` adds features
#' and changes the processing of whitespace.
#'
#' `strwrap2_ctl` can convert tabs to spaces, pad strings up to `width`, and
#' hard-break words if single words are wider than `width`.
#'
#' Unlike [base::strwrap], both these functions will translate any non-ASCII
#' strings to UTF-8 and return them in UTF-8.  Additionally, malformed UTF-8
#' sequences are not converted to a text representation of bytes.
#'
#' When replacing tabs with spaces the tabs are computed relative to the
#' beginning of the input line, not the most recent wrap point.
#' Additionally,`indent`, `exdent`, `initial`, and `prefix` will be ignored when
#' computing tab positions.
#'
#' @note Non-ASCII strings are converted to and returned in UTF-8 encoding.
#'   Width calculations will not work correctly with R < 3.2.2.
#' @seealso [`fansi`] for details on how _Control Sequences_ are
#'   interpreted, particularly if you are getting unexpected results,
#'   [`normalize_sgr`] for more details on what the `normalize` parameter does.
#' @inheritParams base::strwrap
#' @inheritParams tabs_as_spaces
#' @inheritParams substr_ctl
#' @inheritSection substr_ctl _ctl vs. _sgr
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
#'   are implicit in boundaries between vector elements.
#' @param tabs.as.spaces FALSE (default) or TRUE, whether to convert tabs to
#'   spaces.  This can only be set to TRUE if `strip.spaces` is FALSE.
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
  warn=getOption('fansi.warn'), term.cap=getOption('fansi.term.cap'),
  ctl='all', normalize=getOption('fansi.normalize', FALSE),
  carry=getOption('fansi.carry', FALSE),
  terminate=getOption('fansi.terminate', TRUE)
) {
  args <- validate(
    x=x, warn=warn, term.cap=term.cap, ctl=ctl, normalize=normalize,
    carry=carry, terminate=terminate
  )
  args.basic <- validate_wrap_basic(width, indent, exdent, prefix, initial)
  with(
    c(args.basic, args), {
      res <- .Call(
        FANSI_strwrap_csi,
        x, width, indent, exdent,
        enc2utf8(prefix), enc2utf8(initial),
        FALSE, "",
        TRUE,
        FALSE, 8L,
        warn, term.cap.int,
        FALSE,   # first_only
        ctl.int, normalize
      )
      if(simplify) {
        if(normalize) normalize_sgr(unlist(res), warn, term.cap)
        else unlist(res)
      } else {
        if(normalize) normalize_sgr_list(res, warn, term.cap.int) else res
  } } )
}
#' @export
#' @rdname strwrap_ctl

strwrap2_ctl <- function(
  x, width = 0.9 * getOption("width"), indent = 0,
  exdent = 0, prefix = "", simplify = TRUE, initial = prefix,
  wrap.always=FALSE, pad.end="",
  strip.spaces=!tabs.as.spaces,
  tabs.as.spaces=getOption('fansi.tabs.as.spaces'),
  tab.stops=getOption('fansi.tab.stops'),
  warn=getOption('fansi.warn'), term.cap=getOption('fansi.term.cap'),
  ctl='all', normalize=getOption('fansi.normalize', FALSE),
  carry=getOption('fansi.carry', FALSE),
  terminate=getOption('fansi.terminate', TRUE)
) {
  args.basic <-
    validate_wrap_basic(width, indent, exdent, prefix, initial, pad.end)
  args <- validate(
    x=x, warn=warn, term.cap=term.cap, ctl=ctl, normalize=normalize,
    carry=carry, terminate=terminate, tab.stops=tab.stops,
    tabs.as.spaces=tabs.as.spaces, strip.spaces=strip.spaces
  )
  if(!is.character(pad.end) || length(pad.end) != 1 || nchar(pad.end) > 1)
    stop("Argument `pad.end` must be a one character or empty string.")
  if(!is.logical(wrap.always)) wrap.always <- as.logical(wrap.always)
  if(length(wrap.always) != 1L || is.na(wrap.always))
    stop("Argument `wrap.always` must be TRUE or FALSE.")
  if(!is.logical(tabs.as.spaces)) tabs.as.spaces <- as.logical(tabs.as.spaces)
  if(wrap.always && width < 2L)
    stop("Width must be at least 2 in `wrap.always` mode.")
  if(!is.character(prefix)) prefix <- as.character(prefix)
  if(length(prefix) != 1L)
    stop("Argument `prefix` must be a scalar character.")
  if(!is.character(initial)) initial <- as.character(initial)
  if(length(initial) != 1L)
    stop("Argument `initial` must be a scalar character.")
  prefix <- enc2utf8(prefix)
  if(Encoding(prefix) == "bytes")
    stop("Argument `prefix` cannot be \"bytes\" encoded.")
  initial <- enc2utf8(initial)
  if(Encoding(initial) == "bytes")
    stop("Argument `initial` cannot be \"bytes\" encoded.")

  tab.stops <- as.integer(tab.stops)

  with(
    c(args.basic, args), {
      res <- .Call(
        FANSI_strwrap_csi,
        x, width,
        indent, exdent,
        enc2utf8(prefix), enc2utf8(initial),
        wrap.always, pad.end,
        strip.spaces,
        tabs.as.spaces, tab.stops,
        warn, term.cap.int,
        FALSE,   # first_only
        ctl.int, normalize
      )
      if(simplify) {
        if(normalize) normalize_sgr(unlist(res), warn, term.cap)
        else unlist(res)
      } else {
        if(normalize) normalize_sgr_list(res, warn, term.cap.int) else res
  } } )
}
#' @export
#' @rdname strwrap_ctl

strwrap_sgr <- function(
  x, width = 0.9 * getOption("width"), indent = 0,
  exdent = 0, prefix = "", simplify = TRUE, initial = prefix,
  warn=getOption('fansi.warn'), term.cap=getOption('fansi.term.cap'),
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
#' @rdname strwrap_ctl

strwrap2_sgr <- function(
  x, width = 0.9 * getOption("width"), indent = 0,
  exdent = 0, prefix = "", simplify = TRUE, initial = prefix,
  wrap.always=FALSE, pad.end="",
  strip.spaces=!tabs.as.spaces,
  tabs.as.spaces=getOption('fansi.tabs.as.spaces'),
  tab.stops=getOption('fansi.tab.stops'),
  warn=getOption('fansi.warn'), term.cap=getOption('fansi.term.cap'),
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

validate_wrap_basic <- function(
  width, indent, exdent, prefix, initial, pad.end
) {
  call <- sys.cal(-1)
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
          "must be a positive scalar numeric representable as integer."
      ) )
    x
  }
  width <- is_scl_int_pos(x, 'width', strict=TRUE)
  exdent <- is_scl_int_pos(x, 'exdent', strict=FALSE)
  indent <- is_scl_int_pos(x, 'indent', strict=FALSE)
  width <- max(c(as.integer(width) - 1L, 1L))

  list(
    width=width, indent=indent, exdent=extent, prefix=prefix, initial=initial
  )
}


