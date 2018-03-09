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
#' @seealso [fansi] for details on how _Control Sequences_ are
#'   interpreted, particularly if you are getting unexpected results.
#' @inheritParams base::strwrap
#' @inheritParams tabs_as_spaces
#' @inheritParams substr_ctl
#' @param wrap.always TRUE or FALSE (default), whether to hard wrap at requested
#'   width if no word breaks are detected within a line.  If set to TRUE then
#'   `width` must be at least 2.
#' @param pad.end character(1L), a single character to use as padding at the
#'   end of each line until the line is `width` wide.  This must be a printable
#'   ASCII character or an empty string (default).  If you set it to an empty
#'   string the line remains unpadded.
#' @param strip.spaces TRUE (default) or FALSE, if TRUE, extraneous white spaces
#'   (spaces, newlines, tabs) are removed in the same way as [base::strwrap]
#'   does.
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
#' ## (works best on displays that support ANSI CSI
#' ## SGR sequences)
#'
#' NEWS <- readLines(file.path(R.home('doc'), 'NEWS'))
#' length(NEWS) <- 450 # make sure at least 450 long
#' bg <- ceiling((seq_along(NEWS)) %% 215 + 1) + 16
#' fg <- ifelse((((bg - 16) %/% 18) %% 2), 30, 37)
#' tpl <- "\033[%d;48;5;%dm%s\033[49m"
#'
#' nz <- nzchar(NEWS)
#' NEWS[nz] <- sprintf(tpl, fg[nz], bg[nz], NEWS[nz])
#' NEWS[!nz] <- '\n\n'
#' NEWS.C <- paste0(NEWS, collapse="")
#'
#' W <- strwrap2_ctl(NEWS.C, 25, pad.end=" ", wrap.always=TRUE)
#' writeLines(c("", paste(W[1:40], W[200:240], W[410:450]), ""))

strwrap_ctl <- function(
  x, width = 0.9 * getOption("width"), indent = 0,
  exdent = 0, prefix = "", simplify = TRUE, initial = prefix,
  warn=getOption('fansi.warn'), term.cap=getOption('fansi.term.cap')
) {
  if(!is.character(x)) x <- as.character(x)
  vetr(
    x=character(),
    width=NUM,
    indent=INT.1.POS,
    exdent=INT.1.POS, prefix=character(1), simplify=LGL.1, initial=character(1),
    warn=LGL.1, term.cap=CHR
  )
  if(anyNA(term.cap.int <- match(term.cap, VALID.TERM.CAP)))
    stop(
      "Argument `term.cap` may only contain values in ",
      deparse(VALID.TERM.CAP)
    )

  width <- max(c(as.integer(width) - 1L, 1L))
  indent <- as.integer(indent)
  exdent <- as.integer(exdent)

  res <- .Call(
    FANSI_strwrap_csi,
    x, width, indent, exdent,
    prefix, initial,
    FALSE, "",
    TRUE,
    FALSE, 8L,
    warn, term.cap.int,
    FALSE   # first_only
  )
  if(simplify) unlist(res) else res
}
#' @export
#' @rdname strwrap_ctl

strwrap2_ctl <- function(
  x, width = 0.9 * getOption("width"), indent = 0,
  exdent = 0, prefix = "", simplify = TRUE, initial = prefix,
  wrap.always=FALSE, pad.end="",
  strip.spaces=!tabs.as.spaces, tabs.as.spaces=FALSE, tab.stops=8L,
  warn=getOption('fansi.warn'), term.cap=getOption('fansi.term.cap')
) {
  if(!is.character(x)) x <- as.character(x)
  vetr(
    x=character(),
    width=NUM.1 && I(. > 1 || !wrap.always),
    indent=INT.1.POS,
    exdent=INT.1.POS, prefix=character(1), simplify=LGL.1, initial=character(1),
    pad.end=CHR.1 && nchar(.) < 2, wrap.always=LGL.1, strip.spaces=LGL.1,
    tabs.as.spaces=LGL.1,
    tab.stops=INT.POS.STR && length(.) > 0,
    warn=LGL.1, term.cap=CHR
  )
  if(tabs.as.spaces && strip.spaces)
    stop("`tabs.as.spaces` and `strip.spaces` should not both be TRUE.")
  if(anyNA(term.cap.int <- match(term.cap, VALID.TERM.CAP)))
    stop(
      "Argument `term.cap` may only contain values in ",
      deparse(VALID.TERM.CAP)
    )

  width <- max(c(as.integer(width) - 1L, 1L))
  indent <- as.integer(indent)
  exdent <- as.integer(exdent)
  tab.stops <- as.integer(tab.stops)

  res <- .Call(
    FANSI_strwrap_csi,
    x, width,
    indent, exdent,
    prefix, initial,
    wrap.always, pad.end,
    strip.spaces,
    tabs.as.spaces, tab.stops,
    warn, term.cap.int,
    FALSE   # first_only
  )
  if(simplify) unlist(res) else res
}

