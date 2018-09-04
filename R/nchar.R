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

#' ANSI Control Sequence Aware Version of nchar
#'
#' `nchar_ctl` counts all non _Control Sequence_ characters.
#' `nzchar_ctl` returns TRUE for each input vector element that has non _Control
#' Sequence_ sequence characters.  By default newlines and other C0 control
#' characters are not counted.
#'
#' `nchar_ctl` is just a wrapper around `nchar(strip_ctl(...))`.  `nzchar_ctl`
#' is implemented in native code and is much faster than the otherwise
#' equivalent `nzchar(strip_ctl(...))`.
#'
#' These functions will warn if either malformed or non-CSI escape sequences are
#' encountered, as these may be incorrectly interpreted.
#'
#' @inheritParams substr_ctl
#' @inheritParams base::nchar
#' @inheritSection substr_ctl ctl vs. sgr
#' @note the `keepNA` parameter is ignored for R < 3.2.2.
#' @export
#' @param type character string, one of "chars", or "width".  For byte counts
#'   use [base::nchar].
#' @seealso [fansi] for details on how _Control Sequences_ are
#'   interpreted, particularly if you are getting unexpected results,
#'   [strip_ctl] for removing _Control Sequences_.
#' @examples
#' nchar_ctl("\033[31m123\a\r")
#' ## with some wide characters
#' cn.string <-  sprintf("\033[31m%s\a\r", "\u4E00\u4E01\u4E03")
#' nchar_ctl(cn.string)
#' nchar_ctl(cn.string, type='width')
#'
#' ## Remember newlines are not counted by default
#' nchar_ctl("\t\n\r")
#'
#' ## The 'c0' value for the `ctl` argument does
#' ## not include newlines.
#' nchar_ctl("\t\n\r", ctl="c0")
#' nchar_ctl("\t\n\r", ctl=c("c0", "nl"))
#'
#' ## The _sgr flavor only treats SGR sequences as zero width
#'
#' nchar_sgr("\033[31m123")
#' nchar_sgr("\t\n\n123")
#'
#' ## All of the following are Control Sequences
#' nzchar_ctl("\n\033[42;31m\033[123P\a")

nchar_ctl <- function(
  x, type='chars', allowNA=FALSE, keepNA=NA, ctl='all',
  warn=getOption('fansi.warn'), strip
) {
  if(!is.character(x)) x <- as.character(x)
  if(!is.logical(warn)) warn <- as.logical(warn)
  if(length(warn) != 1L || is.na(warn))
    stop("Argument `warn` must be TRUE or FALSE.")

  if(!is.logical(allowNA)) allowNA <- as.logical(allowNA)
  if(length(allowNA) != 1L)
    stop("Argument `allowNA` must be a scalar logical.")

  if(!is.logical(keepNA)) keepNA <- as.logical(keepNA)
  if(length(keepNA) != 1L)
    stop("Argument `keepNA` must be a scalar logical.")

  if(!missing(strip)) {
    message("Parameter `strip` has been deprecated; use `ctl` instead.")
    ctl <- strip
  }
  if(!is.character(ctl))
    stop("Argument `ctl` must be character.")
  if(!all(ctl %in% VALID.CTL))
    stop(
      "Argument `ctl` may contain only values in `", deparse(VALID.CTL), "`"
    )
  if(!is.character(type) || length(type) != 1 || is.na(type))
    stop("Argument `type` must be scalar character and not NA.")
  valid.types <- c('chars', 'width', 'bytes')
  if(is.na(type.int <- pmatch(type, valid.types)))
    stop(
      "Argument `type` must partial match one of 'chars', 'width', or 'bytes'."
    )
  type <- valid.types[type.int]
  stripped <- strip_ctl(x, ctl=ctl, warn=warn)

  R.ver.gte.3.2.2 <- R.ver.gte.3.2.2 # "import" symbol from namespace
  if(R.ver.gte.3.2.2) nchar(stripped, type=type, allowNA=allowNA, keepNA=keepNA)
  else nchar(stripped, type=type, allowNA=allowNA) # nocov
}
#' @export
#' @rdname nchar_ctl

nzchar_ctl <- function(x, keepNA=NA, ctl='all', warn=getOption('fansi.warn')) {
  if(!is.character(x)) x <- as.character(x)

  if(length(warn) != 1L || is.na(warn))
    stop("Argument `warn` must be TRUE or FALSE.")

  if(!is.logical(keepNA)) keepNA <- as.logical(keepNA)
  if(length(keepNA) != 1L)
    stop("Argument `keepNA` must be a scalar logical.")

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
  term.cap.int <- seq_along(VALID.TERM.CAP)
  .Call(FANSI_nzchar_esc, enc2utf8(x), keepNA, warn, term.cap.int, ctl.int)
}
#' @export
#' @rdname nchar_ctl

nchar_sgr <- function(
  x, type='chars', allowNA=FALSE, keepNA=NA, warn=getOption('fansi.warn')
)
 nzchar_ctl(x=x, keepNA=keepNA, warn=warn, ctl='sgr')

