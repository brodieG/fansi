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

#' ANSI Control Sequence Aware Version of nchar
#'
#' `nchar_esc` counts all non control character / sequence characters.
#' `nzchar_esc` returns TRUE for each input vector element that has non control
#' chacater / sequence characters.
#'
#' These functions are faster than the semantically equivalent
#' `nchar(strip_esc(x, what='all')).  If you wish to control which control
#' characters and sequences are stripped you will need to use
#' `nchar(strip_esc(x, what=...))`.  In particular, note that newlines are
#' treated as control characters and not counted.
#'
#' These functions will warn if either malformed or non-CSI escape sequences are
#' encountered, as these may be incorrectly interpreted.
#'
#' Any non-ASCII non-UTF8 string will be converted to UTF8 prior to computing
#' character or width counts.
#'
#' @inheritParams base::nchar
#' @export
#' @param type character string, one of "chars", or "width".  For byte counts
#'   use [base::nchar].
#' @seealso [strip_esc]
#' @examples
#' nchar_esc("\033[31m123\a\r")
#' ## with some wide characters
#' cn.string <-  sprintf("\033[31m%s\a\r", "\u4E00\u4E01\u4E03")
#' nchar_esc(cn.string)
#' nchar_esc(cn.string, type='width')
#'
#' ## All of the following are control sequences
#' nzchar_esc("\n\033[42;31m\033[123P\a")
#' ## If we want to count newlines, this is a slower option

nchar_esc <- function(
  x, type='chars', allowNA=FALSE, keepNA=NA, warn=getOption('fansi.warn')
) {
  if(!is.character(x)) x <- as.character(x)
  vetr(
    warn=LGL.1, type=CHR.1, allowNA=LGL.1, keepNA=logical(1), strip=CHR
  )
  if(!all(strip %in% VALID.STRIP))
    stop(
      "Argument `strip` may contain only values in `", deparse(VALID.STRIP), "`"
    )
  nchar(
    strip_esc(x, strip=strip, warn=warn), type=type, allowNA=allowNA,
    keepNA=keepNA
  )
}
#' @export
#' @rdname nchar_esc

nzchar_esc <- function(x, keepNA=NA, warn=getOption('fansi.warn')) {
  if(!is.character(x)) x <- as.character(x)
  vetr(warn=LGL.1, keepNA=logical(1))
  term.cap.int <- seq_along(VALID.TERM.CAP)
  .Call(FANSI_nzchar_esc, x, keepNA, warn, term.cap.int)
}
