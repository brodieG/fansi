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

#' ANSI Control Sequence Aware Version of strsplit
#'
#' Will run [base::strsplit] with the provided inputs, and then will process
#' them to ensure the effect of CSI SGR sequences is preserved within each
#' element of the input character vector.
#'
#' Currently this function will not produce the expected outcome if `split`
#' matches CSI SGR sequences.  For example, if you use `split='m'` this will
#' destroy the CSI SGR sequences as they end in the letter "m".  CSI SGR
#' sequences contain ESC, "[", "m", and numbers.  In future releases we will
#' ensure splits do not affect the CSI SGR sequences.
#'
#' @seealso [fansi] for details on how _Control Sequences_ are
#'   interpreted, particularly if you are getting unexpected results,
#'   [base::strsplit] for details on the splitting.
#' @export
#' @inheritParams base::strsplit
#' @inheritParams strwrap_ctl
#' @return list, see [base::strsplit].

strsplit_ctl <- function(
  x, split, fixed=FALSE, perl=FALSE, useBytes=FALSE,
  warn=getOption('fansi.warn'), term.cap=getOption('fansi.term.cap')
) {
  x.split <- strsplit(x, split, fixed=FALSE, perl=FALSE, useBytes=FALSE)
  vetr(warn=LGL.1, term.cap=CHR)
  if(anyNA(term.cap.int <- match(term.cap, VALID.TERM.CAP)))
    stop(
      "Argument `term.cap` may only contain values in ",
      deparse(VALID.TERM.CAP)
    )
  .Call(FANSI_strsplit, x.split, warn, term.cap.int)
}


