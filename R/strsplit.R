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

# strsplit_ctl <- function(
#   x, split, fixed=FALSE, perl=FALSE, useBytes=FALSE,
#   warn=getOption('fansi.warn'), term.cap=getOption('fansi.term.cap')
# ) {
#   x.split <- strsplit(x, split, fixed=FALSE, perl=FALSE, useBytes=FALSE)
#   vetr(warn=LGL.1, term.cap=CHR)
#   if(anyNA(term.cap.int <- match(term.cap, VALID.TERM.CAP)))
#     stop(
#       "Argument `term.cap` may only contain values in ",
#       deparse(VALID.TERM.CAP)
#     )
#   .Call(FANSI_strsplit, x.split, warn, term.cap.int)
# }

strsplit_ctl <- function(
  x, split, fixed=FALSE, perl=FALSE, useBytes=FALSE,
  warn=getOption('fansi.warn')
) {
  x <- enc2utf8(as.character(x))
  split <- as.character(enc2utf8(split))
  if(!length(split)) split <- ""

  # Need to handle recycling, complicated by the ability of strsplit to accept
  # multiple different split arguments

  x.seq <- seq_along(x)
  s.seq <- seq_along(split)
  s.x.seq <- rep(s.seq, length.out=length(x))

  matches <- res <- vector("list", length(x))
  x.strip <- strip_ctl(x, warn=warn)
  chars <- nchar(x.strip)

  # Find the split locations and widths

  for(i in s.seq) {
    to.split <- s.x.seq == i
    matches[to.split] <- if(!nzchar(split[i])) {
      # special handling for zero width split
      lapply(
        chars[to.split],
        function(y)
          structure(
            seq.int(from=2L, by=1L, length.out=y - 1L),
            match.length=integer(y - 1L)
          )
      )
    } else {
      gregexpr(
        split[i], x.strip[to.split], perl=perl, useBytes=useBytes, fixed=fixed
      )
  } }
  # Use `substr` to select the pieces between the start/end

  for(i in seq_along(x)) {
    if(any(matches[[i]] > 0)) {
      starts <- c(1L, matches[[i]] + attr(matches[[i]], 'match.length'))
      ends <- c(matches[[i]] - 1L, chars[i])
      starts[starts < 1L] <- 1L
      sub.invalid <- starts > chars[i]

      if(any(sub.invalid)) {
        # happens when split goes all way to end of string
        starts <- starts[!sub.invalid]
        ends <- ends[!sub.invalid]
      }
      res[[i]] <- substr_ctl_internal(
        x=x[[i]],
        start=starts, stop=ends, type=0L,
        round.start=TRUE, round.stop=FALSE,
        tabs.as.spaces=FALSE, tab.stops=8L, warn=warn,
        term.cap.int=integer(), x.len=length(starts)
      )
    } else {
      res[[i]] <- x[[i]]
    }
  }
  # lazy fix for zero length strings splitting into nothing; would be better to
  # fix upstream...

  res[!chars] <- list(character(0L))
  res
}

