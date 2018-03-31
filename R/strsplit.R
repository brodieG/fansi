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
#' A drop-in replacement for [base::strsplit].  It will be noticeably slower,
#' but should otherwise behave the same way except for CSI SGR sequence
#' awareness.
#'
#' @note Non-ASCII strings are converted to and returned in UTF-8 encoding.  The
#'   split positions are computed after both `x` and `split` are converted to
#'   UTF-8.
#' @seealso [fansi] for details on how _Control Sequences_ are
#'   interpreted, particularly if you are getting unexpected results,
#'   [base::strsplit] for details on the splitting.
#' @export
#' @param x a character vector, or, unlike [base::strsplit] an object that can
#'   be coerced to character.
#' @inheritParams base::strsplit
#' @inheritParams strwrap_ctl
#' @return list, see [base::strsplit].
#' @examples
#' strsplit_ctl("\033[31mhello\033[42m world!", " ")

# strsplit_ctl <- function(
#   x, split, fixed=FALSE, perl=FALSE, useBytes=FALSE,
#   warn=getOption('fansi.warn'), term.cap=getOption('fansi.term.cap')
# ) {
#   x.split <- strsplit(x, split, fixed=FALSE, perl=FALSE, useBytes=FALSE)
#   if(anyNA(term.cap.int <- match(term.cap, VALID.TERM.CAP)))
#     stop(
#       "Argument `term.cap` may only contain values in ",
#       deparse(VALID.TERM.CAP)
#     )
#   .Call(FANSI_strsplit, x.split, warn, term.cap.int)
# }

strsplit_ctl <- function(
  x, split, fixed=FALSE, perl=FALSE, useBytes=FALSE,
  warn=getOption('fansi.warn'), term.cap=getOption('fansi.term.cap')
) {
  x <- as.character(x)
  if(any(Encoding(x) == "bytes"))
    stop("BYTE encoded strings are not supported.")

  if(is.null(split)) split <- ""
  split <- as.character(enc2utf8(split))
  if(!length(split)) split <- ""
  if(anyNA(split)) stop("Argument `split` may not contain NAs.")

  if(!is.logical(warn)) warn <- as.logical(warn)
  if(length(warn) != 1L || is.na(warn))
    stop("Argument `warn` must be TRUE or FALSE.")

  if(!is.logical(fixed)) fixed <- as.logical(fixed)
  if(length(fixed) != 1L || is.na(fixed))
    stop("Argument `fixed` must be TRUE or FALSE.")

  if(!is.logical(perl)) perl <- as.logical(perl)
  if(length(perl) != 1L || is.na(perl))
    stop("Argument `perl` must be TRUE or FALSE.")

  if(!is.logical(useBytes)) useBytes <- as.logical(useBytes)
  if(length(useBytes) != 1L || is.na(useBytes))
    stop("Argument `useBytes` must be TRUE or FALSE.")

  if(!is.character(term.cap))
    stop("Argument `term.cap` must be character.")
  if(anyNA(term.cap.int <- match(term.cap, VALID.TERM.CAP)))
    stop(
      "Argument `term.cap` may only contain values in ",
      deparse(VALID.TERM.CAP)
    )

  # Need to handle recycling, complicated by the ability of strsplit to accept
  # multiple different split arguments

  x.na <- is.na(x)
  x.seq <- seq_along(x)
  s.seq <- seq_along(split)
  s.x.seq <- rep(s.seq, length.out=length(x)) * (!x.na)

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
        start=starts, stop=ends, type.int=0L,
        round.start=TRUE, round.stop=FALSE,
        tabs.as.spaces=FALSE, tab.stops=8L, warn=warn,
        term.cap.int=term.cap.int, x.len=length(starts)
      )
    } else {
      res[[i]] <- x[[i]]
    }
  }
  # lazy fix for zero length strings splitting into nothing; would be better to
  # fix upstream...

  res[!chars] <- list(character(0L))
  res[x.na] <- list(NA_character_)
  res
}

