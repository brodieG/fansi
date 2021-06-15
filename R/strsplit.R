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

#' ANSI Control Sequence Aware Version of strsplit
#'
#' A drop-in replacement for [base::strsplit].  It will be noticeably slower,
#' but should otherwise behave the same way except for _Control Sequence_
#' awareness.
#'
#' This function works by computing the position of the split points after
#' removing _Control Sequences_, and uses those positions in conjunction with
#' [`substr_ctl`] to extract the pieces.  This concept is borrowed from
#' `crayon::col_strsplit`.  An important implication of this is that you cannot
#' split by _Control Sequences_ that are being treated as _Control Sequences_.
#' You can however limit which control sequences are treated specially via the
#' `ctl` parameters (see examples).
#'
#' @note Non-ASCII strings are converted to and returned in UTF-8 encoding.  The
#'   split positions are computed after both `x` and `split` are converted to
#'   UTF-8.
#' @seealso [`fansi`] for details on how _Control Sequences_ are
#'   interpreted, particularly if you are getting unexpected results,
#'   [`normalize_sgr`] for more details on what the `normalize` parameter does,
#'   [base::strsplit] for details on the splitting.
#' @export
#' @param x a character vector, or, unlike [base::strsplit] an object that can
#'   be coerced to character.
#' @inheritParams base::strsplit
#' @inheritParams strwrap_ctl
#' @inheritSection substr_ctl _ctl vs. _sgr
#' @return list, see [base::strsplit].
#' @examples
#' strsplit_sgr("\033[31mhello\033[42m world!", " ")
#'
#' ## Next two examples allow splitting by newlines, which
#' ## normally doesn't work as newlines are _Control Sequences_
#' strsplit_sgr("\033[31mhello\033[42m\nworld!", "\n")
#' strsplit_ctl("\033[31mhello\033[42m\nworld!", "\n", ctl=c("all", "nl"))


strsplit_ctl <- function(
  x, split, fixed=FALSE, perl=FALSE, useBytes=FALSE,
  warn=getOption('fansi.warn'), term.cap=getOption('fansi.term.cap'),
  ctl='all', normalize=getOption('fansi.normalize', FALSE),
  carry=getOption('fansi.carry', FALSE),
  terminate=getOption('fansi.terminate', TRUE)
) {
  VAL_IN_ENV(
    x=x, warn=warn, term.cap=term.cap, ctl=ctl, normalize=normalize,
    carry=carry, terminate=terminate
  )
  if(is.null(split)) split <- ""
  split <- enc2utf8(as.character(split))
  if(!length(split)) split <- ""
  if(anyNA(split)) stop("Argument `split` may not contain NAs.")
  if(any(Encoding(split) == "bytes"))
    stop("Argument `bytes` may not be \"bytes\" encoded.")
  if(!is.logical(fixed)) fixed <- as.logical(fixed)
  if(length(fixed) != 1L || is.na(fixed))
    stop("Argument `fixed` must be TRUE or FALSE.")

  if(!is.logical(perl)) perl <- as.logical(perl)
  if(length(perl) != 1L || is.na(perl))
    stop("Argument `perl` must be TRUE or FALSE.")

  if(!is.logical(useBytes)) useBytes <- as.logical(useBytes)
  if(length(useBytes) != 1L || is.na(useBytes))
    stop("Argument `useBytes` must be TRUE or FALSE.")

  # Need to handle recycling, complicated by the ability of strsplit to accept
  # multiple different split arguments

  x.na <- is.na(x)
  x.seq <- seq_along(x)
  s.seq <- seq_along(split)
  s.x.seq <- rep(s.seq, length.out=length(x)) * (!x.na)

  matches <- res <- vector("list", length(x))
  x.strip <- strip_ctl(x, warn=warn, ctl=ctl)
  chars <- nchar(x.strip)

  # Find the split locations and widths

  for(i in s.seq) {
    to.split <- s.x.seq == i & chars
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
        term.cap.int=term.cap.int, x.len=length(starts),
        ctl.int=ctl.int, normalize=normalize,
        carry=carry, terminate=terminate
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
#' @rdname strsplit_ctl
#' @export

strsplit_sgr <- function(
  x, split, fixed=FALSE, perl=FALSE, useBytes=FALSE,
  warn=getOption('fansi.warn'), term.cap=getOption('fansi.term.cap'),
  normalize=getOption('fansi.normalize', FALSE),
  carry=getOption('fansi.carry', FALSE),
  terminate=getOption('fansi.terminate', TRUE)
)
  strsplit_ctl(
    x=x, split=split, fixed=fixed, perl=perl, useBytes=useBytes,
    warn=warn, term.cap=term.cap, ctl='sgr', normalize=normalize,
    carry=carry, terminate=terminate
  )

# # old interface to split happening directly in C code
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


