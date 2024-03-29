## Copyright (C) Brodie Gaslam
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
## Go to <https://www.r-project.org/Licenses> for copies of the licenses.

#' Control Sequence Aware Version of strsplit
#'
#' A drop-in replacement for [`base::strsplit`].
#'
#' This function works by computing the position of the split points after
#' removing _Control Sequences_, and uses those positions in conjunction with
#' [`substr_ctl`] to extract the pieces.  This concept is borrowed from
#' `crayon::col_strsplit`.  An important implication of this is that you cannot
#' split by _Control Sequences_ that are being treated as _Control Sequences_.
#' You can however limit which control sequences are treated specially via the
#' `ctl` parameters (see examples).
#'
#' @note The split positions are computed after both `x` and `split` are
#'   converted to UTF-8.
#' @export
#' @param x a character vector, or, unlike [`base::strsplit`] an object that can
#'   be coerced to character.
#' @inheritParams base::strsplit
#' @inheritParams strwrap_ctl
#' @inherit substr_ctl seealso
#' @inheritSection substr_ctl Control and Special Sequences
#' @inheritSection substr_ctl Output Stability
#' @inheritSection substr_ctl Bidirectional Text
#' @note Non-ASCII strings are converted to and returned in UTF-8 encoding.
#'   Width calculations will not work properly in R < 3.2.2.
#' @return Like [`base::strsplit`], with _Control Sequences_ excluded.
#' @examples
#' strsplit_ctl("\033[31mhello\033[42m world!", " ")
#'
#' ## Splitting by newlines does not work as they are _Control
#' ## Sequences_, but we can use `ctl` to treat them as ordinary
#' strsplit_ctl("\033[31mhello\033[42m\nworld!", "\n")
#' strsplit_ctl("\033[31mhello\033[42m\nworld!", "\n", ctl=c("all", "nl"))

strsplit_ctl <- function(
  x, split, fixed=FALSE, perl=FALSE, useBytes=FALSE,
  warn=getOption('fansi.warn', TRUE),
  term.cap=getOption('fansi.term.cap', dflt_term_cap()),
  ctl='all', normalize=getOption('fansi.normalize', FALSE),
  carry=getOption('fansi.carry', FALSE),
  terminate=getOption('fansi.terminate', TRUE)
) {
  ## modifies / creates NEW VARS in fun env
  VAL_IN_ENV(
    x=x, warn=warn, term.cap=term.cap, ctl=ctl, normalize=normalize,
    carry=carry, terminate=terminate, round="start"
  )
  if(is.null(split)) split <- ""
  split <- enc_to_utf8(as.character(split))
  if(!length(split)) split <- ""
  if(anyNA(split)) stop("Argument `split` may not contain NAs.")
  if(any(Encoding(split) == "bytes"))
    stop("Argument `split` may not be \"bytes\" encoded.")
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
  x.strip <- strip_ctl(x, warn=FALSE, ctl=ctl)
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
        x=rep(x[i], length.out=length(starts)),
        start=starts, stop=ends, type.int=0L,
        round.int=ROUND.INT,
        tabs.as.spaces=FALSE, tab.stops=8L, warn.int=WARN.INT,
        term.cap.int=TERM.CAP.INT, x.len=length(starts),
        ctl.int=CTL.INT, normalize=normalize,
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
#' Check for Presence of Control Sequences
#'
#' This function is deprecated in favor of the [`strsplit_ctl`].
#'
#' @inheritParams strsplit_ctl
#' @inherit strsplit_ctl return
#' @keywords internal
#' @export

strsplit_sgr <- function(
  x, split, fixed=FALSE, perl=FALSE, useBytes=FALSE,
  warn=getOption('fansi.warn', TRUE),
  term.cap=getOption('fansi.term.cap', dflt_term_cap()),
  normalize=getOption('fansi.normalize', FALSE),
  carry=getOption('fansi.carry', FALSE),
  terminate=getOption('fansi.terminate', TRUE)
)
  strsplit_ctl(
    x=x, split=split, fixed=fixed, perl=perl, useBytes=useBytes,
    warn=warn, term.cap=term.cap, ctl='sgr', normalize=normalize,
    carry=carry, terminate=terminate
  )
