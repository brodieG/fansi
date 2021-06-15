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

#' ANSI Control Sequence Aware Version of strtrim
#'
#' One difference with [base::strtrim] is that all C0 control characters such as
#' newlines, carriage returns, etc., are treated as zero width.
#'
#' `strtrim2_ctl` adds the option of converting tabs to spaces before trimming.
#' This is the only difference between `strtrim_ctl` and `strtrim2_ctl`.
#'
#' @note Non-ASCII strings are converted to and returned in UTF-8 encoding.
#'   Width calculations will not work correctly with R < 3.2.2.
#' @export
#' @inheritSection substr_ctl _ctl vs. _sgr
#' @seealso [`fansi`] for details on how _Control Sequences_ are
#'   interpreted, particularly if you are getting unexpected results,
#'   [`normalize_sgr`] for more details on what the `normalize` parameter does.
#' @inheritParams base::strtrim
#' @inheritParams strwrap_ctl
#' @examples
#' strtrim_ctl("\033[42mHello world\033[m", 6)

strtrim_ctl <- function(
  x, width, warn=getOption('fansi.warn'), ctl='all',
  normalize=getOption('fansi.normalize', FALSE),
  carry=getOption('fansi.carry', FALSE),
  terminate=getOption('fansi.terminate', TRUE)
) {
  VAL_IN_ENV(
    x=x, warn=warn, ctl=ctl, normalize=normalize, carry=carry,
    terminate=terminate
  )
  if(!is.numeric(width) || length(width) != 1L || is.na(width) || width < 0)
    stop(
      "Argument `width` must be a positive scalar numeric representable ",
      "as an integer."
    )
  width <- as.integer(width)
  # can assume all term cap available for these purposes

  term.cap.int <- seq_along(VALID.TERM.CAP)

  # a bit inefficient to rely on strwrap, but oh well

  res <- .Call(
      FANSI_strwrap_csi,
      enc2utf8(x), width,
      0L, 0L,    # indent, exdent
      "", "",    # prefix, initial
      TRUE, "",  # wrap always
      FALSE,     # strip spaces
      FALSE, 8L,
      warn, term.cap.int,
      TRUE,      # first only
      ctl.int,
      normalize,
      carry,
      terminate
  )
  if(normalize) normalize_sgr(res) else res
}
#' @export
#' @rdname strtrim_ctl

strtrim2_ctl <- function(
  x, width, warn=getOption('fansi.warn'),
  tabs.as.spaces=getOption('fansi.tabs.as.spaces'),
  tab.stops=getOption('fansi.tab.stops'),
  ctl='all', normalize=getOption('fansi.normalize', FALSE),
  carry=getOption('fansi.carry', FALSE),
  terminate=getOption('fansi.terminate', TRUE)
) {
  VAL_IN_ENV(
    x=x, warn=warn, ctl=ctl,
    tabs.as.spaces=tabs.as.spaces, tab.stops=tab.stops,
    normalize=normalize, carry=carry,
    terminate=terminate
  )
  if(!is.numeric(width) || length(width) != 1L || is.na(width) || width < 0)
    stop(
      "Argument `width` must be a positive scalar numeric representable ",
      "as an integer."
    )
  width <- as.integer(width)

  # can assume all term cap available for these purposes
  term.cap.int <- seq_along(VALID.TERM.CAP)
  width <- as.integer(width)
  tab.stops <- as.integer(tab.stops)

  # a bit inefficient to rely on strwrap, but oh well
  res <- .Call(
      FANSI_strwrap_csi,
      enc2utf8(x), width,
      0L, 0L,    # indent, exdent
      "", "",    # prefix, initial
      TRUE, "",  # wrap always
      FALSE,     # strip spaces
      tabs.as.spaces, tab.stops,
      warn, term.cap.int,
      TRUE,      # first only
      ctl.int,
      normalize, carry, terminate
  )
  if(normalize) normalize_sgr(res) else res
}
#' @export
#' @rdname strtrim_ctl

strtrim_sgr <- function(
  x, width, warn=getOption('fansi.warn'),
  normalize=getOption('fansi.normalize', FALSE),
  carry=getOption('fansi.carry', FALSE),
  terminate=getOption('fansi.terminate', TRUE)
)
  strtrim_ctl(
    x=x, width=width, warn=warn, ctl='sgr', normalize=normalize,
    carry=carry, terminate=terminate
  )

#' @export
#' @rdname strtrim_ctl

strtrim2_sgr <- function(x, width, warn=getOption('fansi.warn'),
  tabs.as.spaces=getOption('fansi.tabs.as.spaces'),
  tab.stops=getOption('fansi.tab.stops'),
  normalize=getOption('fansi.normalize', FALSE),
  carry=getOption('fansi.carry', FALSE),
  terminate=getOption('fansi.terminate', TRUE)
)
  strtrim2_ctl(
    x=x, width=width, warn=warn, tabs.as.spaces=tabs.as.spaces,
    tab.stops=tab.stops, ctl='sgr', normalize=normalize,
    carry=carry, terminate=terminate
  )
