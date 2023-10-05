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

#' Control Sequence Aware Version of strtrim
#'
#' A drop in replacement for [`base::strtrim`], with the difference that all
#' C0 control characters such as newlines, carriage returns, etc., are always
#' treated as zero width, whereas in base it may vary with platform / R version.
#'
#' `strtrim2_ctl` adds the option of converting tabs to spaces before trimming.
#' This is the only difference between `strtrim_ctl` and `strtrim2_ctl`.
#'
#' @export
#' @note Non-ASCII strings are converted to and returned in UTF-8 encoding.
#'   Width calculations will not work properly in R < 3.2.2.
#' @inheritParams base::strtrim
#' @inheritParams strwrap_ctl
#' @inherit substr_ctl seealso
#' @return Like [`base::strtrim`], except that _Control Sequences_ are treated
#'   as zero width.
#' @examples
#' strtrim_ctl("\033[42mHello world\033[m", 6)

strtrim_ctl <- function(
  x, width, warn=getOption('fansi.warn', TRUE), ctl='all',
  normalize=getOption('fansi.normalize', FALSE),
  carry=getOption('fansi.carry', FALSE),
  terminate=getOption('fansi.terminate', TRUE)
) {
  strtrim2_ctl(
    x=x, width=width, warn=warn, ctl=ctl, normalize=normalize,
    carry=carry, terminate=terminate
  )
}
#' @export
#' @rdname strtrim_ctl

strtrim2_ctl <- function(
  x, width, warn=getOption('fansi.warn', TRUE),
  tabs.as.spaces=getOption('fansi.tabs.as.spaces', FALSE),
  tab.stops=getOption('fansi.tab.stops', 8L),
  ctl='all', normalize=getOption('fansi.normalize', FALSE),
  carry=getOption('fansi.carry', FALSE),
  terminate=getOption('fansi.terminate', TRUE)
) {
  ## modifies / creates NEW VARS in fun env
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
  # can assume all term cap available for these purposes
  term.cap.int <- 1L
  width <- as.integer(width)
  tab.stops <- as.integer(tab.stops)

  # a bit inefficient to rely on strwrap, but oh well
  res <- .Call(
    FANSI_strwrap_csi,
    x, width,
    0L, 0L,    # indent, exdent
    "", "",    # prefix, initial
    TRUE, "",  # wrap always
    FALSE,     # strip spaces
    tabs.as.spaces, tab.stops,
    WARN.INT, term.cap.int,
    TRUE,      # first only
    CTL.INT,
    normalize, carry, terminate
  )
  if(normalize) normalize_state(res, warn=FALSE) else res
}
#' Control Sequence Aware Version of strtrim
#'
#' These functions are deprecated in favor of the [`strtrim_ctl`] flavors.
#'
#' @inheritParams strtrim_ctl
#' @inherit strtrim_ctl return
#' @keywords internal
#' @export

strtrim_sgr <- function(
  x, width, warn=getOption('fansi.warn', TRUE),
  normalize=getOption('fansi.normalize', FALSE),
  carry=getOption('fansi.carry', FALSE),
  terminate=getOption('fansi.terminate', TRUE)
)
  strtrim_ctl(
    x=x, width=width, warn=warn, ctl='sgr', normalize=normalize,
    carry=carry, terminate=terminate
  )

#' @export
#' @rdname strtrim_sgr

strtrim2_sgr <- function(x, width, warn=getOption('fansi.warn', TRUE),
  tabs.as.spaces=getOption('fansi.tabs.as.spaces', FALSE),
  tab.stops=getOption('fansi.tab.stops', 8L),
  normalize=getOption('fansi.normalize', FALSE),
  carry=getOption('fansi.carry', FALSE),
  terminate=getOption('fansi.terminate', TRUE)
)
  strtrim2_ctl(
    x=x, width=width, warn=warn, tabs.as.spaces=tabs.as.spaces,
    tab.stops=tab.stops, ctl='sgr', normalize=normalize,
    carry=carry, terminate=terminate
  )
