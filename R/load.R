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


# nocov start
.onLoad <- function(libname, pkgname) {
  # Scheme defaults are fairly complex...

  check_assumptions()

  # Default options; beware of defining default options that may have different
  # values during package install, which is when this list is contructed, and
  # function runtime

  .default.opts <- list(
    fansi.tabs.as.spaces=FALSE,
    fansi.tab.stops=8L,
    fansi.warn=TRUE,
    fansi.ctrl="all",
    fansi.term.cap=c(
      if(isTRUE(Sys.getenv('COLORTERM') %in% c('truecolor', '24bit')))
      'truecolor',
      'bright', '256'
    ),
    # This is not a particularly good default setting as it may exceed or fail
    # to cover the interline distance when two lines have background colors.  To
    # ensure lines are exactly touching use inline-block, although that has it's
    # own issues.  Otherwise specify your own values.

    fansi.css="PRE.fansi SPAN {padding-top: .25em; padding-bottom: .25em};"
  )
  # Scheme defaults are fairly complex...

  existing.opts <- options()
  options(.default.opts[setdiff(names(.default.opts), names(existing.opts))])
  R.ver.gte.3.2.2 <<- getRversion() >= "3.2.2"
}
.onAttach <- function(libname, pkgname) {
  if(!R.ver.gte.3.2.2) {
    packageStartupMessage(
      "`fansi` capabilities are degraded with R versions less than 3.2.2.  In ",
      "particular string width calculations will be incorrect for wide and/or ",
      "zero width characters."
    )
  }
}
.onUnload <- function(libpath) {
  library.dynam.unload("fansi", libpath)
}
# nocov end
