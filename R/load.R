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
    fansi.term.cap=c(
      if(isTRUE(Sys.getenv('COLORTERM') %in% c('truecolor', '24bit')))
      'truecolor',
      'bright', '256'
    )
  )
  # Scheme defaults are fairly complex...

  existing.opts <- options()
  options(.default.opts[setdiff(names(.default.opts), names(existing.opts))])
  R.ver.gte.3.2 <<- getRversion() >= "3.2.0"
}

.onUnload <- function(libpath) {
  library.dynam.unload("fansi", libpath)
}
# nocov end
