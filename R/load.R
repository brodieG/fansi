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


# nocov start
.onLoad <- function(libname, pkgname) {
  # Scheme defaults are fairly complex...

  check_assumptions()
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
