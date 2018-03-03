/*
 * Copyright (C) 2018  Brodie Gaslam
 *
 * This file is part of "fansi - ANSI Escape Aware String Functions"
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * Go to <https://www.r-project.org/Licenses/GPL-2> for a copy of the license.
 */

SEXP FANSI_nchar(
  SEXP x, SEXP type, SEXP allowNA, SEXP keepNA, SEXP what,
  SEXP warn
) {
  if(
    TYPEOF(x) != STRSXP || TYPEOF(type) != INTSXP || 
    TYPEOF(allowNA) != LGLSXP || TYPEOF(keepNA) != LGLSXP ||
    TYPEOF(what) != INTSXP || TYPEOF(warn) != LGLSXP
  )
    error("Internal error: input type error; contact maintainer");

  int what_int = FANSI_what_as_int(what);

}
