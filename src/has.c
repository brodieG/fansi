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

/*
 * Check if a CHARSXP contains ANSI esc sequences
 */

#include "fansi.h"

int FANSI_has_int(SEXP x) {
  if(TYPEOF(x) != CHARSXP) error("Argument `x` must be CHRSXP.");
  if(x == NA_STRING) return NA_LOGICAL;
  else return FANSI_find_esc(CHAR(x)).valid != 0;
}
/*
 * Check if a CHARSXP contains ANSI esc sequences
 */
SEXP FANSI_has(SEXP x) {
  if(TYPEOF(x) != STRSXP) error("Argument `x` must be character.");
  R_xlen_t len = XLENGTH(x);

  SEXP res = PROTECT(allocVector(LGLSXP, len));
  int * res_int = LOGICAL(res);

  for(R_xlen_t i = 0; i < len; ++i)
    res_int[i] = FANSI_has_int(STRING_ELT(x, i));

  UNPROTECT(1);
  return res;
}

