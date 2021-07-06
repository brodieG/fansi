/*
 * Copyright (C) 2021  Brodie Gaslam
 *
 * This file is part of "fansi - ANSI Control Sequence Aware String Functions"
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

static int has_int(SEXP x, int * warn, SEXP ctl, R_xlen_t i) {
  if(TYPEOF(x) != STRSXP) error("Argument `x` must be STRSXP.");
  int res = 0;
  const char * xc = CHAR(STRING_ELT(x, i));
  int off_init = FANSI_seek_ctl(xc);
  if(xc + off_init) {
    SEXP R_false = PROTECT(ScalarLogical(0));
    struct FANSI_state state = FANSI_state_init_ctl(x, R_false, ctl, i);
    UNPROTECT(1);
    state.pos_byte = off_init;
    struct FANSI_ctl_pos pos = FANSI_find_ctl(state, *warn, i);
    res = pos.len > 0;
    if(pos.warn) *warn = 0;
  }
  return res;
}
/*
 * Check if a CHARSXP contains ANSI esc sequences
 */
SEXP FANSI_has(SEXP x, SEXP ctl, SEXP warn) {
  if(TYPEOF(x) != STRSXP) error("Argument `x` must be character.");
  if(TYPEOF(ctl) != INTSXP) error("Internal Error: `ctl` must be INTSXP.");
  R_xlen_t len = XLENGTH(x);

  SEXP res = PROTECT(allocVector(LGLSXP, len));
  int * res_int = LOGICAL(res);
  int warn_int = asLogical(warn);

  for(R_xlen_t i = 0; i < len; ++i) {
    FANSI_interrupt(i);
    SEXP chrsxp = STRING_ELT(x, i);
    FANSI_check_chrsxp(chrsxp, i);
    if(chrsxp == NA_STRING) res_int[i] = NA_LOGICAL;
    else res_int[i] = has_int(x, &warn_int, ctl, i);
  }
  UNPROTECT(1);
  return res;
}

