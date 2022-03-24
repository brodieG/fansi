/*
 * Copyright (C) 2022 Brodie Gaslam
 *
 * This file is part of "fansi - ANSI Control Sequence Aware String Functions"
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2 or 3 of the License.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * Go to <https://www.r-project.org/Licenses> for a copies of the licenses.
 */

/*
 * Check if a CHARSXP contains ANSI esc sequences
 */

#include "fansi.h"
/*
 * Check if a CHARSXP contains ANSI esc sequences
 */
SEXP FANSI_has(SEXP x, SEXP ctl, SEXP warn) {
  if(TYPEOF(x) != STRSXP) error("Argument `x` must be character.");
  if(TYPEOF(ctl) != INTSXP) error("Internal Error: `ctl` must be INTSXP.");
  R_xlen_t len = XLENGTH(x);

  SEXP res = PROTECT(allocVector(LGLSXP, len));
  int * res_int = LOGICAL(res);
  struct FANSI_state state;
  const char * arg = "x";

  for(R_xlen_t i = 0; i < len; ++i) {
    if(!i) state = FANSI_state_init_ctl(x, warn, ctl, i);
    else FANSI_state_reinit(&state, x, i);
    FANSI_interrupt(i);
    SEXP chrsxp = STRING_ELT(x, i);
    if(chrsxp != NA_STRING) {
      int res = 0;
      const char * xc = CHAR(chrsxp);
      int off_init = FANSI_seek_ctl(xc);
      if(*(xc + off_init)) {
        state.pos.x = off_init;
        FANSI_find_ctl(&state, i, arg);
        res = (state.status & CTL_MASK) > 0;
      }
      res_int[i] = res;
    } else {
      res_int[i] = NA_LOGICAL;
    }
  }
  UNPROTECT(1);
  return res;
}

