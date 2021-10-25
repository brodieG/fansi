/*
 * Copyright (C) 2021  Brodie Gaslam
 *
 *  This file is part of "fansi - ANSI Control Sequence Aware String Functions"
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

#include "fansi.h"

/*
 * @param z logical(1L) whether to stop once we confirm there is one non-sgr
 *  character.
 */

SEXP FANSI_nchar(
  SEXP x, SEXP type, SEXP keepNA, SEXP allowNA,
  SEXP warn, SEXP term_cap, SEXP ctl, SEXP z
) {
  if(TYPEOF(z) != LGLSXP || XLENGTH(z) != 1)
    error("Internal error: `z` type error; contact maintainer"); // nocov
  if(TYPEOF(keepNA) != LGLSXP || XLENGTH(keepNA) != 1)
    error("Internal error: `keepNA` type error; contact maintainer"); // nocov
  if(TYPEOF(type) != INTSXP || XLENGTH(type) != 1)
    error("Internal error: `type` type error; contact maintainer"); // nocov

  int keepNA_int = asInteger(keepNA);
  int type_int = asInteger(type);
  int zz = asInteger(z);

  R_xlen_t x_len = XLENGTH(x);

  // Handle wrnings explicitly as we have a different threshold for warning with
  // nchar than with normal reading.
  SEXP warn2 = PROTECT(ScalarLogical(0));
  SEXP res = PROTECT(allocVector(zz ? LGLSXP : INTSXP, x_len));
  int * resi = zz ? LOGICAL(res) : INTEGER(res);
  int warned = 0;

  struct FANSI_state state;

  for(R_xlen_t i = 0; i < x_len; ++i) {
    FANSI_interrupt(i);
    if(STRING_ELT(x, i) == R_NaString) {
      // NA case, see ?nchar
      if(keepNA_int == 1 || (keepNA_int == NA_INTEGER && !type_int)) {
        resi[i] = zz ? NA_LOGICAL : NA_INTEGER;
      } else resi[i] = zz ? 1 : 2;
    } else {
      if(!i) {
        state = FANSI_state_init_full(
          x, warn2, term_cap, allowNA, keepNA, type, ctl, i
        );
      } else {
        state = FANSI_state_reinit(state, x, i);
      }
      while(state.string[state.pos_byte]) {
        state = FANSI_read_next(state, i, 1);
        // early exits
        if((zz && state.pos_raw) || state.err_code == 9) break;
      }
      if(zz) {  // nzchar mode
        resi[i] = !state.pos_raw;
      } else if (state.err_code == 9) {
        if(state.allowNA) {
          resi[i] = zz ? NA_LOGICAL : NA_INTEGER;
        } else {
          // read_next should have had an error on invalid encoding
          error("Internal Error: invalid encoding unhandled."); // nocov
        }
      } else if (!state.use_nchar) {// "char" mode
        resi[i] = state.pos_raw;
      } else {                      // "width", "grapheme" modes
        resi[i] = state.pos_width;
      }
      if(state.err_code ) {

      }
  } }
  UNPROTECT(2);
  return res;
}
