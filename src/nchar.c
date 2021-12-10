/*
 * Copyright (C) 2021  Brodie Gaslam
 *
 *  This file is part of "fansi - ANSI Control Sequence Aware String Functions"
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
 * Go to <https://www.r-project.org/Licenses/GPL-2> for a copy of the license.
 */

#include "fansi.h"

/*
 * Not very optimized.
 *
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

  int prt = 0;
  int keepNA_int = asLogical(keepNA);
  int type_int = asInteger(type);
  int zz = asLogical(z);
  if(zz && (type_int != FANSI_COUNT_CHARS || !asLogical(allowNA)))
    error("Internal Error: `type` must be \"char\" for `nzchar_ctl`");

  const char * arg = "x";;

  R_xlen_t x_len = XLENGTH(x);

  SEXP res = PROTECT(allocVector(zz ? LGLSXP : INTSXP, x_len)); prt++;
  int * resi = zz ? LOGICAL(res) : INTEGER(res);

  struct FANSI_state state;

  for(R_xlen_t i = 0; i < x_len; ++i) {
    FANSI_interrupt(i);
    if(!i) {
      state = FANSI_state_init_full(
        x, warn, term_cap, allowNA, keepNA, type, ctl, i
      );
    } else FANSI_state_reinit(&state, x, i);

    if(STRING_ELT(x, i) == R_NaString) {
      // NA case, see ?nchar, note nzchar behavior is incorrectly doc'ed
      if(
        keepNA_int == 1 ||
        (
          keepNA_int == NA_LOGICAL &&
          !(type_int == FANSI_COUNT_WIDTH || type_int == FANSI_COUNT_GRAPH) &&
          !zz
        )
      ) {
        resi[i] = zz ? NA_LOGICAL : NA_INTEGER;
      } else resi[i] = zz ? 1 : 2;
    } else {
      if(zz) {  // nzchar mode
        while(state.string[state.pos.x] && !state.pos.w) {
          FANSI_read_next(&state, i, arg);
          if(state.pos.w) break;
        }
        resi[i] = state.pos.w > 0;
      } else {
        // Invalid utf8 in !ALLOWNA should cause errors in read_next.  Whether
        // errors are thrown is controlled via the warn bits set from R.
        while(state.string[state.pos.x]) {
          FANSI_read_next(&state, i, arg);
          if(FANSI_GET_ERR(state.status) == ERR_BAD_UTF8) break;
        }
        unsigned int err_tmp = FANSI_GET_ERR(state.status);
        if (err_tmp == ERR_BAD_UTF8) {
          if(state.settings & FANSI_SET_ALLOWNA) resi[i] = NA_INTEGER;
          else error("Internal Error: invalid encoding unhandled."); // nocov
        } else resi[i] = state.pos.w;
  } } }
  UNPROTECT(prt);
  return res;
}
