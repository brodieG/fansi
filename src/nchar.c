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

SEXP FANSI_nzchar(
  SEXP x, SEXP keepNA, SEXP warn, SEXP term_cap, SEXP ctl
) {
  if(
    TYPEOF(x) != STRSXP ||
    TYPEOF(keepNA) != LGLSXP ||
    TYPEOF(warn) != LGLSXP ||
    TYPEOF(term_cap) != INTSXP ||
    TYPEOF(ctl) != INTSXP
  )
    error("Internal error: input type error; contact maintainer"); // nocov

  int keepNA_int = asInteger(keepNA);
  int warn_int = asInteger(warn);
  int warned = 0;

  R_xlen_t x_len = XLENGTH(x);

  SEXP res = PROTECT(allocVector(LGLSXP, x_len));
  int * resl = LOGICAL(res);

  for(R_xlen_t i = 0; i < x_len; ++i) {
    FANSI_interrupt(i);
    SEXP string_elt = STRING_ELT(x, i);
    FANSI_check_chrsxp(string_elt, i);

    if(string_elt == R_NaString) {
      if(keepNA_int == 1) {
        resl[i] = NA_LOGICAL;
      } else resl[i] = 1;
    } else {
      const char * string = CHAR(string_elt);
      // Read through any leading controls
      resl[i] = 0;
      if(FANSI_maybe_ctl(*string)) {
        // could be a control
        SEXP R_false = PROTECT(ScalarLogical(0));
        struct FANSI_state state = FANSI_state_init_ctl(x, R_false, ctl, i);
        UNPROTECT(1);

        while(FANSI_maybe_ctl(*string)) {
          struct FANSI_ctl_pos pos =
            FANSI_find_ctl(state, warn_int && !warned, i, 1);

          warned = warned || pos.warn;
          if(!pos.len) {  // Not an escape
            resl[i] = 1;
            break;
          }
          state.pos_byte = pos.offset + pos.len;
          string = state.string + state.pos_byte;
        }
      }
      if(*string) resl[i] = 1;  // at least one non-esc bytes
  } }
  UNPROTECT(1);
  return res;
}
