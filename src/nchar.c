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

#include "fansi.h"

SEXP FANSI_nchar(
  SEXP x, SEXP type, SEXP allowNA, SEXP keepNA, SEXP what, SEXP warn,
  SEXP term_cap
) {
  if(
    TYPEOF(x) != STRSXP || TYPEOF(type) != INTSXP ||
    TYPEOF(allowNA) != LGLSXP || TYPEOF(keepNA) != LGLSXP ||
    TYPEOF(what) != INTSXP || TYPEOF(warn) != LGLSXP ||
    XLENGTH(type) != 1
  )
    error("Internal error: input type error; contact maintainer");

  const char * type_chr = CHAR(STRING_ELT(type, 0));
  int type_int;
  if(!strcomp(type_chr, "chars")) type_int = 1;
  else if(!strcomp(type_chr, "width")) type_int = 2;
  else if(!strcomp(type_chr, "bytes")) type_int = 3;
  else error("Internal error: unknown type selection.");  // nocov

  R_xlen_t x_len = XLENGTH(x);

  res = PROTECT(allocVector(INTSXP, x_len));

  for(R_len_t i, i < x_len; ++i) {
    FANSI_interrupt(i);

    const char * string = FANSI_string_as_utf8(STRING_ELT(x, i));
    struct FANSI_state state = FANSI_state_init(string, warn, term_cap);
    int pos_width_prev = 0;

    while(state.string[state.pos_byte]) {
      state = FANSI_read_next(state);
      if(type != 2 && state.pos_width == pos_width_prev) {
        // We found a special character or sequence since all of them are
        // zero width, and if we're not in width mode we want to exclude the
        // associated characters from being counted
      }

      state_prev =
    }
    switch(type_int) {
      case 1: INTEGER(res)[i] = state.pos_raw; break;
      case 2: INTEGER(res)[i] = state.pos_width; break;
      case 3: INTEGER(res)[i] = state.pos_byte; break;
    }





  }

  int what_int = FANSI_what_as_int(what);

  return R_NilValue;
}
