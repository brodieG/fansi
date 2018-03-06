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
/*
 * @param term_cap is expected to be provided in full by the R fun, this is just
 *   purely for convenience and doesn't do anything.
 */
SEXP FANSI_nchar(
  SEXP x, SEXP type, SEXP allowNA, SEXP keepNA, SEXP warn, SEXP term_cap
) {
  if(
    TYPEOF(x) != STRSXP ||
    TYPEOF(allowNA) != LGLSXP ||
    TYPEOF(keepNA) != LGLSXP ||
    TYPEOF(warn) != LGLSXP ||
    TYPEOF(type) != STRSXP || XLENGTH(type) != 1 ||
    TYPEOF(term_cap) != INTSXP
  )
    error("Internal error: input type error; contact maintainer");

  const char * valid_types[2] = {"chars", "width"};
  int type_int = FANSI_pmatch(type, valid_types, 2, "type");
  int allowNA_int = asLogical(allowNA);
  int keepNA_int = asLogical(keepNA);
  int warn_int = asLogical(warn);
  int warned = 0;

  R_xlen_t x_len = XLENGTH(x);

  SEXP res = PROTECT(allocVector(INTSXP, x_len));
  SEXP R_false = PROTECT(ScalarLogical(0));

  for(R_len_t i = 0; i < x_len; ++i) {
    FANSI_interrupt(i);
    SEXP string_elt = STRING_ELT(x, i);

    if(string_elt == R_NaString) {
      if((keepNA_int == NA_LOGICAL && type_int != 1) || keepNA_int == 1) {
        INTEGER(res)[i] = NA_INTEGER;
      } else INTEGER(res)[i] = 2;
    } else {
      const char * string = FANSI_string_as_utf8(string_elt).string;
      struct FANSI_state state =
        FANSI_state_init_full(string, R_false, term_cap, allowNA, keepNA);

      while(state.string[state.pos_byte]) {
        int is_esc = state.string[state.pos_byte] == 27;
        state = FANSI_read_next(state);
        if(
          is_esc && warn_int && !warned &&
          (state.err_code >= 5 &&  state.err_code <= 7)
        ) {
          warned = 1;
          warning(
            "Encountered %s ESC sequence at index [%.0f], %s%s",
            "invalid or possibly incorrectly handled",
            (double) i + 1,
            "see `?unhandled_esc`; you can use `warn=FALSE` to turn ",
            "off these warnings."
      );} }
      if(allowNA_int && state.nchar_err) {
        // We could consider breaking as soon as we hit nchar_err, but we let it
        // continue since these should be rarely encountered and it would create
        // one more command in the tight loop.
        INTEGER(res)[i] = NA_INTEGER;
      } else {
        switch(type_int) {
          case 0: INTEGER(res)[i] = state.pos_raw; break;
          case 1: INTEGER(res)[i] = state.pos_width; break;
          default:
            error("Internal Error: unknown `type` value; contact maintainer.");
  } } } }
  UNPROTECT(2);
  return res;
}
SEXP FANSI_nzchar(SEXP x, SEXP keepNA, SEXP warn, SEXP term_cap) {
  if(
    TYPEOF(x) != STRSXP ||
    TYPEOF(keepNA) != LGLSXP ||
    TYPEOF(warn) != LGLSXP ||
    TYPEOF(term_cap) != INTSXP
  )
    error("Internal error: input type error; contact maintainer");

  int keepNA_int = asInteger(keepNA);
  int warned = 0;

  R_xlen_t x_len = XLENGTH(x);

  SEXP res = PROTECT(allocVector(LGLSXP, x_len));

  for(R_len_t i = 0; i < x_len; ++i) {
    FANSI_interrupt(i);
    SEXP string_elt = STRING_ELT(x, i);
    if(string_elt == R_NaString) {
      if(keepNA_int == 1) {
        LOGICAL(res)[i] = NA_LOGICAL;
      } else LOGICAL(res)[i] = 1;
    } else {
      // Don't bother converting to UTF8

      const char * string = CHAR(string_elt);

      while((*string > 0 && *string < 32) || *string == 127) {
        struct FANSI_csi_pos pos = FANSI_find_esc(string, FANSI_WHAT_ALL);
        if(!warned && (!pos.valid || (pos.what & (1 << 4)))) {
          warning(
            "Encountered %s ESC sequence at index [%.0f], %s%s",
            !pos.valid ? "invalid" : "possibly incorrectly handled",
            (double) i + 1,
            "see `?unhandled_esc`; you can use `warn=FALSE` to turn ",
            "off these warnings."
          );
        }
        string += pos.len;
      }
      LOGICAL(res)[i] = *string != 0;
  } }
  UNPROTECT(1);
  return res;
}
