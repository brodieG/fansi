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

SEXP FANSI_unhandled_esc(SEXP x) {
  if(TYPEOF(x) != STRSXP)
    error("Argument `x` must be a character vector.");

  R_xlen_t x_len = XLENGTH(x);
  if(x_len >= INT_MAX)
    // nocov start
    error(
      "This function does not support vectors of length INT_MAX or longer."
    );
    // nocov end

  SEXP zero_vec = PROTECT(allocVector(INTSXP, 0));
  SEXP no_warn = PROTECT(ScalarLogical(0));
  SEXP res, res_start;
  res = res_start = R_NilValue;
  PROTECT_INDEX ipx;
  PROTECT_WITH_INDEX(res, &ipx);  // reserve spot if we need to alloc later
  int any_errors = 0;
  int err_count = 0;
  int break_early = 0;

  for(R_xlen_t i = 0; i < x_len; ++i) {
    FANSI_interrupt(i);
    SEXP chrsxp = STRING_ELT(x, i);

    if(chrsxp != NA_STRING && LENGTH(chrsxp)) {
      struct FANSI_string_as_utf8 string_dat = FANSI_string_as_utf8(chrsxp);
      const char * string, * string_start;

      string = string_start = string_dat.string;

      struct FANSI_state state = FANSI_state_init(string, no_warn, zero_vec);
      int has_errors = 0;

      while(*string && (string = strchr(string, 0x1b))) {
        // Since we don't care about width, etc, we only use the state objects to
        // parse the ESC sequences

        state.pos_byte = (string - string_start);

        int esc_start = state.pos_ansi;
        state = FANSI_read_next(state);
        if(state.err_code) {
          if(err_count == INT_MAX) {
            warning(
              "%s%s",
              "There are more than INT_MAX unhandled sequences, returning ",
              "first INT_MAX errors."
            );
            break_early = 1;
            break;
          }
          if(esc_start == INT_MAX || state.pos_ansi == INT_MAX)
            error(
              "%s%s",
              "Internal error: computed offset is INT_MAX, shouldn't happen; ",
              "contact maintainer."
            );
          if(!has_errors) has_errors = 1;

          SEXP err_vals = PROTECT(allocVector(INTSXP, 5));
          INTEGER(err_vals)[0] = i + 1;
          INTEGER(err_vals)[1] = esc_start + 1;
          INTEGER(err_vals)[2] = state.pos_ansi;
          INTEGER(err_vals)[3] = state.err_code;
          INTEGER(err_vals)[4] = string_dat.translated;
          SEXP err_vals_list = PROTECT(list1(err_vals));

          if(!any_errors) {
            any_errors = 1;
            REPROTECT(err_vals_list, ipx);
            res = res_start = err_vals_list;
          } else {
            SETCDR(res, err_vals_list);
            res = CDR(res);
          }
          ++err_count;
          UNPROTECT(2);
        }
        string += state.pos_byte;
      }
      if(break_early) break;
    }
  }
  // Convert result to a list that we could easily turn into a DFs

  SEXP res_fin = PROTECT(allocVector(VECSXP, 5));
  SEXP res_idx = PROTECT(allocVector(INTSXP, err_count));
  SEXP res_esc_start = PROTECT(allocVector(INTSXP, err_count));
  SEXP res_esc_end = PROTECT(allocVector(INTSXP, err_count));
  SEXP res_err_code = PROTECT(allocVector(INTSXP, err_count));
  SEXP res_translated = PROTECT(allocVector(LGLSXP, err_count));

  res = res_start;

  for(int i = 0; i < err_count; ++i) {
    FANSI_interrupt(i);
    if(res == R_NilValue)
      // nocov start
      error(
        "%s%s",
        "Internal Error: mismatch between list and err count; "
        "contact maintainer."
      );
      // nocov end
    INTEGER(res_idx)[i] = INTEGER(CAR(res))[0];
    INTEGER(res_esc_start)[i] = INTEGER(CAR(res))[1];
    INTEGER(res_esc_end)[i] = INTEGER(CAR(res))[2];
    INTEGER(res_err_code)[i] = INTEGER(CAR(res))[3];
    LOGICAL(res_translated)[i] = INTEGER(CAR(res))[4];
    res = CDR(res);
  }
  SET_VECTOR_ELT(res_fin, 0, res_idx);
  SET_VECTOR_ELT(res_fin, 1, res_esc_start);
  SET_VECTOR_ELT(res_fin, 2, res_esc_end);
  SET_VECTOR_ELT(res_fin, 3, res_err_code);
  SET_VECTOR_ELT(res_fin, 4, res_translated);
  UNPROTECT(9);
  return res_fin;
}
