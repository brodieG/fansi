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

#include "fansi.h"

/*
 * is only needed because the existing unique algo is so bad when
 * dealing with long strings that are the same, which is likely a common use
 * case for `substr`.
 */

SEXP FANSI_unique_chr(SEXP x) {
  if(TYPEOF(x) != STRSXP) error("Internal Error: type mismatch");

  SEXP x_srt = PROTECT(FANSI_sort_chr(x));

  // Loop and check how many deltas there are

  SEXP res, x_prev;
  R_xlen_t x_len = XLENGTH(x_srt);
  R_xlen_t u_count = 1;

  if(x_len >= 2) {
    // Do a two pass version, not ideal but easier
    x_prev = STRING_ELT(x_srt, 0);
    for(R_xlen_t i = 1; i < x_len; ++i) {
      SEXP x_cur;
      x_cur = STRING_ELT(x_srt, i);
      FANSI_check_chrsxp(x_cur, i);
      if(x_prev != x_cur) {
        ++u_count;
        x_prev = x_cur;
    } }
    res = PROTECT(allocVector(STRSXP, u_count));
    SET_STRING_ELT(res, 0, STRING_ELT(x_srt, 0));

    x_prev = STRING_ELT(x_srt, 0);
    u_count = 1;
    for(R_xlen_t i = 1; i < x_len; ++i) {
      SEXP x_cur = STRING_ELT(x_srt, i);
      if(x_prev != x_cur) {
        SET_STRING_ELT(res, u_count++, x_cur);
        x_prev = x_cur;
    } }
  } else {
    res = PROTECT(x);
  }
  UNPROTECT(2);
  return res;
}

