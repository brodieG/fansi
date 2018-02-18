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

  SEXP zero_vec = PROTECT(allocVector(INTSXP, 0));
  SEP res = res_start = PROTECT(list1(R_NilValue));

  for(R_xlen_t i = 0; i < x_len; ++i) {
    SEXP chrsxp = STRING_ELT(x, i);
    if(chrsxp != NA_STRING && LENGTH(chrsxp)) {
      struct FANSI_string_as_utf8 string_dat = FANSI_string_as_utf8(chrsxp);
      const char * chr = string_dat.string;


      if(string_dat.translated) {
        warning(
          "String at index [%.0f] was translated to UTF-8.", (double) (i + 1);
        );
      }
      SETCDR(char_list, list1(res_sxp));
      char_list = CDR(char_list);
      UNPROTECT(1);


    }


  }



}
