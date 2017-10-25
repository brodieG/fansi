/*
Copyright (C) 2017  Brodie Gaslam

This file is part of "fansi - ANSI CSI-aware String Functions"

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Go to <https://www.r-project.org/Licenses/GPL-2> for a copy of the license.
*/

#include "fansi.h"

int FANSI_is_utf8_loc() {
  SEXP sys_getlocale = PROTECT(install("Sys.getlocale"));
  SEXP lc_ctype = PROTECT(mkString("LC_CTYPE"));
  SEXP loc_call = PROTECT(lang2(sys_getlocale, lc_ctype));

  int err_val = 0;
  SEXP eval_tmp = PROTECT(R_tryEval(loc_call, R_BaseEnv, &err_val));
  if(err_val)
    // nocov start
    error("Internal Error: failed getting UTF8 locale; contact maintainer.");
    // nocov end

  if(TYPEOF(eval_tmp) != STRSXP && xlength(eval_tmp) != 1)
    // nocov start
    error("Internal Error: UTF8 locale not a string; contact maintainer.");
    // nocov end

  const char * loc_string = CHAR(asChar(eval_tmp));

  // If eval_tmp produces a non-null terminated string we're screwed here...

  size_t loc_len = strlen(loc_string);

  if(loc_len >= INT_MAX)
    // nocov start
    error(
      "%s%s",
      "Internal Error: UTF8 locale string possibly longer than INT_MAX; ",
      "contact maintainer."
    );
    // nocov end

  int res = loc_len >= 5 &&
    loc_string[loc_len - 1] == '8' &&
    loc_string[loc_len - 2] == '-' &&
    (loc_string[loc_len - 3] == 'F' || loc_string[loc_len - 3] == 'f') &&
    (loc_string[loc_len - 4] == 'T' || loc_string[loc_len - 4] == 't') &&
    (loc_string[loc_len - 5] == 'U' || loc_string[loc_len - 5] == 'u');

  UNPROTECT(4);
  return(res);
}


/*
 * Code copied directly from src/main/util.c@1186, this code is actually not
 * completely compliant, but we're just trying to match R behavior rather than
 * the correct UTF8 decoding.
 *
 * Among other things note that this allows 5-6 byte encodings which are no
 * longer valid.
 */

/* Number of additional bytes */

static const unsigned char utf8_table4[] = {
  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
  2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
  3,3,3,3,3,3,3,3,4,4,4,4,5,5,5,5 };

int FANSI_utf8clen(char c)
{
    /* This allows through 8-bit chars 10xxxxxx, which are invalid */
    if ((c & 0xc0) != 0xc0) return 1;
    return 1 + utf8_table4[c & 0x3f];
}


