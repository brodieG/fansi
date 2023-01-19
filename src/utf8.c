/*
 * Copyright (C) Brodie Gaslam
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

#include "fansi.h"
// Most of the code here has been move to read.c

/*
 * We need to translate to UTF8 any time that we care about string width as we
 * need to know how many bytes to select to feed through R_nchar.
 *
 * Other than that, we don't actually care about the string encoding, though the
 * implicit assumption is that a) anything 127 and under is ASCII, and
 * additionally that no Control Sequence is going to have anything abovce 127 in
 * it.
 */

/*
 * Confirm encoding is not obviously wrong, and length okay.
 */

void FANSI_check_chrsxp(SEXP x, R_xlen_t i) {
  if(TYPEOF(x) != CHARSXP)
    error("Internal Error: expected CHARSXP.");  // nocov
  cetype_t type = getCharCE(x);
  if(type != CE_NATIVE && type != CE_UTF8) {
    if(type == CE_BYTES)
      error(
        "%s at index %jd. %s.",
        "Byte encoded string encountered", FANSI_ind(i),
        "Byte encoded strings are not supported"
      );
    else
      // this should only happen if somehow a string not converted to UTF8
      // sneaks in.
      error(
        "%s %d encountered at index %jd. %s.",
        "Internal Error: unexpected encoding", type,
        FANSI_ind(i), "Contact maintainer"
      );
  }
  if(LENGTH(x) > FANSI_lim.lim_int.max) {
    error(
      "Strings longer than INT_MAX not supported (length %jd at index %jd).",
      (intmax_t)(LENGTH(x)), FANSI_ind(i)
    );
  }
}

/*
 * Testing interface
 */
SEXP FANSI_check_enc_ext(SEXP x, SEXP i) {
  if(TYPEOF(x) != STRSXP)
    error("Internal Error: expected character input."); // nocov
  FANSI_check_chrsxp(STRING_ELT(x, asInteger(i) - 1), asInteger(i) - 1);
  return ScalarLogical(1);
}
