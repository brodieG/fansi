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

int FANSI_utf8clen(char c) {
  /* This allows through 8-bit chars 10xxxxxx, which are invalid */
  if ((c & 0xc0) != 0xc0) return 1;
  return 1 + utf8_table4[c & 0x3f];
}
/*
 * Basic validation, checks there is a zero in the right spot
 * for the first byte, and that continuation bytes start with 10.
 *
 * Assumes correct number of continuation bytes exist and that
 * input was read through FANSI_utf8clen.
 *
 * DO NOT USE AS STANDALONE UTF8 VALIDATION.
 */
int FANSI_valid_utf8(const char * chr, int bytes) {
  int pass = !(*chr & (0x20 >> (bytes - 2)));
  switch(bytes) {
    case 4: pass &= (*(++chr) & 0xc0) == 0x80;
    case 3: pass &= (*(++chr) & 0xc0) == 0x80;
    case 2: pass &= (*(++chr) & 0xc0) == 0x80; break;
    default: pass = 0;
  }
  return pass;
}

// Compute a unicode code point from a _valid_ UTF8 encoding
// Assumes 0 < bytes < 7 (or else bad stuff will happen)

int FANSI_utf8_to_cp(const char * chr, int bytes) {
  int cp = 0;
  // Lead byte (trailing bytes only contribute 6 bits each)
  cp |=
    (*chr & (0xff >> (bytes + (bytes > 1)))) // keep  7, 5, 4, or 3 bits
    << (bytes - 1) * 6;                      // shift by byte count

  // Trailing bytes keep trailing 6 bits
  for(int i = bytes - 1; i > 0; --i)
    cp |= (*(chr + bytes - i) & 0x3f) << (i - 1) * 6;

  return cp;
}

SEXP FANSI_utf8_to_cp_ext(SEXP x) {
  if(TYPEOF(x) != STRSXP)
    error("Argument `x` must be a character vector.");

  R_xlen_t len = XLENGTH(x);
  SEXP res = PROTECT(allocVector(INTSXP, len));
  int * resi = INTEGER(res);

  for(R_xlen_t i = 0; i < len; ++i) {
    const char * chr = CHAR(STRING_ELT(x, i));
    int len = FANSI_utf8clen(*chr);
    if(LENGTH(STRING_ELT(x, i)) != len)
      error("`x` may only contain single valid UTF-8 encoded code point.");
    resi[i] = FANSI_utf8_to_cp(chr, len);
  }
  UNPROTECT(1);
  return res;
}

