/*
Copyright (C) 2017  Brodie Gaslam

This file is part of "fansi - ANSI-aware String Functions"

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
/*
 * Strips ANSI tags from input
 *
 * Assumes input is NULL terminated.
 *
 * Freaks out if ANSI escape sequence contains stuff outside of 1-127.  Observed
 * behavior is that that stuff gets spat out to screen but ANSI tag continues to
 * be processed...  Probably undefined behavior?
 */

SEXP FANSI_strip(SEXP input) {
  if(TYPEOF(input) != STRSXP) error("`input` should be STRSXP");

  R_xlen_t i, len = xlength(input);
  PROTECT_INDEX ipx;
  PROTECT_WITH_INDEX(input, &ipx);  // reserve spot if we need to alloc later
  SEXP res_fin = input;

  int any_ansi = 0;
  R_len_t mem_req = 0;         // how much memory we need for each ansi
  int invalid_ansi = 0;
  R_xlen_t invalid_ansi_idx = 0;

  struct FANSI_csi_pos csi;

  // Compute longest char element, we'll assume this is the required size of our
  // string buffer.  This is potentially wastful if there is one very large
  // CSI-less string and all the CSI strings are short.  The alternative would
  // be to keep reallocating to a larger size, maybe by 2x, but then that
  // requires the growing buffer, etc.  For now go simple

  for(i = 0; i < len; ++i) {
    if(!i % 10000) R_CheckUserInterrupt();
    R_len_t chr_len = LENGTH(STRING_ELT(input, i));
    if(chr_len > mem_req) mem_req = chr_len;
  }
  // Now strip

  for(i = 0; i < len; ++i) {
    if(!i % 1000) R_CheckUserInterrupt();
    SEXP input_chr = STRING_ELT(input, i);
    if(input_chr == NA_STRING) continue;

    int has_ansi = 0;
    const char * chr = CHAR(input_chr);
    const char * chr_track = chr;
    char * chr_buff;
    char * res_track, * res_start;

    while((csi = FANSI_find_csi(chr_track)).start) {
      if(csi.valid) {
        // As soon as we encounter ansi, allocate vector to track what has ansi
        if(!any_ansi) {
          any_ansi = 1;

          // We need to allocate a result vector since we'll be stripping ANSI
          // CSI, and also the buffer we'll use to re-write the CSI less strings

          REPROTECT(res_fin = duplicate(input), ipx);

          // Note the is guaranteed to be an over-allocation

          if(mem_req == R_LEN_T_MAX)
            // nocov start
            error(
              "%s%s",
              "Internal error, string should be shorter than R_LEN_T_MAX, ",
              "contact maintainer."
            );
            // nocov end

          chr_buff = (char *) R_alloc(mem_req + 1, sizeof(char));
        }
        if(!has_ansi) {
          has_ansi = 1;
          res_start = res_track = chr_buff;
        }
        // Is memcpy going to cause problems again by reading past end of
        // block?  Didn't in first valgrind check.

        if(csi.len) {
          memcpy(res_track, chr_track, csi.start - chr_track);
          res_track += csi.start - chr_track;
        }
      } else if(!invalid_ansi) {
        invalid_ansi = 1;
        invalid_ansi_idx = i;
        warning("Invalid CSI len: %d at index %.0f", csi.len, (double) i + 1);
      }
      chr_track = csi.start + csi.len;
    }
    // Update string

    if(has_ansi) {
      // Copy final chunk if it exists because above we only memcpy when we
      // encounter the tag

      if(*chr_track) {
        const char * chr_end = chr + LENGTH(input_chr);
        if(!chr_end) {
          // nocov start
          error(
            "%s%s",
            "Internal Error: failed to find str end, ",
            "contact maintainer."
          );
          // nocov end
        } else if(chr_end > chr_track) {
          memcpy(res_track, chr_track, chr_end - chr_track);
          res_track += chr_end - chr_track;
      } }
      *res_track = '\0';
      SEXP chr_sexp = PROTECT(
        mkCharLenCE(
          res_start, res_track - res_start, getCharCE(input_chr)
      ) );
      SET_STRING_ELT(res_fin, i, chr_sexp);
      UNPROTECT(1);
    }
  }
  UNPROTECT(1);
  return res_fin;
}
