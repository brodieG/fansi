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
  int * which_ansi;            // track which elements contain ansi
  R_len_t mem_req = 0;         // how much memory we need for each ansi
  int invalid_ansi = 0;
  R_xlen_t invalid_ansi_idx = 0;

  struct FANSI_csi_pos csi;

  // First pass identify which items have ansi, and how many characters the
  // longest of the lot has

  for(i = 0; i < len; ++i) {
    if(!i % 1000) R_CheckUserInterrupt();
    SEXP input_chr = STRING_ELT(input, i);
    if(input_chr == NA_STRING) continue;
    const char * chr = CHAR(input_chr);
    csi = FANSI_find_csi(chr);
    if(csi.valid) {
      // As soon as we encounter ansi, allocate vector to track what has ansi
      if(!any_ansi) {
        any_ansi = 1;
        which_ansi = (int *) R_alloc(len, sizeof(int));
        for(R_len_t j = 0; j < len; ++j) which_ansi[j] = 0;
      }
      which_ansi[i] = 1;
      R_len_t chr_len = LENGTH(input_chr);
      if(chr_len > mem_req) mem_req = chr_len;
    } else if(csi.start) {
      // Mark vector loc of first invalid ansi (should we warn here)?

      if(!invalid_ansi) {
        invalid_ansi = 1;
        invalid_ansi_idx = i;
        warning("Invalid CSI len: %d at index %.0f", csi.len, (double) i + 1);
      }
    }
  }
  // Now strip

  if(any_ansi) {
    // We need to allocate a result vector since we'll be stripping ANSI CSI

    REPROTECT(res_fin = duplicate(input), ipx);

    // Allocate memory sufficient for largest string.  Overallocate for
    // simplicity, in reality shouldn't need to allocate for all the ansi tags
    // we're stripping

    if(mem_req == R_LEN_T_MAX)
      // nocov start
      error(
        "%s%s",
        "Internal error, string should be shorter than R_LEN_T_MAX, ",
        "contact maintainer."
      );
      // nocov end

    char * chr_buff = (char *) R_alloc(mem_req + 1, sizeof(char));

    for(i = 0; i < len; ++i) {
      if(!i % 1000) R_CheckUserInterrupt();
      if(which_ansi[i]) {
        const char * chr,  * chr_last,  * chr_track;
        SEXP input_chr = STRING_ELT(input, i);
        chr = chr_last = chr_track = CHAR(input_chr);

        char * res_track, * res_start;
        res_start = res_track = chr_buff;

        while((csi = FANSI_find_csi(chr_track)).start) {
          if(csi.valid) {
            // Is memcpy going to cause problems again by reading past end of
            // block?  Didn't in first valgrind check.

            if(csi.len) {
              memcpy(res_track, chr_track, csi.start - chr_track);
              res_track += csi.start - chr_track;
            }
          }
          chr_track = csi.start + csi.len;
        }
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
  } } }
  UNPROTECT(1);
  return res_fin;
}
