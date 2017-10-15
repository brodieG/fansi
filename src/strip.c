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

  for(i = 0; i < len; ++i) {
    if(!i % 1000) R_CheckUserInterrupt();
    const char * chr,  * chr_last,  * chr_track;
    chr = chr_last = chr_track = CHAR(STRING_ELT(input, i));

    char * res_track, * res_start;
    int has_ansi;
    size_t i_len = strlen(chr);

    if(i_len > INT_MAX)
      // nocov start
      error("Illegal size string encountered of size %.0f.", (double) i_len);
      // nocov end

    while(*chr_track) {
      if(*chr_track == 27 && *(chr_track + 1) == '[') {
        // valid ansi must end in lower case letter?

        const char * tag_start = chr_track;
        while(* chr_track < 'a' & * chr_track > 'z' & * chr_track) {
          ++chr_track;
        }
        if(chr_track) {
          if(!has_ansi) {
            // Overallocate for simplicity, in reality shouldn't need to allocate
            // for all the ansi tags we're stripping
            res_track = (char *) R_alloc(i_len, sizeof(char));
            has_ansi = any_ansi = 1;
          }
          // Is memcpy going to cause problems again by reading past end of
          // block?

          memcpy(res_track, chr_last, tag_start - chr_last + 1);
          res_track += tag_start - chr_last + 1; // +1?
          chr_last = chr_track + 1;
        }
      }
      ++chr_track;
    }
    if(any_ansi && res_fin == input) {
      REPROTECT(res_fin = allocVector(STRSXP, len), ipx);
    }
    if(has_ansi) {
      SEXP chr_sexp = PROTECT(mkChar(res_start));
      SET_STRING_ELT(res_fin, i, chr_sexp);
      UNPROTECT(1);
    }
  }
  UNPROTECT(1);
  return res_fin;
}
