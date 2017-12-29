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
 *
 * Really should have tried to harmonize this and strip white?
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
/*
 * Strips Extra ASCII Spaces, and optionally ASCII control characters
 *
 * Won't do anything about weird UTF8 spaces, etc.
 *
 * Keeps newlines and tabs.  Rationale for this is we can handle the cursor
 * effects of those two, but no really any of the others (at least not easily,
 * e.g. \r).
 *
 * Allows two spaces after periods, question marks, and exclamation marks.  This
 * is to line up with strwrap behavior.
 */

SEXP FANSI_strip_white(SEXP input, int extra, struct FANSI_buff *buff) {
  if(TYPEOF(input) != STRSXP) error("Input is not a character vector.");

  SEXP res = PROTECT(input);  // dummy PROTECT
  int strip_any = 0;          // Have any elements in the STRSXP been stripped

  R_xlen_t len = XLENGTH(res);
  for(R_xlen_t i = 0; i < len; ++i) {
    const char * string = CHAR(STRING_ELT(res, i));
    const char * string_start = string;
    char * buff_start;

    R_len_t len_j = LENGTH(STRING_ELT(res, i));
    int strip_this, to_strip, punct_prev, punct_prev_prev, space_prev,
        space_start, control_prev, para_start;

    strip_this = to_strip = punct_prev = punct_prev_prev = space_prev =
      space_start = control_prev = 0;

    para_start = 1;

    R_len_t j_last = 0;

    // First space we encounter after non-space, non-control, can be kept,
    // unless after a punct, in which case two can be kept.  Note that we
    // purposefully allow ourselves to read up to the NULL terminator.

    for(R_len_t j = 0; j <= len_j; ++j) {
      int space = string[j] == ' ';
      int control = extra &&
        (
          (
            string[j] >= 1 && string[j] < 32 &&
            string[j] != '\n' && string[j] != '\t'
          ) ||
          string[j] == 127
        );

      int line_end = (string[j] == '\n' || !string[j]);

      int strip =
        (space && space_prev && !punct_prev_prev) ||
        (space && control_prev) ||
        (space && para_start) ||
        (control);

      // Need to keep track if we're in a sequence that starts with a space in
      // case a line ends, as normally we keep one or two spaces, but if we hit
      // the end of the line we don't want to keep them.

      if(space) {
        if(!(space_prev || control_prev)) space_start = 1;
        else if(space && space_prev && punct_prev_prev) space_start = 2;
      } else if(!control && !line_end) space_start = 0;

      // transcribe string if we've hit something that we don't need to strip

      /*
      Rprintf(
        "para_start: %d strip: %d to_strip: %d j: %d spc: %d %d %d ctrl: %d %d, write: %d strip_this: %d char: %c\n",
        para_start, strip, to_strip, j, space, space_prev, space_start, control,
        control_prev,
        (!strip && to_strip) || (!string[j] && strip_this),
        strip_this,
        (string[j] ? string[j] : '~')
      );
      */
      if(
        (!strip && to_strip) || (!string[j] && (strip_this || space_start))
      ) {
        // need to copy entire STRSXP since we haven't done that yet
        if(!strip_any) {
          UNPROTECT(1);  // input is still protected
          res = PROTECT(duplicate(input));
          strip_any = 1;
        }
        // Make sure buffer is big enough
        if(!strip_this) {
          FANSI_size_buff(buff, len_j + 1);
          buff_start = buff->buff;
          strip_this = 1;
        }
        // Copy the portion up to the point we know should be copied, need
        // special treatment when hitting line ends with spaces.

        int copy_bits = j - j_last - to_strip -
          (line_end * space_start * !para_start);

        // Rprintf("Copy bits %d j: %d j_last: %d\n", copy_bits, j, j_last);
        if(copy_bits) {
          memcpy(buff->buff, string_start, copy_bits);
          buff->buff += copy_bits;
        }
        string_start = string + j;
        j_last = j;
        to_strip = 0;
        space_start = 0;
      } else if(strip) to_strip++;

      para_start = string[j] == '\n' || (para_start && strip);
      control_prev = control;
      space_prev = space;
      punct_prev_prev = punct_prev;
      punct_prev = string[j] == '.' || string[j] == '!' || string[j] == '?';
    }
    if(strip_this) {
      /*
      Rprintf(
        "About to write n: %d %p %p\n",
        buff->buff - buff_start, buff->buff,
        buff_start
      );
      */
      *(buff->buff) = 0;

      SEXP chrsxp = PROTECT(
        mkCharLenCE(
          buff_start, buff->buff - buff_start, getCharCE(STRING_ELT(input, i))
      ) );
      SET_STRING_ELT(res, i, chrsxp);
      UNPROTECT(1);
    }
  }
  UNPROTECT(1);
  return res;
}

SEXP FANSI_strip_white_ext(SEXP input, SEXP extra) {
  struct FANSI_buff buff;

  return FANSI_strip_white(input, asInteger(extra), &buff);
}
