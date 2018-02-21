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

/*
 * Compute Location and Size of Next ANSI Sequences
 *
 * See FANSI_parse_esc as well, where there is similar logic, although we keep
 * it separated here for speed since we don't try to interpret the string.
 *
 * Length includes the ESC and [, and start point is the ESC.
 *
 * Validity here means striclty that all the contained escape sequences were
 * valid CSI sequences as per the strict definition.
 *
 * We report the length of invalid sequnces, but you really can't trust them.
 * The true length may actually be different depending on your terminal,
 * (e.g. OSX terminal spits out illegal characters to screen but keeps
 * processing the sequence).
 *
 * @param what is a bit flag to line up against VALID.WHAT index values
 */

struct FANSI_csi_pos FANSI_find_esc(const char * x, int what) {
  /***************************************************\
  | IMPORTANT: KEEP THIS ALIGNED WITH FANSI_parse_esc |
  \***************************************************/
  int valid = 0;
  int strip = 0;
  const char * x_track = x;
  const char * x_strip_end = x;
  const char * x_strip_start = x;

  struct FANSI_csi_pos res;
  while(*x_track) {
    const char x_val = *(x_track++);
    // use strip & strip_this in conjunction so that we can allow multiple
    // strippable adjacent elements to be stripped in one go

    int strip_this = 0;

    // If not normal ASCII or UTF8, examine whether we need to strip
    if(!(x_val > 31 && x_val < 127 || x_val < 0)) {
      strip_this = 1;
      if(x_val == 27) {
        // If not stripping CSI or SGR, skip (12 = 2^2 + 2^3), where ^2
        // corresponds to SGR and ^3 CSI

        if(x_val == '[' && (what & 12)) {
          // This is a CSI sequence, so it has multiple characters that we need to
          // skip.  The final character is processed outside of here since it has
          // the same logic for CSI and non CSI sequences

          // skip [

          ++x_track;

          // Skip all the valid parameters tokens

          while(*x_track >= 0x30 && *x_track <= 0x3F) ++x_track;

          // And all the valid intermediates

          while(*x_track >= 0x20 && *x_track <= 0x2F) ++x_track;
        } else if (what & 1 << 4) {
        } else strip_this = 0;

        // In either normal or CSI sequence, there needs to be a final character:

        valid = *x_track >= 0x40 && *x_track <= 0x7E;
        x_strip = x_track;
        if(*x_track) ++x_track;
        if(*x_track == 27) continue;
      } else if (x_val == '\n' && (what & 1)) {
        // Strip newlines
        strip = 1;
        if(*x_track == '\n') continue;
      } else if (x_val > 0 && (what & 2)) {
        // Strip all other C0,
        strip = 1;
        if(*x_track > 0 && *x_track < 31 '\n') continue;
      }
      if(!strip && strip_this) strip = 1;
    }
    if(strip && !strip_this) break;
  }

  if((x_start = x_track = strchr(x, 27))) {
    while(*x_track == 27) {
    res = (struct FANSI_csi_pos){
      .start=x_start, .len=(x_track - x_start), .valid=valid
    };
  } else {
    res = (struct FANSI_csi_pos){.start=x_start, .len=0, .valid=0};
  }
  return res;
}
/*
 * Allocates a fresh chunk of memory if the existing one is not large enough.
 *
 * We never intend to re-use what's already in memory so we don't realloc.  If
 * allocation is needed the buffer will be either twice as large as it was
 * before, or size `size` if that is greater than twice the size.
 */
void FANSI_size_buff(struct FANSI_buff * buff, int size) {
  // Rprintf("  buff_len %d size %d\n", buff->len, size);
  if(size > buff->len) {
    if(size < 128) size = 128;  // in theory little penalty to ask this minimum
    int tmp_double_size = FANSI_add_int(buff->len, buff->len);
    if(size > tmp_double_size) tmp_double_size = size;
    buff->len = tmp_double_size;
    // Rprintf("  Alloc to %d\n", buff->len);
    buff->buff = R_alloc(buff->len, sizeof(char));
  }
}
/*
 * Compute how many digits are in a number
 *
 * Add an extra character for negative integers.
 */

int FANSI_digits_in_int(int x) {
  int num = 1;
  if(x < 0) {
    ++num;
    x = -x;
  }
  while((x = (x / 10))) ++num;
  return num;
}
SEXP FANSI_digits_in_int_ext(SEXP y) {
  if(TYPEOF(y) != INTSXP) error("Internal Error: required int.");

  R_xlen_t ylen = XLENGTH(y);
  SEXP res = PROTECT(allocVector(INTSXP, ylen));

  for(R_xlen_t i = 0; i < ylen; ++i)
    INTEGER(res)[i] = FANSI_digits_in_int(INTEGER(y)[i]);

  UNPROTECT(1);
  return(res);
}
/*
 * Add integers while checking for overflow
 *
 * Note we are stricter than necessary when y is negative because we want to
 * count hitting INT_MIN as an overflow so that we can use the integer values
 * in R where INT_MIN is NA.
 */
int FANSI_add_int(int x, int y) {
  if((y >= 0 && (x > INT_MAX - y)) || (y < 0 && (x <= INT_MIN - y)))
    error ("Integer overflow");
  return x + y;
}

// concept borrowed from utf8-lite

void FANSI_interrupt(int i) {if(!(i % 1000)) R_CheckUserInterrupt();}
