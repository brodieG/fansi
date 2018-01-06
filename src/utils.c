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
 * Compute Location and Size of Next CSI ANSI Sequences
 *
 * Only sequences that start in ESC [ are considered.
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
 */

struct FANSI_csi_pos FANSI_find_csi(const char * x) {
  int valid = 0;
  const char * x_track = x;
  const char * x_start = x;

  // Note there is a potentially unncessary call to `strchr` here in the case
  // the ESC is the last thing in a string, but handling it explicitly adds a
  // bit of complexity and it should be rare

  while((x_start = strchr(x_track, 27))) {
    x_track++;
    if(*x_track == '[') {

      // skip [

      ++x_track;

      // Skip all the valid parameters tokens

      while(*x_track >= 0x30 && *x_track <= 0x3F) ++x_track;

      // And all the valid intermediates

      while(*x_track >= 0x20 && *x_track <= 0x2F) ++x_track;

      // Now there should be a single valid ending byte

      if(*x_track) {
        valid = *x_track >= 0x40 && *x_track <= 0x7E;
      }
      break;
  } }

  struct FANSI_csi_pos res;
  if(!x_start) {
    res = (struct FANSI_csi_pos){.start=x_start, .len=0, .valid=0};
  } else {
    if(x_track - x > INT_MAX - 1)
      // nocov start
      error(
        "%s%s",
        "Interal Error: encountered CSI seq that is too long; ",
        "contact maintianer."
      );
      // nocov end

    res = (struct FANSI_csi_pos){
      .start=x_start, .len=(x_track - x_start + 1), .valid=valid
    };
  }
  return res;
}
/*
 * Translates a CHARSXP to a UTF8 char if necessary, otherwise returns
 * the char
 */
const char * FANSI_string_as_utf8(SEXP x, int is_utf8_loc) {
  if(TYPEOF(x) != CHARSXP)
    error("Internal Error: expect CHARSXP."); // nocov

  cetype_t enc_type = getCharCE(x);

  if(enc_type == CE_BYTES)
    error("BYTE encoded strings are not supported.");

  // CE_BYTES is not necessarily of any encoding, don't allow then?

  int translate = !(
    (is_utf8_loc && enc_type == CE_NATIVE) || enc_type == CE_UTF8
  );
  const char * string;
  /*
  Rprintf(
    "About to translate %s (translate? %d)\n",
    type2char(TYPEOF(x)), translate
  );
  */
  if(translate) string = translateCharUTF8(x);
  else string = CHAR(x);
  // Rprintf("done translate\n");

  return string;
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
    int tmp_double_size = FANSI_add_int(buff->len, buff->len);
    if(size > tmp_double_size) tmp_double_size = size;
    buff->len = tmp_double_size;
    // Rprintf("  Alloc to %d\n", buff->len);
    buff->buff = R_alloc(buff->len, sizeof(char));
  }
}
