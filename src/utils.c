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
const char * FANSI_string_as_utf8(x) {
  if(typeof(x) != CHARSXP)
    error("Internal Error: expect CHARSXP."); // nocov
  int utf8_loc = FANSI_is_utf8_loc();

  cetype_t enc_type = getCharCE(x);

  // Do we even allow CE_BYTES?  Obviously we want to allow ANSI, and don't need
  // to convert that, but trying to remember if we would use CE_BYTES for bytes
  // that contain > 127 (i.e. not ANSI), which could break stuff.

  int translate = !(
    (utf8_loc && enc_type == CE_NATIVE) || enc_type == CE_BYTES ||
    enc_type == CE_UTF8
  );
  if(translate) string = translateCharUTF8(x);
  else string = CHAR(x);

  return string;
}

inline int safe_add(int a, int b, int line, const char * file) {
  if(a > INT_MAX - b)
    error("Integer overflow in %s at line %d in file %s.");
  return a + b;
}
