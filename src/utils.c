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

  // Note there is a potentially unncessary call to `strchr` here in the case
  // the ESC is the last thing in a string, but handling it explicitly adds a
  // bit of complexity and it should be rare

  while((x_track = strchr(x_track, 27))) {
    x_track++;
    if(*x_track == '[') {
      // skip esc and [

      x_track += 2;

      // Skip all the valid parameters tokens

      while(*x_track >= 0x30 && *x_track <= 0x3F) ++x_track;

      // And all the valid intermediates

      while(*x_track >= 0x20 && *x_track <= 0x2F) ++x_track;

      // Now there should be a single valid ending byte

      if(*x_track) {
        x_track++;
        valid = *x_track >= 0x40 && *x_track <= 0x7E;
  } } }
  struct FANSI_csi_pos res;
  if(!x_track) {
    res = (struct FANSI_csi_pos){.start=x_track, .len=0, .valid=0};
  } else {
    if(x_track - x > INT_MAX - 1)
      // nocov start
      error(
        "%s%s",
        "Interal Error: encountered CSI seq that is too long; ",
        "contact maintianer."
      );
      // nocov end

    res =
      (struct FANSI_csi_pos){.start=x_track, .len=(x_track - x + 1), .valid=0};
  }
  return res;
}
