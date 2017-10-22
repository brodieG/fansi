/*
Copyright (C) 2017  Brodie Gaslam

This file is part of "fansi - ANSI CSI-aware String Functions"

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

#include <R.h>
#include <Rinternals.h>

/*
 * Used when computing position and size of ANSI tag with FANSI_loc
 */

struct FANSI_csi_pos {
  // Pointer to the first ESC, or NULL, if it is not found
  const char * start;
  // how many characters to the end of the sequnce
  int len;
  // whether the sequnce is complete or not
  int valid;
};

/*
 * Captures the ANSI state at any particular position in a string.  Note this is
 * only designed to capture SGR CSI codes (i.e. those of format "ESC[n;n;n;m")
 * where "n" is a number.  This is a small subset of the possible ANSI escape
 * codes.
 */

struct FANSI_state {
  /* bold, italic, etc, should be interpreted as bit mask where with 2^n we
   * have:
   *
   * - n == 1: bold
   * - n == 2: blur/faint
   * - n == 3: italic
   * - n == 4: underline
   * - n == 5: blink slow
   * - n == 6: blink fast
   * - n == 7: invert
   * - n == 8: conceal
   * - n == 9: crossout
   *
   * Additionally, will be zero if there are no ANSI tags up to this point.
   * Note that n == 0 is not used so that the translation between n values and
   * the ANSI codes that correspond to each style is direct.
   */

  unsigned int style;

  /*
   * A number in 0-9, corrsponds to the ansi codes in the 3[0-9] range, if less
   * than zero or 9 means no color is active.  If 8 (i.e. corresponding to the
   * `38` code), then `color_extra` contains additional color info.
   */

  int color;

  /*
   * Encode the additional information required to render the 38 color code in
   * an array of 4 numbers:
   *
   * - color_extra[0]: whether to use the r;g;b format (2) or the single value
   *   format (5)
   * - color_extra[1]: the "r" value when in r;g;b format, otherwise the single
   *   color value, should be a number in 0-255
   * - color_extra[2]: the "g" value when in r;g;b format, 0-255
   * - color_extra[3]: the "b" value when in r;g;b format, 0-255
   */

  int color_extra[4];

  /*
   * Same as `color` and `color_extra`, except for the ansi SGR codes starting
   * with a 4
   */

  int bg_color;
  int bg_color_extra[4];

  /*
   * Position markers (all zero index), we use int because these numbers
   * need to make it back to R which doesn't have a `size_t` type.
   *
   * - pos_byte: the byte in the string
   * - pos_ansi: actual character position, note that for the time being this is
   *   actually just the byte position until we add UTF-8 handling
   * - pos_raw: the character position after we strip the handled ANSI tags
   */

  int pos_ansi;
  int pos_raw;
  int pos_byte;

  /*
   * The original string the state corresponds to.  This is the pointer to the
   * beginning of the string.
   */
  const char * string;

  /* Internal Flags ------------------------------------------------------------
   *
   * Used to communicate back from sub-processes that sub-parsing failed, the
   * sub-process is supposed to leave the state pointed at the failing character
   * with the byte position updated.  The parent process is then in charge of
   * updating the raw position.
   */
  int fail;
  int last;
};

struct FANSI_csi_pos FANSI_find_csi(const char * x);

SEXP FANSI_has(SEXP x);
SEXP FANSI_strip(SEXP input);
SEXP FANSI_state_at_raw_pos_ext(SEXP text, SEXP pos);
