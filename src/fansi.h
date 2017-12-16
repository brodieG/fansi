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

#ifndef _FANSI_H
#define _FANSI_H

// concept borrowed from utf8-lite

#define FANSI_INTERRUPT(i) (if(!((i) % 1000))) R_CheckUserInterrupt())
#define FANSI_ADD_INT(x, y) (\
  ((int)x) > INT_MAX - ((int)y) ? \
  error(\
    "Integer overflow in %s at line %d in file %s.", __func__, __LINE__,\
    __FILE__\
  ) : \
  (x) + (y))

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
   * Captures the ANSI state at any particular position in a string.  Note this
   * is only designed to capture SGR CSI codes (i.e. those of format
   * "ESC[n;n;n;m") where "n" is a number.  This is a small subset of the
   * possible ANSI escape codes.
   */

  struct FANSI_state {
    /*
     * The original string the state corresponds to.  This is the pointer to the
     * beginning of the string.
     */
    const char * string;

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
     * - pos_ansi: actual character position
     * - pos_raw: the character position after we strip the handled ANSI tags,
     *   the difference with pos_ansi is that pos_ansi counts the escaped
     *   characters whereas this one does not.
     * - pos_width: the character postion accounting for double width
     *   characters, etc., note in this case ASCII escape sequences are treated
     *   as zero chars.  Width is computed with R_nchar.
     * - pos_width_target: pos_width when the requested width cannot be matched
     *   exactly, pos_width is the exact width, and this one is what was
     *   actually requested.  Needed so we can match back to request.
     *
     * Actually not clear if there is a difference b/w pos_raw and pos_ansi,
     * might need to remove one
     */

    int pos_ansi;
    int pos_raw;
    int pos_width;
    int pos_width_target;
    int pos_byte;

    // Are there bytes outside of 0-127

    int has_utf8;

    // Track width of last character

    int last_char_width;

    /* Internal Flags ------------------------------------------------------------
     *
     * Used to communicate back from sub-processes that sub-parsing failed, the
     * sub-process is supposed to leave the state pointed at the failing
     * character with the byte position updated.  The parent process is then in
     * charge of updating the raw position.
     */

    int fail;
    int last;
  };
  /*
   * Need to keep track of fallback state, so we need ability to return two states
   */
  struct FANSI_state_pair {
    struct FANSI_state cur;
    struct FANSI_state prev;
  };
  /*
   * Sometimes need to keep track of a string and the encoding that it is in
   * outside of a CHARSXP
   */
  struct FANSI_string_type {
    const char * string;
    cetype_t type;
  };

  struct FANSI_csi_pos FANSI_find_csi(const char * x);

  SEXP FANSI_has(SEXP x);
  SEXP FANSI_strip(SEXP input);
  SEXP FANSI_state_at_pos_ext(
    SEXP text, SEXP pos, SEXP type, SEXP lag, SEXP ends
  );

  int FANSI_is_utf8_loc();
  int FANSI_utf8clen(char c);

  SEXP FANSI_check_assumptions();

  const char * FANSI_string_as_utf8(const char * x, int is_utf8_loc);

  struct FANSI_state FANSI_state_init();

  int FANSI_state_size(FANSI_state state);

#endif
