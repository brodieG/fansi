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


  /*
   * Buffer used for writing strings
   *
   * Kept around so we don't keep having to re-allocate memory if it is
   * sufficiently large to write what we want.
   */
  struct FANSI_buff {
    char * buff; // Buffer
    int len;     // How many bytes the buffer has been allocated to
  };
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
   *
   * Note that the struct fields are ordered by size
   */

  struct FANSI_state {
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
     *
     * See also color, bgcolor
     */

    int color_extra[4];
    int bg_color_extra[4];

    /*
     * The original string the state corresponds to.  This should always be
     * a pointer to the beginning of the string, use the
     * `state.string[state.pos_byte]` to access the current position.
     */
    const char * string;

    /*
     * Tab stops, the last value repeats.  This should be a pre-protected SEXP
     */
    SEXP tab_stops;

    /*
     * should be interpreted as bit mask where with 2^n.
     *
     * - n ==  1: bold
     * - n ==  2: blur/faint
     * - n ==  3: italic
     * - n ==  4: underline
     * - n ==  5: blink slow
     * - n ==  6: blink fast
     * - n ==  7: invert
     * - n ==  8: conceal
     * - n ==  9: crossout
     * - n == 10: fraktur
     * - n == 11: double underline
     */
    unsigned int style;

    /*
     * should be interpreted as bit mask where with 2^n.
     *
     * - n == 1: framed
     * - n == 2: encircled
     * - n == 3: overlined
     * - n == 4: unused (turns off a style)
     * - n == 5: unused (turns off a style)
     * - n == 6: reserved
     * - n == 7: reserved
     * - n == 8: reserved
     * - n == 9: reserved
     */

    unsigned int border;
    /*
     * should be interpreted as bit mask where with 2^n.
     *
     * - n == 0: ideogram underline or right side line
     * - n == 1: ideogram double underline or double line on the right side
     * - n == 2: ideogram overline or left side line
     * - n == 3: ideogram double overline or double line on the left side
     * - n == 4: ideogram stress marking
     */

    unsigned int ideogram;

    /* Alternative fonts, 10-19, where 0 is the primary font */

    int font;

    /*
     * A number in 0-9, corrsponds to the ansi codes in the [3-4][0-9] range, if
     * less than zero or 9 means no color is active.  If 8 (i.e. corresponding
     * to the `38` code), then `color_extra` contains additional color info.
     */

    int color;           // the number follwing the 3 in 3[0-9]
    int bg_color;        // the number follwing the 4 in 4[0-9]

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

    /* Internal Flags ---------------------------------------------------------
     *
     * Used to communicate back from sub-processes that sub-parsing failed, the
     * sub-process is supposed to leave the state pointed at the failing
     * character with the byte position updated.  The parent process is then in
     * charge of updating the raw position.
     */
    /*
     * Type of failure
     *
     * * 0: no error
     * * 1: well formed csi sgr, but contains uninterpretable characters [:<=>]
     * * 2: well formed csi sgr, but contains uninterpretable sub-strings, if a
     *      CSI sequence is not fully parsed yet (i.e. last char not read) it is
     *      assumed to be SGR until we read the final code.
     * * 3: well formed csi, but not an SGR
     * * 4: malformed csi
     * * 5: other escape sequence
     */
    int err_code;
    int last;

    // convert tabs to spaces

    int tabs_as_spaces;
  };
  /*
   * Need to keep track of fallback state, so we need ability to return two
   * states
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

  struct FANSI_csi_pos FANSI_find_esc(const char * x);

  // External funs

  SEXP FANSI_has(SEXP x);
  SEXP FANSI_strip(SEXP input);
  SEXP FANSI_state_at_pos_ext(
    SEXP text, SEXP pos, SEXP type, SEXP lag, SEXP ends
  );
  SEXP FANSI_strwrap_ext(
    SEXP x, SEXP width,
    SEXP indent, SEXP exdent, SEXP prefix, SEXP initial,
    SEXP wrap_always, SEXP pad_end,
    SEXP strip_spc,
    SEXP tabs_as_spc, SEXP tab_stops
  );
  SEXP FANSI_process(SEXP input, struct FANSI_buff * buff);
  SEXP FANSI_process_ext(SEXP input);

  // Internal

  // Utilities

  SEXP FANSI_check_assumptions();
  SEXP FANSI_digits_in_int_ext(SEXP y);

  struct FANSI_state FANSI_parse_esc(struct FANSI_state state);
  void FANSI_size_buff(struct FANSI_buff * buff, int size);
  int FANSI_is_utf8_loc();
  int FANSI_utf8clen(char c);
  int FANSI_digits_in_int(int x);
  const char * FANSI_string_as_utf8(SEXP x, int is_utf8_loc);
  struct FANSI_state FANSI_state_init();
  int FANSI_state_comp(struct FANSI_state target, struct FANSI_state current);
  int FANSI_state_has_style(struct FANSI_state state);
  int FANSI_state_size(struct FANSI_state state);
  int FANSI_csi_write(char * buff, struct FANSI_state state, int buff_len);
  struct FANSI_state FANSI_read_ascii(struct FANSI_state state);
  struct FANSI_state FANSI_read_next(struct FANSI_state state);
  int FANSI_add_int(int x, int y);
  int FANSI_has_utf8(const char * x);
  void FANSI_interrupt(int i);

#endif
