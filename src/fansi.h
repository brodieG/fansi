/*
Copyright (C) 2017  Brodie Gaslam

This file is part of "fansi - ANSI Control Sequence Aware String Functions"

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

#include <stdint.h>
#include <R.h>
#include <Rinternals.h>
#include <Rversion.h>


#ifndef _FANSI_H
#define _FANSI_H

  // - Constants / Macros ------------------------------------------------------

  // CAREFUL with these; if we get close to INT_MAX with 2^x we can have
  // problems with signed/unsigned bit shifts.  Shouldn't be anywhere close to
  // that but something to keep in mind

  #define FANSI_CTL_NL 1
  #define FANSI_CTL_C0 2
  #define FANSI_CTL_SGR 4
  #define FANSI_CTL_CSI 8
  #define FANSI_CTL_ESC 16
  #define FANSI_CTL_ALL 31 // 1 + 2 + 4 + 8 + 16 == 2^0 + 2^1 + 2^2 + 2^3 + 2^4

  #define FANSI_STYLE_MAX 12 // 12 is double underline

  #define FANSI_TERM_BRIGHT 1
  #define FANSI_TERM_256 2
  #define FANSI_TERM_TRUECOLOR 4

  // symbols

  extern SEXP FANSI_warn_sym;

  // macros

  #define FANSI_ADD_INT(x, y) FANSI_add_int((x), (y), __FILE__, __LINE__)

  // Global variables (see utils.c)
  // These should probably not be uintmax, this was all done originally when we
  // thought we could feed the struct to one function, but that is not to be.
  // (well TBD).

  struct FANSI_ulimit {
    const char * name;
    uintmax_t min;
    uintmax_t max;
  };
  struct FANSI_slimit {
    const char * name;
    intmax_t min;
    intmax_t max;
  };
  // Update assumption checks if any of this changes
  struct FANSI_limits {
    struct FANSI_slimit lim_int;
    struct FANSI_slimit lim_R_len_t;
    struct FANSI_slimit lim_R_xlen_t;
    struct FANSI_ulimit lim_size_t;
  };
  extern struct FANSI_limits FANSI_lim;

  // - Structs -----------------------------------------------------------------
  /*
   * Buffer used for writing strings
   *
   * Kept around so we don't keep having to re-allocate memory if it is
   * sufficiently large to write what we want.
   */
  struct FANSI_buff {
    char * buff; // Buffer
    size_t len;     // How many bytes the buffer has been allocated to
  };
  struct FANSI_string_as_utf8 {
    const char * string;  // buffer
    size_t len;           // size of buffer
    int translated;       // whether translation was required
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
    // what types of control sequences were found, seel also FANSI_state.ctl
    int ctl;
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
     *
     * Should be no longer than INT_MAX excluding terminating NULL.
     */
    const char * string;
    /*
     * Any error associated with err_code
     */
    const char * err_msg;

    /*
     * should be interpreted as bit mask where with 2^n., 1-9 match to the
     * corresponding ANSI CSI SGR codes, 10 and greater are not necessarily
     * contiguous but were put here because they could co-exist with the style
     *
     * - n ==  0: UNUSED, to simplify logic
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
     * - n == 12: prop spacing
     *
     * UPDATE FANSI_STYLE_MAX if we add more here!!, make sure to check the
     * size, read, and write funs any time this changes.
     *
     * Also, if any HTML styles are added check those too.
     */
    unsigned int style;

    /*
     * should be interpreted as bit mask where with 2^n.
     *
     * - n == 0: UNUSED, to simplify logic
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
     *
     * If > 9 then for color in 90-97, the bright color, for bg_color, in
     * 100-107 the bright bg color.
     */

    int color;           // the number following the 3 in 3[0-9]
    int bg_color;        // the number following the 4 in 4[0-9]

    /*
     * Position markers (all zero index), we use int because these numbers
     * need to make it back to R which doesn't have a `size_t` type.
     *
     * - pos_byte: the byte in the string
     * - pos_ansi: actual character position, different from pos_byte due to
     *   multi-byte characters (i.e. UTF-8)
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

    // Track width of last character (this seems to be the display width)

    int last_char_width;

    /* Control Flags -----------------------------------------------------------
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
     * * 1: well formed csi sgr, but contains uninterpretable sub-strings, if a
     *      CSI sequence is not fully parsed yet (i.e. last char not read) it is
     *      assumed to be SGR until we read the final code.
     * * 2: well formed csi sgr, but contains uninterpretable characters [:<=>]
     * * 3: well formed csi sgr, but contains color codes that exceed terminal
     *     capabilities
     * * 4: well formed csi, but not an SGR
     * * 5: malformed csi
     * * 6: other escape sequence
     * * 7: malformed escape
     * * 8: c0 escapes
     * * 9: malformed UTF8
     */
    int err_code;
    /*
     * Terminal capabilities
     *
     * term_cap & 1        // bright colors
     * term_cap & (1 << 1) // 256 colors
     * term_cap & (1 << 2) // true color
     *
     */
    int term_cap;
    // Whether at end of a CSI escape sequence (i.e. found an 'm').
    int last;
    // Whether the last control sequence that was completely read is known to be
    // an SGR sequence.  This is used as part of the `read_esc` process and is
    // really intended to be internal.  It's really only meaningful when
    // `state.last` is true.
    int sgr;
    // Whether to issue warnings if err_code is non-zero, if -1 means that the
    // warning was issued at least once so may not need to be re-issued
    int warn;
    // Whether to use R_nchar, really only needed when we're doing things in
    // width mode
    int use_nchar;
    // Whether the most recently read escape sequence is adjacent a NULL, which
    // allows us to decide not to write it back out as it would be redundant.
    int terminal;

    /*
     * These support the arguments of the same names for nchar
     */
    int allowNA;
    int keepNA;
    // invalid multi-byte char, a bit of duplication with err_code = 9;
    int nchar_err;
    // what types of Control Sequences should have special treatment.  This
    // mirrors the `ctl` parameter for `FANSI_find_esc`.  See `FANSI_ctl_as_int`
    // for the encoding.
    int ctl;
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

  // - External funs -----------------------------------------------------------

  SEXP FANSI_has(SEXP x, SEXP ctl, SEXP warn);
  SEXP FANSI_strip(SEXP x, SEXP ctl, SEXP warn);
  SEXP FANSI_state_at_pos_ext(
    SEXP text, SEXP pos, SEXP type, SEXP lag, SEXP ends,
    SEXP warn, SEXP term_cap, SEXP ctl
  );
  SEXP FANSI_strwrap_ext(
    SEXP x, SEXP width,
    SEXP indent, SEXP exdent, SEXP prefix, SEXP initial,
    SEXP wrap_always, SEXP pad_end,
    SEXP strip_spaces,
    SEXP tabs_as_spaces, SEXP tab_stops,
    SEXP warn, SEXP term_cap,
    SEXP first_only, SEXP ctl, SEXP norm
  );
  SEXP FANSI_process(SEXP input, struct FANSI_buff * buff);
  SEXP FANSI_process_ext(SEXP input);
  SEXP FANSI_tabs_as_spaces_ext(
    SEXP vec, SEXP tab_stops, SEXP warn, SEXP term_cap, SEXP ctl
  );
  SEXP FANSI_color_to_html_ext(SEXP x);
  SEXP FANSI_esc_to_html(SEXP x, SEXP warn, SEXP term_cap, SEXP class_pre);
  SEXP FANSI_unhandled_esc(SEXP x, SEXP term_cap);

  SEXP FANSI_nchar(
    SEXP x, SEXP type, SEXP allowNA, SEXP keepNA, SEXP warn, SEXP term_cap
  );
  SEXP FANSI_nzchar(SEXP x, SEXP keepNA, SEXP warn, SEXP term_cap, SEXP ctl);
  SEXP FANSI_strsplit(SEXP x, SEXP warn, SEXP term_cap);
  SEXP FANSI_tabs_as_spaces(
    SEXP vec, SEXP tab_stops, struct FANSI_buff * buff, SEXP warn,
    SEXP term_cap, SEXP ctl
  );
  // utility

  SEXP FANSI_cleave(SEXP x);
  SEXP FANSI_order(SEXP x);
  SEXP FANSI_sort_int(SEXP x);
  SEXP FANSI_sort_chr(SEXP x);

  SEXP FANSI_check_assumptions();
  SEXP FANSI_digits_in_int_ext(SEXP y);
  SEXP FANSI_unique_chr(SEXP x);

  SEXP FANSI_add_int_ext(SEXP x, SEXP y);

  SEXP FANSI_set_int_max(SEXP x);
  SEXP FANSI_get_int_max();
  SEXP FANSI_esc_html(SEXP x);

  // - Internal funs -----------------------------------------------------------

  struct FANSI_csi_pos FANSI_find_esc(const char * x, int ctl);
  struct FANSI_state FANSI_inc_width(struct FANSI_state state, int inc);
  struct FANSI_state FANSI_reset_pos(struct FANSI_state state);
  struct FANSI_state FANSI_reset_width(struct FANSI_state state);

  void FANSI_check_chrsxp(SEXP x, R_xlen_t i);
  SEXP FANSI_check_enc_ext(SEXP x, SEXP i);

  int FANSI_ctl_as_int(SEXP ctl);
  SEXP FANSI_ctl_as_int_ext(SEXP ctl);

  void FANSI_size_buff(struct FANSI_buff * buff, size_t size);

  int FANSI_pmatch(
    SEXP x, const char ** choices, int choice_count, const char * arg_name
  );

  int FANSI_is_utf8_loc();
  int FANSI_utf8clen(char c);
  int FANSI_digits_in_int(int x);
  struct FANSI_string_as_utf8 FANSI_string_as_utf8(SEXP x);
  struct FANSI_state FANSI_state_init(
    SEXP strsxp, SEXP warn, SEXP term_cap, R_xlen_t i
  );
  struct FANSI_state FANSI_state_init_full(
    SEXP strsxp, SEXP warn, SEXP term_cap, SEXP allowNA, SEXP keepNA,
    SEXP width, SEXP ctl, R_xlen_t i
  );
  int FANSI_state_comp(struct FANSI_state target, struct FANSI_state current);
  int FANSI_state_comp_color(
    struct FANSI_state target, struct FANSI_state current
  );
  int FANSI_state_comp_basic(
    struct FANSI_state target, struct FANSI_state current
  );
  int FANSI_state_has_style(struct FANSI_state state);
  struct FANSI_state FANSI_state_copy_style(
    struct FANSI_state target, struct FANSI_state current
  );
  int FANSI_state_size(struct FANSI_state state);
  int FANSI_csi_write(char * buff, struct FANSI_state state, int buff_len);

  struct FANSI_state FANSI_read_next(struct FANSI_state state);

  int FANSI_add_int(int x, int y, const char * file, int line);

  // Utilities

  int FANSI_has_utf8(const char * x);
  void FANSI_interrupt(R_xlen_t i);
  intmax_t FANSI_ind(R_xlen_t i);
  void FANSI_check_chr_size(char * start, char * end, R_xlen_t i);
  SEXP FANSI_mkChar(
    const char * start, const char * end, cetype_t enc, R_xlen_t i
  );
  SEXP FANSI_reset_limits();
  void FANSI_check_limits();

  int FANSI_check_append(int cur, int extra, const char * msg, R_xlen_t i);
  void FANSI_check_append_err(const char * msg, R_xlen_t i);

  int FANSI_copy_or_measure(
    char ** buff, const char * tmp, int len, R_xlen_t i,
    const char * err_msg
  );
  #define COPY_OR_MEASURE(A, B) FANSI_copy_or_measure((A), (B), len, i, err_msg)

  // - Compatibility -----------------------------------------------------------

  // R_nchar does not exist prior to 3.2.2, so we sub in this dummy

  #if defined(R_VERSION) && R_VERSION >= R_Version(3, 2, 2)
  #else
  typedef enum {Bytes, Chars, Width} nchar_type;
  int R_nchar(SEXP string, nchar_type type_,
              Rboolean allowNA, Rboolean keepNA, const char* msg_name);
  #endif

#endif
