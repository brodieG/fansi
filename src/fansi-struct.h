/*
Copyright (C) 2021 Brodie Gaslam

This file is part of "fansi - ANSI Control Sequence Aware String Functions"

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 2 or 3 of the License.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Go to <https://www.r-project.org/Licenses/GPL-2> for a copy of the license.
*/

#ifndef _FANSI_STRUCT_H
#define _FANSI_STRUCT_H

// Global variables (see utils.c)
// These was originally designed hoping we could have a single struct with
// shared logic, but in the end it's like this...  We used to have uintmax_t
// and intmax_t, but removed those for performance concerns.

struct FANSI_limit_int {
  const char * name;
  int min;
  int max;
};
struct FANSI_limit_rlent {
  const char * name;
  R_len_t min;
  R_len_t max;
};
struct FANSI_limit_rxlent {
  const char * name;
  R_xlen_t min;
  R_xlen_t max;
};
struct FANSI_limit_sizet {
  const char * name;
  size_t min;
  size_t max;
};
// Update assumption checks if any of this changes
struct FANSI_limits {
  struct FANSI_limit_int lim_int;
  struct FANSI_limit_rlent lim_R_len_t;
  struct FANSI_limit_rxlent lim_R_xlen_t;
  struct FANSI_limit_sizet lim_size_t;
};
extern struct FANSI_limits FANSI_lim;

// - Structs -------------------------------------------------------------------
/*
 * Buffer used for writing strings
 *
 * Kept around so we don't keep having to re-allocate memory if it is
 * sufficiently large to write what we want.
 */
struct FANSI_buff {
  char * buff0;      // Buffer start
  char * buff;       // Buffer last written
  void * vheap_self; // Pointer to self on the heap protection stack
  void * vheap_prev; // Pointer to what was previously on top of prot stack
  size_t len_alloc;  // Bytes allocated, includes trailing NULL.
  int len;           // Size target
  const char * fun;  // Function that initialized the buffer.
  int warned;        // Whether a warning was issued already.
  int reset;         // Indicate the buffer was reset as required.
};
/*
 * Used when computing position and size of ANSI tag with FANSI_loc
 */
struct FANSI_ctl_pos {
  // Byte offset to first recognized control sequence
  int offset;
  // how many characters to the end of the sequnce
  int len;
  // Warnings encounted, encoded "bitwise" as with FANSI_state.warn
  unsigned int warn_max;
};
struct FANSI_color {
  /*
   * Most significant 4 bytes are the color mode, least significant are the
   * actual colors.
   *
   * Color modes:
   *
   * - 00: off
   * - 01: on in lower bytes: 0-7 nomal, 8-15 bright.
   * - 10: 256 color mode (record value in .a)
   * - 11: truecolor (recored values in .a, .b, .c)
   */
  unsigned char x;
  // Color channels, or at index 0 256 color value
  // **WARNING**: these are not necessarily reset, must check `x` first.
  unsigned char extra[3];
}
/*
 * Encode Active SGR
 */
struct FANSI_sgr {
  struct FANSI_color color;
  struct FANSI_color bgcol;

  /*
   * Should be interpreted as bit mask where with 2^n.  See
   * FANSI_(ST|BRD|IDG)_* constants for details of what each bit means.
   *
   * Make sure this remains consistent with the HTML code
   */
  unsigned int style;
};
// val should always be initialized and never NULL.
struct FANSI_string {
  const char * val;
  int len;
};
/*
 * OSC derived URL info.
 *
 * Failed url parses designated by bytes == 0.
 */
struct FANSI_osc {
  int len;    // bytes of the entire OSC, excluding the initial ESC
  int error;  // error, if any, one of 0, 4 or 5 (see FANSI_state.err_code).
};
struct FANSI_url {
  struct FANSI_string url;
  struct FANSI_string params;  // unparsed param string
  struct FANSI_string id;      // parsed id
  struct FANSI_osc osc;
};
struct FANSI_format {
  struct FANSI_url url;
  struct FANSI_sgr sgr;
}
/*
 * Position markers (all zero index).
 *
 * - pos_byte: the first unread byte in the string:  IMPORTANT, unlike all
 *   the other `pos_` trackers which track how many units have already
 *   been read, this one points to the first UNread byte (need to change
 *   variable name).
 * - pos_ansi: actual character position, different from pos_byte due to
 *   multi-byte characters (i.e. UTF-8).  Originally this was use for feeding to
 *   `base::substr`
 * - pos_raw: the character position after we strip the handled ANSI tags,
 *   the difference with pos_ansi is that pos_ansi counts the escape
 *   sequences whereas this one does not.
 * - pos_width: the character postion accounting for width mode, which could be
 *   either display width (computed with R_nchar), graphemes, character count,
 *   or byte count, all of these excluding CSI and other controls.
 */

struct FANSI_position {
  int x;   // Next byte to read
  int a;   // pos_ansi
  int r;   // pos_raw
  int w;   // pos_width
}
/*
 * Captures the SGR and OSC URL state at any particular position in a string.
 *
 * Possibly more thought should go into padding for alignment.
 *
 * Keep in-sync with e.g. `FANSI_reset_state`.
 */
struct FANSI_state {
  struct FANSI_format   fmt;
  struct FANSI_position pos;

  const char * string;
  const char * arg;     // parameter name for error messages

  // R level settings, see FANSI_SET_*
  unsigned int settings;

  // Status flags, see FANSI_STAT_*
  unsigned int status;

  // Are there bytes outside of 0-127 (i.e. UTF-8 since that is the only way
  // those should show up).  Records the 0-index start byte of the **next**
  // character **after** last-seen UTF-8 character (we don't record the start
  // byte because that could be 0, which is ambiguous; we coud initialize this
  // to -1 so it isn't ambiguous, but that is also fragile).
  int utf8;
};
/*
 * Need to keep track of fallback state, so we need ability to return two
 * states
 */
struct FANSI_state_pair {
  struct FANSI_state cur;
  struct FANSI_state restart;
};
/*
 * Sometimes need to keep track of a string and the encoding that it is in
 * outside of a CHARSXP
 */
struct FANSI_string_type {
  const char * string;
  cetype_t type;
};

#endif  /* _FANSI_STRUCT_H */

