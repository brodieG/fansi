/*
 * Copyright (C) 2020  Brodie Gaslam
 *
 * This file is part of "fansi - ANSI Control Sequence Aware String Functions"
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
// Which styles actuall produce HTML

static const unsigned int css_html_style[8] = {
  1, 2, 3, 4, 5, 6,
  // 7,                // Inverse doesn't actually produces a style
  8, 9
};
static unsigned int css_html_mask = 0;
/*
 * Looks like we don't need to worry about C0 sequences, however we must
 * parse all ESC sequences as HTML gladly displays everything right after the
 * initial ESC.
 *
 * In terms of sequences we consider meaningful, only colors and basic styles
 * for html translation
 */
struct FANSI_css {const char * css; int len;};

// .len leftover from when we used pre-computed widths

static const struct FANSI_css css_style[9] = {
  // Code 1: bold
  {.css="font-weight: bold", .len=-1},
  // Code 2: lighter
  {.css="font-weight: 100", .len=-1},
  // Code 3: italic
  {.css="font-style: italic", .len=-1},
  // Code 4: underline
  {.css="text-decoration: underline", .len=-1},
  // Code 5: blink
  {.css="text-decoration: blink", .len=-1},
  // Code 6: blink
  {.css="text-decoration: blink", .len=-1},
  // Code 7: invert; unused, but needs to be here for offset lookups to work;
  {.css="", .len=0},
  // Code 8: conceal
  {.css="color: transparent", .len=-1},
  // Code 9: line-through
  {.css="text-decoration: line-through", .len=-1},
};
// Generate mask for html styles in first pass

static unsigned int style_html_mask() {
  if(!css_html_mask) {
    int style_n = sizeof(css_html_style) / sizeof(unsigned int);
    for(int i = 0; i < style_n; ++i)
      css_html_mask |= 1U << css_html_style[i];
  }
  return css_html_mask;
}
static int state_has_color(struct FANSI_state state) {
  return state.color >= 0 || state.bg_color >= 0;
}
static int state_has_style_html(struct FANSI_state state) {
  // generate mask first time around.
  return (state.style & style_html_mask()) ||
    state.color >= 0 || state.bg_color >= 0;
}
static int state_comp_html(
  struct FANSI_state target, struct FANSI_state current
) {
  return
    // Colors are Different
    FANSI_state_comp_color(target, current) ||
    // HTML rendered styles are different
    (
      (target.style & style_html_mask()) !=
      (current.style & style_html_mask())
    ) ||
    // If one has color both have the same color, but we need to check
    // whether they have different invert status
    (
      (state_has_color(target)) &&
      (target.style & (1U << 7)) ^ (current.style & (1U << 7))
    );
}

/*
 * Converts basic, bright and 8 bit colors to a range of 0:255.
 * Returns -1 for other values including no color.
 *
 * For use with color classes.
 *
 * Recall colors in 30:39 and 40:49 already converted to 0:9.
 */
static int color_to_8bit(int color, int* color_extra) {
  int col256 = -1;
  if (color >= 0 && color <= 7) {
    // Basic colors
    col256 = color % 10;
  } else if ((color >= 100 && color <= 107) || (color >= 90 && color <= 97)) {
    // Brights
    col256 = color % 10 + 8;
  } else if ((color == 8) && color_extra[0] == 5) {
    // 8 Bit colors
    col256 = color_extra[1];
    if(col256 < 0 || col256 > 255)
      error("Internal Error: 0-255 color outside of that range."); // nocov
  }
  return col256;
}
/*
 * Given CSI SGR Color Codes and User Provided color classes,
 * return the corresponding color class if the color can be mapped to one of the
 * 8 bit color codes in 0:255, or NULL if not.
 *
 * CAUTION: test result before dereferencing: could be a pointer to NULL.
 *
 * @param whether to return foreground or background styles
 */

static const char * get_color_class(
  int color, int* color_extra, SEXP color_classes, int bg
) {
  int col8bit = color_to_8bit(color, color_extra);
  if(col8bit >= 0 && XLENGTH(color_classes) / 2 > (R_xlen_t) col8bit)
    return CHAR(STRING_ELT(color_classes, col8bit * 2 + bg));
  else return NULL;
}
/*
 * All color conversions taken from
 *
 * <https://en.wikipedia.org/wiki/ANSI_escape_code>
 *
 * @param color an integer expected to be in 0:9, 90:97, 100:107. NB: ranges
 *   30:39 and 40:49 already converted to 0:9.
 * @param color_extra a pointer to a 4 long integer array as you would get in
 *   struct FANSI_state.color_extra
 * @param buff a buffer with at least 8 bytes allocated.
 * @return the *buff pointer
 */

static char * color_to_html(int color, int * color_extra, char * buff) {
  // CAREFUL: DON'T WRITE MORE THAN 7 BYTES + NULL TERMINATOR

  const char * dectohex = "0123456789ABCDEF";
  const char * std_16[16] = {
    "000000", "800000", "008000", "808000",
    "000080", "800080", "008080", "C0C0C0",
    "808080", "FF0000", "00FF00", "FFFF00",
    "0000FF", "FF00FF", "00FFFF", "FFFFFF"
  };
  // According to <https://en.wikipedia.org/wiki/ANSI_escape_code> these are the
  // putty defaults

  const char * std_8[8] = {
    "000000", "BB0000", "00BB00", "BBBB00",
    "0000BB", "BB00BB", "00BBBB", "BBBBBB"
  };
  const char * std_5[6] = {"00", "5F", "87", "AF", "D7", "FF"};
  const char * bright[8] = {
    "555555", "FF5555", "55FF55", "FFFF55",
    "5555FF", "FF55FF", "55FFFF", "FFFFFF"
  };
  char * buff_track = buff;

  if(color == 9) {
    error(
      "Internal Error: should not be applying no-color; contact maintainer."
    );
  } else if(color >= 0 && color < 10) {
    *(buff_track++) = '#';

    if(color == 8) {
      if(color_extra[0] == 2) {
        for(int i = 1; i < 4; ++i) {
          char hi = dectohex[color_extra[i] / 16];
          char lo = dectohex[color_extra[i] % 16];
          *(buff_track++) = hi;
          *(buff_track++) = lo;
        }
      } else if(color_extra[0] == 5) {
        // These are the 0-255 color codes
        int color_5 = color_extra[1];
        if(color_5 < 0 || color_5 > 255)
          error("Internal Error: 0-255 color outside of that range."); // nocov
        if(color_5 < 16) {
          // Standard colors
          memcpy(buff_track, std_16[color_5], 6);
          buff_track += 6;
        } else if (color_5 < 232) {
          int c5 = color_5 - 16;
          int c5_r = c5 / 36;
          int c5_g = (c5 % 36) / 6;
          int c5_b = c5 % 6;

          if(c5_r > 5 || c5_g > 5 || c5_b > 5)
            error("Internal Error: out of bounds computing 6^3 clr."); // nocov

          memcpy(buff_track, std_5[c5_r], 2);
          buff_track += 2;
          memcpy(buff_track, std_5[c5_g], 2);
          buff_track += 2;
          memcpy(buff_track, std_5[c5_b], 2);
          buff_track += 2;
        } else {
          int c_bw = (color_5 - 232) * 10 + 8;
          char hi = dectohex[c_bw / 16];
          char lo = dectohex[c_bw % 16];
          for(int i = 0; i < 3; ++i) {
            *(buff_track++) = hi;
            *(buff_track++) = lo;
          }
        }
      } else
        // nocov start
        error(
          "Internal Error: invalid 256 or tru color code; contact maintainer."
        );
        // nocov end
    } else {
      memcpy(buff_track, std_8[color], 6);
      buff_track += 6;
    }
  } else if(
    (color >= 90 && color <= 97) ||
    (color >= 100 && color <= 107)
  ) {
    *(buff_track++) = '#';
    int c_bright = color >= 100 ? color - 100 : color - 90;
    memcpy(buff_track, bright[c_bright], 6);
    buff_track += 6;
  } else {
    error("Internal Error: invalid color code %d", color); // nocov
  }
  *buff_track = 0;
  int dist = (int) (buff_track - buff);
  if(dist != 7) error("Internal Error: unexpected byte count for color.");

  return buff;
}
// Central error function.

static void overflow_err(const char * type, R_xlen_t i) {
  intmax_t ind = i >= INTMAX_MAX ? -2 : i; // i == INTMAX_MAX is the issue
  error(
    "%s %s %s %jd%s",
    "Expanding SGR sequences into HTML will create a string longer than",
    type, "at position", ind + 1, ". Try again with smaller strings."
  );
}
static void overflow_err2(R_xlen_t i) {
  intmax_t ind = i >= INTMAX_MAX ? -2 : i; // i == INTMAX_MAX is the issue
  error(
    "%s %s %s %jd%s",
    "Escaping HTML special characters will create a string longer than",
    "INT_MAX", "at position", ind + 1, ". Try again with smaller strings."
  );
}
/*
 * If *buff is not NULL, copy tmp into it and advance, else measure tmp
 * and advance length
 *
 *   vvvvvvvv
 * !> DANGER <!
 *   ^^^^^^^^
 *
 * This advances *buff so that it points to to the NULL terminator
 * the end of what is written to so string is ready to append to.
 *
 * @param i index in overal character vector, needed to report overflow string.
 */
static unsigned int copy_or_measure(
  char ** buff, const char * tmp, unsigned int len, R_xlen_t i
) {
  size_t tmp_len = strlen(tmp);
  // strictly it's possible for len > FANSI_int_max, but shouldn't happen even
  // in testing since we only grow len by first checking.
  if(tmp_len > FANSI_int_max - len) overflow_err("INT_MAX", i);
  if(*buff) {
    strcpy(*buff, tmp);
    *buff += tmp_len;
    **buff = 0;  // not necessary, but helps to debug
  }
  return tmp_len;
}
/*
 * Compute HTML Size of Each Individual State, Or Write It
 *
 * This used to be two distinct functions, but the risk of getting out of sync
 * was too high so we merged them into one.  We try to minimize the
 * differences between size calculation vs. writing modes to avoid mistakes, but
 * this means the code is less efficient than could be, possibly repeating
 * calcualtions, overflow checks, or computing compile time knowable string
 * widths.
 *
 * @param buff the buffer to write to, if it is null only computes size instead
 *   also of writing.
 */
static int state_size_and_write_as_html(
  struct FANSI_state state,
  struct FANSI_state state_prev,
  char * buff,
  SEXP color_classes, R_xlen_t i,
  int bytes_html
) {
  /****************************************************\
  | IMPORTANT: KEEP THIS ALIGNED WITH FANSI_csi_write  |
  | although right now ignoring rare escapes in html   |
  \****************************************************/

  // Not all basic styles are html styles (e.g. invert), so state only changes
  // on invert when current or previous also has a color style

  int has_cur_state = state_has_style_html(state);
  int has_prev_state = state_has_style_html(state_prev);
  int state_change = state_comp_html(state, state_prev);

  const char * buff_start = buff;
  unsigned int len = bytes_html;  // this is for overflow check

  if(state_change) {
    if(!has_cur_state) {
      len += copy_or_measure(&buff, "</span>", len, i);
    } else {
      if (!has_prev_state) {
        len += copy_or_measure(&buff, "<span", len, i);
      } else {
        len += copy_or_measure(&buff, "</span><span", len, i);
      }
      // Styles
      int invert = state.style & (1 << 7);
      int color = invert ? state.bg_color : state.color;
      int * color_extra = invert ? state.bg_color_extra : state.color_extra;
      int bg_color = invert ? state.color : state.bg_color;
      int * bg_color_extra = invert ? state.color_extra : state.bg_color_extra;

      // Use provided classes instead of inline styles?
      const char * color_class =
        get_color_class(color, color_extra, color_classes, 0);
      const char * bgcol_class =
        get_color_class(bg_color, bg_color_extra, color_classes, 1);

      // Class based colors e.g. " class='fansi-color-06 fansi-bgcolor-04'"
      // Brights remapped to 8-15

      if(color_class || bgcol_class) {
        len += copy_or_measure(&buff, " class='", len, i);
        if(color_class) len += copy_or_measure(&buff, color_class, len, i);
        if(color_class && bgcol_class) len += copy_or_measure(&buff, " ", len, i);
        if(bgcol_class) len += copy_or_measure(&buff, bgcol_class, len, i);
        len += copy_or_measure(&buff, "'", len, i);
      }
      // inline style and/or colors
      if(
        state.style & css_html_mask ||
        (color >= 0 && (!color_class)) ||
        (bg_color >= 0 && (!bgcol_class))
      ) {
        len += copy_or_measure(&buff, " style='", len, i);
        unsigned int len_start = len;
        char color_tmp[8];
        if(color >= 0 && (!color_class)) {
          len += copy_or_measure(&buff, "color: ", len, i);
          len += copy_or_measure(
            &buff, color_to_html(color, color_extra, color_tmp), len, i
          );
        }
        if(bg_color >= 0 && (!bgcol_class)) {
          if(len_start < len) len += copy_or_measure(&buff, "; ", len, i);
          len += copy_or_measure(&buff,  "background-color: ", len, i);
          len += copy_or_measure(
            &buff, color_to_html(bg_color, bg_color_extra, color_tmp), len, i
          );
        }
        // Styles (need to go after color for transparent to work)
        for(int i = 1; i < 10; ++i)
          if(state.style & css_html_mask & (1 << i)) {
            if(len_start < len) len += copy_or_measure(&buff, "; ", len, i);
            len += copy_or_measure(&buff, css_style[i - 1].css, len, i);
          }

        len += copy_or_measure(&buff, "'", len, i);
      }
      len += copy_or_measure(&buff, ">", len, i);
  } }
  len -= bytes_html;
  if(buff) {
    *buff = 0;
    if((unsigned int)(buff - buff_start) != len)
      // nocov start
      error(
        "Internal Error: buffer length mismatch in html generation (%ud vs %ud).",
        len, (unsigned int)(buff - buff_start)
      );
      // nocov end
  }
  // We've checked len at every step, so it cannot overflow INT_MAX.

  return (int)len;
}
/*
 * Final checks for unsual size, and include space for terminator.
 */

static size_t final_string_size(int bytes, R_xlen_t i) {
  // In the extremely unlikely case we're on a systems with weird integer sizes
  // or R changes what R_len_t.  >= SIZE_MAX b/c we need room for the extra NULL
  // terminator byte
  if(INT_MAX >= SIZE_MAX && (unsigned int) bytes >= SIZE_MAX)
    overflow_err("SIZE_MAX", i);     // nocov
  if(INT_MAX > R_LEN_T_MAX && bytes > R_LEN_T_MAX)
    overflow_err("R_LEN_T_MAX", i);  // nocov

  return (size_t) bytes + 1;   // include terminator
}
/*
 * Check for overall overflow, recall that R allows up to R_LEN_T_MAX long
 * strings (which currently is INT_MAX) excluding the NULL.
 *
 * However, need size_t return so we can allocate one extra byte for the NULL
 * terminator.  Strictly speaking we could avoid this as R accepts character
 * buffers of known length via mkCharLenCE or some such, but it feels
 * uncomfortable having an unterminated string floating around.
 *
 * @return the size of the string **including** the NULL terminator
 */
static size_t html_check_overflow(
  int bytes_html, int bytes_esc, int bytes_init, int span_extra, R_xlen_t i
) {
  if(bytes_init < 0 || span_extra < 0)
    error("Internal Error: illegal -ve lengths in overflow check."); // nocov

  // both bytes_html and byte_esc are positive, so this cannot overflow

  int bytes_extra = bytes_html - bytes_esc;
  if(
    (
       bytes_extra >= 0 &&
       bytes_init > FANSI_int_max - bytes_extra - span_extra
    )
    ||
    (
       bytes_extra < 0 &&
       bytes_init + bytes_extra > FANSI_int_max - span_extra
    )
  ) overflow_err("INT_MAX", i);

  if(bytes_init + bytes_extra + span_extra < 0) {
    // nocov start
    error(
      "%s%s",
      "Internal Error: CSS would translate to negative length string; ",
      "this should not happen."
    );
    // nocov end
  }
  int bytes_final = bytes_init + bytes_extra + span_extra;
  return final_string_size(bytes_final, i);
}

SEXP FANSI_esc_to_html(SEXP x, SEXP warn, SEXP term_cap, SEXP color_classes) {
  if(TYPEOF(x) != STRSXP)
    error("Internal Error: `x` must be a character vector");  // nocov
  if(TYPEOF(color_classes) != STRSXP)
    error("Internal Error: `color_classes` must be a character vector");  // nocov

  R_xlen_t x_len = XLENGTH(x);
  struct FANSI_buff buff = {.len=0};
  struct FANSI_state state, state_prev, state_init;
  state = state_prev = state_init = FANSI_state_init("", warn, term_cap);
  const char * span_end = "</span>";
  int span_end_len = (int) strlen(span_end);

  SEXP res = x;
  // Reserve spot on protection stack
  PROTECT_INDEX ipx;
  PROTECT_WITH_INDEX(res, &ipx);

  for(R_xlen_t i = 0; i < x_len; ++i) {
    FANSI_interrupt(i);

    SEXP chrsxp = STRING_ELT(x, i);
    if(chrsxp == NA_STRING) continue;
    FANSI_check_chrsxp(chrsxp, i);
    const char * string = CHAR(chrsxp);

    // Reset position info and string; rest of state info is preserved from
    // prior line so that the state can be continued on new line.
    state = FANSI_reset_pos(state_prev);
    state.string = string;
    struct FANSI_state state_start = FANSI_reset_pos(state);
    state_prev = state_init;  // but there are no styles in the string yet

    int bytes_init = (int) LENGTH(chrsxp);
    int bytes_html = 0;
    int bytes_esc = 0;

    size_t bytes_final = 0;

    // Some ESCs may not produce any HTML, and some strings may gain HTML from
    // an ESC from a prior element even if they have no ESCs.
    int has_esc = 0;
    int has_state = state_has_style_html(state);
    int trail_span = 0;

    // Process the strings in two passes, in pass 1 we compute how many bytes
    // we'll need to store the string, and in the second we actually write it.
    // We trade efficiency for convenience.

    // We cheat by only using FANSI_read_next to read escape sequences as we
    // don't care about display width, etc.  Normally we would _read_next over
    // all characters, not just skip from ESC to ESC.

    // - Pass 1: Measure -------------------------------------------------------

    // Leftover from prior element (only if can't be merged with new)
    if(*string && *string != 0x1b && state_has_style_html(state)) {
      bytes_html += state_size_and_write_as_html(
        state, state_prev,  NULL, color_classes, i, bytes_html
      );
      state_prev = state;
    }
    // New in this element
    while(1) {
      trail_span = state_has_style_html(state_prev);
      string = strchr(string, 0x1b);
      if(!string) string = state.string + bytes_init;
      else {
        has_esc = 1;
        state.pos_byte = (string - state.string);
      }
      // State as html, skip if at end of string
      if(*string) {
        int esc_start = state.pos_byte;
        state = FANSI_read_next(state);
        string = state.string + state.pos_byte;
        bytes_esc += state.pos_byte - esc_start;  // cannot overflow int
        if(*string) {
          bytes_html += state_size_and_write_as_html(
            state, state_prev,  NULL, color_classes, i, bytes_html
          );
        }
        state_prev = state;
        has_state |= state_has_style_html(state);
        if(!*string) break; // nothing after state, so done
      } else break;
    }
    // - Pass 2: Write ---------------------------------------------------------

    if(has_esc || has_state) {
      bytes_final = html_check_overflow(
        bytes_html, bytes_esc, bytes_init,
        span_end_len * trail_span, // Last non-terminal state has style?
        i
      );
      trail_span = 0;
      // Allocate target vector if it hasn't been yet
      if(res == x) REPROTECT(res = duplicate(x), ipx);

      // Allocate buffer and do second pass, bytes_final includes space for NULL
      FANSI_size_buff(&buff, bytes_final);
      string = state.string;  // always points to first byte
      state_start.warn = state.warn;
      state = state_start;
      state_prev = state_init;

      char * buff_track = buff.buff;

      // Very similar to pass 1 loop, but different enough it would be annoying
      // to make a common function

      if(*string && *string != 0x1b && state_has_style_html(state)) {
        buff_track += state_size_and_write_as_html(
          state, state_prev,  buff_track, color_classes, i, 0
        );
        state_prev = state;
      }
      while(1) {
        const char * string_prev = string;
        trail_span = state_has_style_html(state_prev);
        string = strchr(string, 0x1b);
        if(!string) string = state.string + bytes_init;
        else state.pos_byte = (string - state.string);

        // The text since the last ESC
        int bytes_prev = string - string_prev;
        memcpy(buff_track, string_prev, bytes_prev);
        buff_track += bytes_prev;
        state.pos_byte = (string - state.string);

        // State as html, skip if at end of string
        if(*string) {
          state = FANSI_read_next(state);
          string = state.string + state.pos_byte;
          if(*string) {
            buff_track += state_size_and_write_as_html(
              state, state_prev,  buff_track, color_classes, i, 0
            );
          }
          state_prev = state;
          if(!*string) break; // nothing after state, so done
        } else break;
      }
      // Trailing SPAN if needed
      if(trail_span) {
        memcpy(buff_track,span_end, span_end_len);
        buff_track += span_end_len;
      }
      *(buff_track) = '0';  // not strictly needed

      // Final check that we're not out of sync (recall buff.len includes NULL)
      if(buff_track - buff.buff != (int)(bytes_final - 1))
        // nocov start
        error(
          "Internal Error: %s (%td vs %zu).",
          "buffer length mismatch in html generation (2)",
          buff_track - buff.buff, bytes_final - 1
        );
        // nocov end

      // Now create the charsxp with the original encoding.  Since we're only
      // removing SGR and adding FANSI, it should be okay.

      cetype_t chr_type = getCharCE(chrsxp);
      SEXP chrsxp = PROTECT(
        mkCharLenCE(buff.buff, (R_len_t)(buff_track - buff.buff), chr_type)
      );
      SET_STRING_ELT(res, i, chrsxp);
      UNPROTECT(1);
    }
  }
  UNPROTECT(1);
  return res;
}
/*
 * Testing interface
 *
 * x is a 5 x N matrix where, for each column the first value is a color code,
 * and subsequent values correspond to the state.color_extra values.
 */

SEXP FANSI_color_to_html_ext(SEXP x) {
  if(TYPEOF(x) != INTSXP)
    error("Argument must be integer.");  // nocov

  R_xlen_t len = XLENGTH(x);
  if(len % 5)
    error("Argument length not a multipe of 5"); // nocov

  struct FANSI_buff buff = {.len = 0};
  FANSI_size_buff(&buff, 8);

  int * x_int = INTEGER(x);

  SEXP res = PROTECT(allocVector(STRSXP, len / 5));

  for(R_xlen_t i = 0; i < len; i += 5) {
    color_to_html(x_int[i], x_int + (i + 1), buff.buff);
    SEXP chrsxp = PROTECT(mkCharLenCE(buff.buff, 7, CE_BYTES));
    SET_STRING_ELT(res, i / 5, chrsxp);
    UNPROTECT(1);
  }
  UNPROTECT(1);
  return res;
}

/*
 * Escape special HTML characters.
 */

SEXP FANSI_esc_html(SEXP x) {
  if(TYPEOF(x) != STRSXP)
    error("Internal Error: `x` must be a character vector");  // nocov

  R_xlen_t x_len = XLENGTH(x);
  SEXP res = x;
  // Reserve spot on protection stack
  PROTECT_INDEX ipx;
  PROTECT_WITH_INDEX(res, &ipx);

  for(R_xlen_t i = 0; i < x_len; ++i) {
    FANSI_interrupt(i);

    SEXP chrsxp = STRING_ELT(x, i);
    if(chrsxp == NA_STRING) continue;
    FANSI_check_chrsxp(chrsxp, i);
    int bytes = (int) LENGTH(chrsxp);
    const char * string = CHAR(chrsxp);
    struct FANSI_buff buff = {.len=0};

    // - Pass 1: Measure -------------------------------------------------------

    while(*string) {
      if(*string > '>') { // All specials are less than this
        ++string;
        continue;
      }
      switch(*string) {
        case '&': // &amp;
          if(bytes <= FANSI_int_max - 4) bytes += 4;
          else overflow_err2(i);
          break;
        case '"':
        case '\'':
          if(bytes <= FANSI_int_max - 5) bytes += 5;
          else overflow_err2(i);
          break;
        case '<':
        case '>':
          if(bytes <= FANSI_int_max - 3) bytes += 3;
          else overflow_err2(i);
          break;
      }
      ++string;
    }
    // Leftover from prior element (only if can't be merged with new)

    // - Pass 2: Write ---------------------------------------------------------

    if(bytes > LENGTH(chrsxp)) {
      // Allocate target vector if it hasn't been yet
      if(res == x) REPROTECT(res = duplicate(x), ipx);

      // Allocate buffer and do second pass, bytes_final includes space for NULL

      FANSI_size_buff(&buff, final_string_size(bytes, i));

      char * buff_track = buff.buff;
      string = CHAR(chrsxp);

      while(*string) {
        if(*string > '>') { // All specials are less than this
          *(buff_track++) = *(string++);
          continue;
        }
        switch(*string) {
          case '&': // &amp;
            memcpy(buff_track, "&amp;", 5);
            buff_track += 5;
            break;
          case '"':
            memcpy(buff_track, "&quot;", 6);
            buff_track += 6;
            break;
          case '\'':
            memcpy(buff_track, "&#039;", 6);
            buff_track += 6;
            break;
          case '<':
            memcpy(buff_track, "&lt;", 4);
            buff_track += 4;
            break;
          case '>':
            memcpy(buff_track, "&gt;", 4);
            buff_track += 4;
            break;
          default:
            *(buff_track++) = *string;
        }
        ++string;
      }
      *buff_track = 0;
      if(buff_track - buff.buff != bytes)
        // nocov start
        error(
          "Internal Error: %s (%td vs %zu).",
          "buffer length mismatch in html escaping",
          buff_track - buff.buff, bytes
        );
        // nocov end

      cetype_t chr_type = getCharCE(chrsxp);
      SEXP reschr = PROTECT(mkCharLenCE(buff.buff, (R_len_t)(bytes), chr_type));
      SET_STRING_ELT(res, i, reschr);
      UNPROTECT(1);
    }
  }
  UNPROTECT(1);
  return res;
}

