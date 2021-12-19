/*
 * Copyright (C) 2021  Brodie Gaslam
 *
 * This file is part of "fansi - ANSI Control Sequence Aware String Functions"
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2 or 3 of the License.
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

static int sgr_has_color(struct FANSI_sgr sgr) {
  return sgr.color.x || sgr.bgcol.x;
}
static int sgr_has_style_html(struct FANSI_sgr sgr) {
  // generate mask first time around.
  return (sgr.style & STL_MASK2) || sgr.color.x || sgr.bgcol.x;
}
static int sgr_comp_html(
  struct FANSI_sgr target, struct FANSI_sgr current
) {
  return
    // Colors are Different
    FANSI_sgr_comp_color(target, current) ||
    // HTML rendered styles are different
    (
      (target.style & STL_MASK2) !=
      (current.style & STL_MASK2)
    ) ||
    // If one has color both have the same color, but we need to check
    // whether they have different invert status
    (
      (sgr_has_color(target)) &&
      (target.style & STL_INVERT) ^ (current.style & STL_INVERT)
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
static int color_to_8bit(struct FANSI_color color) {
  int col256 = -1;
  switch(color.x & CLR_MASK) {
    case CLR_8:       col256 = color.x & ~CLR_MASK;        break;
    case CLR_BRIGHT:  col256 = (color.x & ~CLR_MASK) + 8;  break;
    case CLR_256:     col256 = color.extra[0];                   break;
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
  struct FANSI_color color, SEXP color_classes, int bg
) {
  int col8bit = color_to_8bit(color);
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
 *   struct FANSI_sgr.color_extra
 * @param buff a buffer with at least 8 bytes allocated.
 * @return the *buff pointer
 */

static char * color_to_html(struct FANSI_color color, char * buff) {
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
  unsigned char clrval = color.x & ~CLR_MASK;
  if(clrval == 9)
    error("Internal Error: applying non-color."); // nocov

  char * buff_track = buff;
  *(buff_track++) = '#';

  switch(color.x & CLR_MASK) {
    case CLR_8:
      memcpy(buff_track, std_8[clrval], 6);
      buff_track += 6;
      break;
    case CLR_BRIGHT:
      memcpy(buff_track, bright[clrval], 6);
      buff_track += 6;
      break;
    case CLR_256: {
      int color_5 = color.extra[0];
      if(color_5 < 16) {
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
      break;
    }
    case CLR_TRU:
      for(int i = 0; i < 3; ++i) {
        char hi = dectohex[color.extra[i] / 16];
        char lo = dectohex[color.extra[i] % 16];
        *(buff_track++) = hi;
        *(buff_track++) = lo;
      }
      break;
    default:
      error("Internal Error: unknown color mode."); // nocov
  }
  *buff_track = 0;
  int dist = (int) (buff_track - buff);
  if(dist != 7)
    error("Internal Error: bad byte count for color (%d).", dist); // nocov

  return buff;
}
static char * oe_sgr_html_err = "Expanding SGR sequences to HTML";

/*
 * Write individual SGR sequence as HTML
 *
 * DANGER: this is a 'W' function, see 'src/write.c' for details.
 */
static int W_state_as_html(
  struct FANSI_buff * buff,
  struct FANSI_state state,
  struct FANSI_state state_prev,
  SEXP color_classes, R_xlen_t i
) {
  /****************************************************\
  | IMPORTANT: KEEP THIS ALIGNED WITH FANSI_csi_write  |
  | although right now ignoring rare escapes in html   |
  \****************************************************/

  // Not all basic styles are html styles (e.g. invert), so sgr only changes
  // on invert when current or previous also has a color style
  int has_cur_sgr = sgr_has_style_html(state.fmt.sgr);
  int has_prev_sgr = sgr_has_style_html(state_prev.fmt.sgr);
  int sgr_change = sgr_comp_html(state.fmt.sgr, state_prev.fmt.sgr);

  int has_cur_url = FANSI_url_active(state.fmt.url);
  int has_prev_url = FANSI_url_active(state_prev.fmt.url);
  int url_change = FANSI_url_comp(state.fmt.url, state_prev.fmt.url);

  const char * err_msg = oe_sgr_html_err;
  struct FANSI_sgr sgr = state.fmt.sgr;

  // FANSI_W_COPY requires variables len, i, and err_msg
  if(sgr_change || url_change) {
    // Close previous
    if(has_prev_sgr) FANSI_W_COPY(buff, "</span>");
    if(has_prev_url) FANSI_W_COPY(buff, "</a>");

    if(has_cur_url) {
      // users responsibility to escape html special chars
      FANSI_W_COPY(buff, "<a href='");
      FANSI_W_MCOPY(buff, URL_STRING(state.fmt.url), URL_LEN(state.fmt.url));
      FANSI_W_COPY(buff, "'>");
    }
    if(has_cur_sgr) {
       FANSI_W_COPY(buff, "<span");
      // Styles
      int invert = sgr.style & STL_INVERT;
      struct FANSI_color color = invert ? sgr.bgcol : sgr.color;
      struct FANSI_color bgcol = invert ? sgr.color : sgr.bgcol;

      // Use provided classes instead of inline styles?
      const char * color_class = get_color_class(color, color_classes, 0);
      const char * bgcol_class = get_color_class(bgcol, color_classes, 1);

      // Class based colors e.g. " class='fansi-color-06 fansi-bgcolor-04'"
      // Brights remapped to 8-15
      if(color_class || bgcol_class) {
        FANSI_W_COPY(buff, " class='");
        if(color_class) FANSI_W_COPY(buff, color_class);
        if(color_class && bgcol_class) FANSI_W_COPY(buff, " ");
        if(bgcol_class) FANSI_W_COPY(buff, bgcol_class);
        FANSI_W_COPY(buff, "'");
      }
      // inline style and/or colors
      if(
        sgr.style & STL_MASK2 ||
        (color.x && (!color_class)) || (bgcol.x && (!bgcol_class))
      ) {
        FANSI_W_COPY(buff, " style='");
        int has_style = 0;
        char color_tmp[8];
        if(color.x && (!color_class)) {
          has_style += FANSI_W_COPY(buff, "color: ");
          FANSI_W_COPY(buff, color_to_html(color, color_tmp));
        }
        if(bgcol.x && (!bgcol_class)) {
          if(has_style) has_style += FANSI_W_COPY(buff, "; ");
          has_style += FANSI_W_COPY(buff,  "background-color: ");
          FANSI_W_COPY(
            buff, color_to_html(bgcol, color_tmp)
          );
        }
        // Styles (need to go after color for transparent to work)
        for(unsigned int i = 0U; i < 9U; ++i)
          if(sgr.style & STL_MASK2 & (1U << i)) {
            if(has_style) FANSI_W_COPY(buff, "; ");
            has_style += FANSI_W_COPY(buff, css_style[i].css);
          }
        FANSI_W_COPY(buff, ";'");
      }
      FANSI_W_COPY(buff, ">");
  } }
  // We've checked len at every step, so it cannot overflow INT_MAX.

  return buff->len;
}
/*
 * Convert SGR Encoded Strings to their HTML equivalents
 */
SEXP FANSI_esc_to_html(
  SEXP x, SEXP warn, SEXP term_cap, SEXP color_classes, SEXP carry
) {
  if(TYPEOF(x) != STRSXP)
    error("Internal Error: `x` must be a character vector");  // nocov
  if(TYPEOF(color_classes) != STRSXP)
    error("Internal Error: `color_classes` must be a character vector");  // nocov

  const char * arg = "x";
  struct FANSI_buff buff;
  FANSI_INIT_BUFF(&buff);

  SEXP ctl = PROTECT(ScalarInteger(1));  // "all"
  int do_carry = STRING_ELT(carry, 0) != NA_STRING;
  struct FANSI_state state_carry = FANSI_carry_init(carry, warn, term_cap, ctl);
  UNPROTECT(1);

  R_xlen_t x_len = XLENGTH(x);
  struct FANSI_state state, state_prev, state_init;
  SEXP empty = PROTECT(mkString(""));
  state = FANSI_state_init(empty, warn, term_cap, (R_xlen_t) 0);
  UNPROTECT(1);

  state_prev = state_init = state;
  state_prev.fmt = state_carry.fmt;

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
    if(do_carry) state = state_prev;
    else state = state_init;

    state.string = string;
    struct FANSI_state state_start = state;
    FANSI_reset_pos(&state_start);
    state.status &= ~STAT_WARNED;
    state_prev = state_init;  // but there are no styles in the string yet

    int bytes_init = (int) LENGTH(chrsxp);

    // Some ESCs may not produce any HTML, and some strings may gain HTML from
    // an ESC from a prior element even if they have no ESCs.
    int has_esc = 0;
    int has_state =
      sgr_has_style_html(state.fmt.sgr) || FANSI_url_active(state.fmt.url);
    int trail_span, trail_a;
    trail_span = trail_a = 0;

    // We cheat by only using FANSI_read_next to read escape sequences as we
    // don't care about display width, etc.  Normally we would _read_next over
    // all characters, not just skip from ESC to ESC.

    const char * err_msg = oe_sgr_html_err;

    // Measure / Write loop
    for(int k = 0; k < 2; ++k) {
      if(k) {
        if(has_esc || has_state) {
          // Allocate target vector if it hasn't been yet
          if(res == x) REPROTECT(res = duplicate(x), ipx);
          // Allocate buffer and reset states for second pass
          FANSI_size_buff(&buff);
          string = state.string;  // always points to first byte
          state_start.status |= state.status & STAT_WARNED;
          state = state_start;
          state_prev = state_init;
        } else break;
      } else {
        FANSI_reset_buff(&buff);
      }
      // Leftover from prior element (only if can't be merged with new)

      if(
        *string && *string != 0x1b &&
        (sgr_has_style_html(state.fmt.sgr) || FANSI_url_active(state.fmt.url))
      ) {
        // dirty hack, state_prev sgr_prev is not exaclty right at beginning
        W_state_as_html(&buff, state, state_prev, color_classes, i);
        state_prev = state;
      }
      // New in this element
      while(1) {
        const char * string_prev = string;
        trail_span = sgr_has_style_html(state_prev.fmt.sgr);
        trail_a = FANSI_url_active(state_prev.fmt.url);
        string = strchr(string, 0x1b);
        if(!string) string = state.string + bytes_init;
        else {
          has_esc = 1;
          state.pos.x = (string - state.string);
        }
        // Intervening bytes before next state
        int bytes_prev = string - string_prev;  // cannot overflow int
        FANSI_W_MCOPY(&buff, string_prev, bytes_prev);
        state.pos.x = (string - state.string);

        // State as html, skip if at end of string
        if(*string) {
          FANSI_read_next(&state, i, arg);
          string = state.string + state.pos.x;
          // dirty hack, state_prev sgr_prev is not exaclty right at beginning
          if(*string)
            W_state_as_html(&buff, state, state_prev, color_classes, i);

          state_prev = state;
          has_state |= sgr_has_style_html(state.fmt.sgr) ||
            FANSI_url_active(state.fmt.url);
          if(!*string) break; // nothing after state, so done
        } else break;
      }
      if(trail_span) FANSI_W_COPY(&buff, "</span>");
      if(trail_a) FANSI_W_COPY(&buff, "</a>");

      if(buff.buff) {  // only ever true if k > 0
        // Now create the charsxp with the original encoding.  Since we're only
        // removing SGR and adding FANSI, it should be okay.
        cetype_t chr_type = getCharCE(chrsxp);
        SEXP chrsxp = PROTECT(FANSI_mkChar(buff, chr_type, i));
        SET_STRING_ELT(res, i, chrsxp);
        UNPROTECT(1);
      }
    }
  }
  FANSI_release_buff(&buff, 1);
  UNPROTECT(1);
  return res;
}
/*
 * Testing interface
 *
 * x is a 5 x N matrix where, for each column the first value is a color code,
 * and subsequent values correspond to the state.sgr.color_extra values.
 *
 * Does not allow for bright mode?
 */

SEXP FANSI_color_to_html_ext(SEXP x) {
  if(TYPEOF(x) != INTSXP)
    error("Argument must be integer.");  // nocov

  R_xlen_t len = XLENGTH(x);
  if(len % 5)
    error("Argument length not a multipe of 5"); // nocov

  struct FANSI_buff buff;
  FANSI_INIT_BUFF(&buff);
  FANSI_size_buff0(&buff, 7);

  int * x_int = INTEGER(x);

  SEXP res = PROTECT(allocVector(STRSXP, len / 5));

  for(R_xlen_t i = 0; i < len; i += 5) {
    struct FANSI_color color;
    unsigned int color_mode = 0;
    if(x_int[i] == 8) {
      if(x_int[i + 1] == 2) color_mode = CLR_TRU;
      else color_mode = CLR_256;
    } else color_mode = CLR_8;
    color.x = x_int[i] | color_mode;
    color.extra[0] = (unsigned char)x_int[i + 2];
    color.extra[1] = (unsigned char)x_int[i + 3];
    color.extra[2] = (unsigned char)x_int[i + 4];
    color_to_html(color, buff.buff);
    SEXP chrsxp = PROTECT(mkCharLenCE(buff.buff, 7, CE_BYTES));
    SET_STRING_ELT(res, i / 5, chrsxp);
    UNPROTECT(1);
  }
  FANSI_release_buff(&buff, 1);
  UNPROTECT(1);
  return res;
}
/*
 * Escape special HTML characters.
 */
SEXP FANSI_esc_html(SEXP x, SEXP what) {
  if(TYPEOF(x) != STRSXP)
    error("Internal Error: `x` must be a character vector");  // nocov
  if(TYPEOF(what) != STRSXP)
    error("Internal Error: `x` must be a character vector");  // nocov

  if(XLENGTH(what) != 1 || STRING_ELT(what, 0) == NA_STRING)
    error("Argument `what` must be scalar character and not NA.");

  SEXP what_chrsxp = STRING_ELT(what, 0);
  R_xlen_t x_len = XLENGTH(x);
  R_len_t what_len = LENGTH(what_chrsxp);

  if(what_len == 0 || x_len == 0) return x;

  // Create a lookup "bitfield" for the 5 chars we can escape
  const unsigned char * what_chr = (const unsigned char *) CHAR(what_chrsxp);
  unsigned int what_val = 0;

  for(R_len_t i = 0; i < what_len; ++i) {
    unsigned const char wc = *(what_chr + i);
    switch(wc) {
      case '&':  what_val |= 1U << 0U; break;
      case '"':  what_val |= 1U << 1U; break;
      case '\'': what_val |= 1U << 2U; break;
      case '<':  what_val |= 1U << 3U; break;
      case '>':  what_val |= 1U << 4U; break;
      default:
        error(
          "%s %s.",
          "Argument `what` may only contain ASCII characters",
          "\"&\", \"<\", \">\", \"'\", or \"\\\"\""
        );
  } }

  SEXP res = x;
  // Reserve spot on protection stack
  PROTECT_INDEX ipx;
  PROTECT_WITH_INDEX(res, &ipx);

  struct FANSI_buff buff;
  FANSI_INIT_BUFF(&buff);

  for(R_xlen_t i = 0; i < x_len; ++i) {
    FANSI_interrupt(i);

    SEXP chrsxp = STRING_ELT(x, i);
    if(chrsxp == NA_STRING) continue;
    FANSI_check_chrsxp(chrsxp, i);
    int len = (int) LENGTH(chrsxp);
    const char * string = CHAR(chrsxp);

    const char * err_msg = "Escaping HTML special characters";
    len = LENGTH(chrsxp);

    // Two passes (k), first one compute incremental length of string, second
    // actually write to the buffer
    for(int k = 0; k < 2; ++k) {
      string = CHAR(chrsxp);

      if(k && len > LENGTH(chrsxp)) {
        FANSI_size_buff0(&buff, len);
        len = LENGTH(chrsxp); // reset so we don't unecessary overflow
        // Allocate result vector if it hasn't been yet
        if(res == x) REPROTECT(res = duplicate(x), ipx);
      }
      // No second pass if no incremental chars
      else if (k) break;
      else FANSI_reset_buff(&buff);

      while(*string) {
        // Skip chars that can't be specials
        if(*string > '>') {
          if(buff.buff) *(buff.buff++) = *string;
          ++string;
          continue;
        }
        --len;    // we're replacing one char, so don't count it
        if(*string == '&' &&       what_val & 1U << 0U)
          len += FANSI_W_COPY(&buff, "&amp;");
        else if(*string == '"' &&  what_val & 1U << 1U)
          len += FANSI_W_COPY(&buff, "&quot;");
        else if(*string == '\'' && what_val & 1U << 2U)
          len += FANSI_W_COPY(&buff, "&#039;");
        else if(*string == '<' &&  what_val & 1U << 3U)
          len += FANSI_W_COPY(&buff, "&lt;");
        else if(*string == '>' &&  what_val & 1U << 4U)
          len += FANSI_W_COPY(&buff, "&gt;");
        // Just advance copy string otherwse
        else {
          ++len;  // we didn't actually replace the char
          if(buff.buff) *(buff.buff++) = *string;
        }
        ++string;
      }
      // Only get here in second pass if we've written
      if(k && buff.buff) {
        *(buff.buff) = 0;
        cetype_t chr_type = getCharCE(chrsxp);
        SEXP reschr = PROTECT(FANSI_mkChar(buff, chr_type, i));
        SET_STRING_ELT(res, i, reschr);
        UNPROTECT(1);
      }
    }
  }
  FANSI_release_buff(&buff, 1);
  UNPROTECT(1);
  return res;
}

