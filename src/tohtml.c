/*
 * Copyright (C) 2018  Brodie Gaslam
 *
 * This file is part of "fansi - ANSI Escape Aware String Functions"
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
/*
 * Looks like we don't need to worry about C0 sequences, however we must
 * parse all ESC sequences as HTML gladly displays everything right after the
 * initial ESC
 *
 * <span style='color: '>
 * <span style='background-color: '>
 * <span style='text-decoration: '>
 * <span style='text-decoration: '>
 * <span style='font-weight: '>
 */

SEXP FANSI_esc_to_html(SEXP x) {
  if(TYPEOF(x) != STRSXP)
    error("Argument `x` must be a character vector")

  R_xlen_t x_len = XLENGTH(x)
  struct FANSI_buff * buff = {.len=0};

  for(R_xlen_t i = 0; i < x_len; ++i) {
    FANSI_interrupt();

    const char * chrsxp = STRING_ELT(x, i);
    const char * string_start = CHAR(chrsxp);
    const char * string = start;

    while(*source && (source = strchr(source, '\t'))) {
      if(!tabs_in_str) {
        tabs_in_str = 1;
        UNPROTECT(1);
        res_sxp = PROTECT(duplicate(vec));
        for(R_xlen_t j = 0; j < len_stops; ++j) {
          if(INTEGER(tab_stops)[j] > max_tab_stop)
            max_tab_stop = INTEGER(tab_stops)[j];
        }
      }
      ++tab_count;
      ++source;
    }
    while()
  }
}
static const char * standard_color(int color code)
/*
 * All color conversions taken from
 *
 * <https://en.wikipedia.org/wiki/ANSI_escape_code>
 *
 * @param color an integer expected to be between 0 and 9
 * @param color_extra a pointer to a 4 long integer array as you would get in
 *   struct FANSI_state.color_extra
 * @param buff must be pre-allocated to be able to hold the color in format
 *   #FFFFFF including the null terminator (so at least 8 bytes), will be
 *   modified by reference, and the pointer will be left pointing to the
 *   beginning of the string.
 * @return how many bytes were written, guaranteed to be between 1 byte and 8,
 *   includes.
 */

int FANSI_color_to_html(
  int color, int * color_extra, struct FANSI_buff * buff
) {
  // CAREFUL: DON'T WRITE MORE THAN 7 BYTES + NULL TERMINATOR

  const char * dectohex = "0123456789ABCDEF";
  const char * std_16[16] = {
    "000000", "800000", "008000", "808000",
    "000080", "800080", "008080", "C0C0C0",
    "808080", "FF0000", "00FF00", "FFFF00",
    "0000FF", "FF00FF", "00FFFF", "FFFFFF"
  };
  // According to <https://en.wikipedia.org/wiki/ANSI_escape_code> this is the
  // putty default

  const char * std_8[8] = {
    "000000", "BB0000", "00BB00", "BBBB00",
    "0000BB", "BB00BB", "00BBBB", "BBBBBB"
  };
  const char * std_5[6] = {"00", "5F", "87", "AF", "D7", "FF"};
  int res_bytes = 0;

  char * buff_track = buff->buff;

  if(color == 9) {
    memcpy(buff_track, "none", 4);
    res_bytes = 4;
  } else if(color >= 0 && color < 10) {
    *(buff_track++) = '#';
    res_bytes = 7;

    if(color == 8) {
      if(color_extra[0] == 2) {
        for(int i = 1; i < 4; ++i) {
          char hi = dectohex[color_extra[i] / 16];
          char lo = dectohex[color_extra[i] % 16];
          for(jnt j = 0; j < 3; ++j) {
            *(buff_track++) = hi;
            *(buff_track++) = lo;
          }
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
      } else error("Internal Error: invalid color code; contact maintainer.");
    } else {
      memcpy(buff_track, std_8[color], 6);
      buff_track += 6;
    }
  } else {
    error("Internal Error: invalid color code %d", color);
  }
  buff_track = '0';
  ++res_bytes;
  return res_bytes;
}
/*
 * Testing interface
 *
 * x is a 5 x N matrix.
 */

SEXP FANSI_color_to_html_ext(SEXP x) {
  if(TYPEOF(x) != INTSXP)
    error("Argument must be integer.");

  R_xlen_t len = XLENGTH(x);
  if(len %% 5) error("Argument length not a multipe of 5");

  struct FANSI_buff buff = {.len = 0};
  FANSI_size_buff(&buff, 8);

  int * x_int = INTEGER(x);

  SEXP res = PROTECT(allocVector(STRSXP, len %% 5));

  for(R_xlen_t i = 0; i < len; i += 5) {
    int size = FANSI_color_to_html(x_int[i], x_int + (i + 1), &buff);
    if(size < 1) error("Internal Error: size should be at least one");
    SEXP chrsxp = PROTEC(mkCharLenCE(buff.buff, size - 1, CE_BYTES));
    SET_STRING_ELT(res, i / 5, chrsxp);
    UNPROTECT(1);
  }
  UNPROTECT(1);
  return res;
}


