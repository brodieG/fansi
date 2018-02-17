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
 * Determine how many spaces tab width should be
 *
 * state should be at a tab
 */
int FANSI_tab_width(struct FANSI_state state, SEXP tab_stops) {
  R_xlen_t stops = XLENGTH(tab_stops);
  if(!stops)
    error("Internal Error: must have at least one tab stop");  // nocov
  if(*(state.string + state.pos_byte) != '\t')
    error("Internal Error: computing tab width on not a tab"); // nocov

  int tab_width = 0;
  R_xlen_t stop_idx = 0;

  while(state.pos_width >= tab_width) {
    int stop_size = INTEGER(tab_stops)[stop_idx];
    if(!stop_size) error("Internal Error: zero size tab stop.");
    tab_width = FANSI_add_int(tab_width, stop_size);
    if(stop_idx < stops - 1) stop_idx++;
  }
  return tab_width - state.pos_width;
}

SEXP FANSI_tabs_as_spaces(
  SEXP vec, SEXP tab_stops, struct FANSI_buff * buff,  SEXP warn, SEXP term_cap
) {
  if(TYPEOF(vec) != STRSXP)
    error("Argument 'vec' should be a character vector");
  R_xlen_t len = XLENGTH(vec);
  R_xlen_t len_stops = XLENGTH(tab_stops);

  const char * source;
  int tabs_in_str = 0;
  int max_tab_stop = 0;

  SEXP res_sxp = PROTECT(vec);

  for(R_xlen_t i = 0; i < len; ++i) {
    FANSI_interrupt(i);
    int tab_count = 0;

    SEXP chr = STRING_ELT(vec, i);
    if(chr == NA_STRING) continue;

    source = CHAR(chr);

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
    if(tab_count) {
      // Need to convert to UTF8 so width calcs work

      struct FANSI_buff_const buff_utf8 =
        FANSI_string_as_utf8(STRING_ELT(vec, i));

      // Figure out possible size of buffer, allowing max_tab_stop for every
      // tab, which should over-allocate

      int new_buff_size = FANSI_add_int(buff_utf8.len, 1);

      for(int k = 0; k < tab_count; ++k) {
        new_buff_size = FANSI_add_int(new_buff_size, max_tab_stop - 1);
      }
      FANSI_size_buff(buff, new_buff_size);

      struct FANSI_state state = FANSI_state_init(buff_utf8.buff, term_cap);
      char cur_chr;

      char * buff_track, * buff_start;
      buff_track = buff_start = buff->buff;

      int last_byte = state.pos_byte;

      while(1) {
        cur_chr = state.string[state.pos_byte];
        int extra_spaces = 0;

        if(cur_chr == '\t') {
          extra_spaces = FANSI_tab_width(state, tab_stops);
        } else if (cur_chr == '\n') {
          state = FANSI_reset_width(state);
        }

        // Write string

        if(cur_chr == '\t' || !cur_chr) {
          int write_bytes = state.pos_byte - last_byte;
          memcpy(buff_track, state.string + last_byte, write_bytes);
          buff_track += write_bytes;

          // consume tab and advance

          state = FANSI_read_next(state);
          cur_chr = state.string[state.pos_byte];
          state = FANSI_inc_width(state, extra_spaces);
          last_byte = state.pos_byte;

          // actually write the extra spaces

          while(extra_spaces) {
            if(extra_spaces > 10) error("too many spaces");
            --extra_spaces;
            *buff_track = ' ';
            ++buff_track;
          }
          if(!cur_chr) *buff_track = 0;
        }
        if(!cur_chr) break;
        state = FANSI_read_next(state);
      }
      // Write the CHARSXP

      cetype_t chr_type = CE_NATIVE;
      if(state.has_utf8) chr_type = CE_UTF8;

      SEXP chr_sxp = PROTECT(
        mkCharLenCE(buff_start, (int) (buff_track - buff_start), chr_type)
      );
      SET_STRING_ELT(res_sxp, i, chr_sxp);
      UNPROTECT(1);
    }
  }
  UNPROTECT(1);
  return res_sxp;
}
SEXP FANSI_tabs_as_spaces_ext(
  SEXP vec, SEXP tab_stops, SEXP warn, SEXP term_cap
) {
  struct FANSI_buff buff = {.len = 0};

  return FANSI_tabs_as_spaces(vec, tab_stops, &buff, warn, term_cap);
}

