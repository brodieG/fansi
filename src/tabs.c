/*
 * Copyright (C) 2021  Brodie Gaslam
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

/*
 * Determine how many spaces tab width should be
 *
 * state should be at a tab
 *
 * stop_idx/tab_width by ref so previous calculations can be re-used
 */
static int tab_width(
  struct FANSI_state state, int * tab_stops, R_xlen_t stops,
  int * stop_idx, int * tab_width
) {
  if(*(state.string + state.pos_byte) != '\t')
    error("Internal Error: computing tab width on not a tab"); // nocov

  while(state.pos_width >= *tab_width) {
    int stop_size = *(tab_stops + *stop_idx);
    if(stop_size < 1)
    if(*tab_width > FANSI_lim.lim_int.max - stop_size)
      error("Integer overflow when attempting to compute tab width."); // nocov
    *tab_width += stop_size;
    if(*stop_idx < stops - 1) (*stop_idx)++;
  }
  return *tab_width - state.pos_width;
}

SEXP FANSI_tabs_as_spaces(
  SEXP vec, SEXP tab_stops, struct FANSI_buff * buff,  SEXP warn,
  SEXP term_cap, SEXP ctl
) {
  if(TYPEOF(vec) != STRSXP)
    error("Argument 'vec' should be a character vector"); // nocov
  R_xlen_t len = XLENGTH(vec);
  R_xlen_t len_stops = XLENGTH(tab_stops);
  int * tab_stops_i = INTEGER(tab_stops);
  int max_tab_stop = 1;

  // check stops
  if(len_stops < 1)
    error("Internal Error: must have at least one tab stop");  // nocov
  if(len_stops > FANSI_lim.lim_int.max)
    error("Internal Error: can have at most INT_MAX tab stops");  // nocov
  for(R_xlen_t j = 0; j < len_stops; ++j) {
    if(tab_stops_i[j] > max_tab_stop)
      max_tab_stop = tab_stops_i[j];
    if(tab_stops_i[j] < 1)
      error("Internal Error: stop size less than 1.");  // nocov
  }
  const char * source;
  int tabs_in_str = 0;

  SEXP res_sxp = vec;

  PROTECT_INDEX ipx;
  PROTECT_WITH_INDEX(res_sxp, &ipx);  // reserve spot if we need to alloc later

  for(R_xlen_t i = 0; i < len; ++i) {
    FANSI_interrupt(i);
    int tab_count = 0;

    SEXP chr = STRING_ELT(vec, i);
    if(chr == NA_STRING) continue;
    source = CHAR(chr);

    while(*source && (source = strchr(source, '\t'))) {
      if(!tabs_in_str) {
        tabs_in_str = 1;
        REPROTECT(res_sxp = duplicate(vec), ipx);
      }
      ++tab_count;
      ++source;
    }
    if(tab_count) {
      // Figure out possible size of buffer, allowing max_tab_stop for every
      // tab, which should over-allocate

      FANSI_check_chrsxp(chr, i);
      int new_buff_size = (int)LENGTH(chr);
      int tab_extra = max_tab_stop - 1;

      for(int k = 0; k < tab_count; ++k) {
        if(new_buff_size > (FANSI_lim.lim_int.max - tab_extra))
          error(
            "%s%s",
            "Converting tabs to spaces will cause string to be longer than ",
            "allowed INT_MAX."
          );
        new_buff_size += tab_extra;
      }
      FANSI_size_buff(buff, new_buff_size);

      SEXP R_true = PROTECT(ScalarLogical(1));
      SEXP R_one = PROTECT(ScalarInteger(1));
      struct FANSI_state state = FANSI_state_init_full(
        vec, warn, term_cap, R_true, R_true, R_one, ctl, i
      );
      UNPROTECT(2);

      char cur_chr;

      char * buff_track, * buff_start;
      buff_track = buff_start = buff->buff;

      int last_byte = state.pos_byte;
      int warn_old = state.warn;
      int tab_acc_width, tab_stop;
      tab_acc_width = tab_stop = 0;

      while(1) {
        cur_chr = state.string[state.pos_byte];

        int extra_spaces = 0;

        if(cur_chr == '\t') {
          extra_spaces =
            tab_width(state, tab_stops_i, len_stops, &tab_acc_width, &tab_stop);
        } else if (cur_chr == '\n') {
          state = FANSI_reset_width(state);
          tab_acc_width = 0;
          tab_stop = 0;
        }
        // Write string

        if(cur_chr == '\t' || !cur_chr) {
          int write_bytes = state.pos_byte - last_byte;
          memcpy(buff_track, state.string + last_byte, write_bytes);
          buff_track += write_bytes;

          // consume tab and advance

          state.warn = 0;
          state = FANSI_read_next(state, i);
          state.warn = warn_old;
          cur_chr = state.string[state.pos_byte];
          state = FANSI_inc_width(state, extra_spaces, i);
          last_byte = state.pos_byte;

          // actually write the extra spaces

          while(extra_spaces) {
            --extra_spaces;
            *buff_track = ' ';
            ++buff_track;
          }
          if(!cur_chr) *buff_track = 0;
        } else {
          state = FANSI_read_next(state, i);
        }
        if(!cur_chr) break;
      }
      // Write the CHARSXP

      cetype_t chr_type = CE_NATIVE;
      if(state.has_utf8) chr_type = CE_UTF8;
      SEXP chr_sxp = PROTECT(FANSI_mkChar(buff_start, buff_track, chr_type, i));
      SET_STRING_ELT(res_sxp, i, chr_sxp);
      UNPROTECT(1);
    }
  }
  UNPROTECT(1);
  return res_sxp;
}
SEXP FANSI_tabs_as_spaces_ext(
  SEXP vec, SEXP tab_stops, SEXP warn, SEXP term_cap, SEXP ctl
) {
  struct FANSI_buff buff;
  FANSI_INIT_BUFF(&buff);
  SEXP res =
    PROTECT(FANSI_tabs_as_spaces(vec, tab_stops, &buff, warn, term_cap, ctl));
  FANSI_release_buff(&buff, 1);
  UNPROTECT(1);
  return res;
}

