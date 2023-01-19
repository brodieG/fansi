/*
 * Copyright (C) Brodie Gaslam
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
 * Go to <https://www.r-project.org/Licenses> for a copies of the licenses.
 */

#include "fansi.h"

static struct FANSI_state FANSI_inc_width(
  struct FANSI_state state, int inc, R_xlen_t i
) {
  if(inc < 0) error("Internal Error: inc may not be negative.");  // nocov
  if(state.pos.w > FANSI_lim.lim_int.max - inc)
    // This error can't really trigger because when expanding tabs to spaces we
    // already check for overflow
    // nocov start
    error(
      "Expanding tabs will cause string to exceed INT_MAX at index [%ju].",
      FANSI_ind(i)
    );
    // nocov end

  state.pos.w += inc;
  return state;
}
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
  if(*(state.string + state.pos.x) != '\t')
    error("Internal Error: computing tab width on not a tab"); // nocov

  while(state.pos.w >= *tab_width) {
    int stop_size = *(tab_stops + *stop_idx);
    if(stop_size < 1)
      error("Internal Error: stop size less than 1.");  // nocov
    if(*tab_width > FANSI_lim.lim_int.max - stop_size)
      error("Integer overflow when attempting to compute tab width."); // nocov
    *tab_width += stop_size;
    if(*stop_idx < stops - 1) (*stop_idx)++;
  }
  return *tab_width - state.pos.w;
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
  const char * err_msg = "Converting tabs to spaces";
  const char * arg = "x";
  const char * source;
  int tabs_in_str = 0;

  SEXP res_sxp = vec;

  int prt = 0;
  PROTECT_INDEX ipx;
  PROTECT_WITH_INDEX(res_sxp, &ipx); prt++;  // reserve spot to alloc later
  struct FANSI_state state;

  SEXP R_true = PROTECT(ScalarLogical(1)); prt++;
  SEXP R_one = PROTECT(ScalarInteger(1)); prt++;
  SEXP keepNA, allowNA, width;
  keepNA = allowNA = R_true;
  width = R_one;

  for(R_xlen_t i = 0; i < len; ++i) {
    FANSI_interrupt(i);
    if(!i) {
      state = FANSI_state_init_full(
        vec, warn, term_cap, allowNA, keepNA, width, ctl, i
      );
    } else FANSI_state_reinit(&state, vec, i);

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
      // tab, which should over-allocate but is faster

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
      // Note: this does not use the measure - write approach; we just
      // overallocate knowing the upper bound of tab space usage.
      FANSI_size_buff0(buff, new_buff_size);

      char cur_chr;

      int last_byte = state.pos.x;
      unsigned int settings = state.settings;  // backup copy of settings
      int tab_acc_width, tab_stop;
      tab_acc_width = tab_stop = 0;

      while(1) {
        cur_chr = state.string[state.pos.x];

        int extra_spaces = 0;

        if(cur_chr == '\t') {
          extra_spaces =
            tab_width(state, tab_stops_i, len_stops, &tab_acc_width, &tab_stop);
        } else if (cur_chr == '\n') {
          FANSI_reset_width(&state);
          tab_acc_width = 0;
          tab_stop = 0;
        }
        // Write string
        if(cur_chr == '\t' || !cur_chr) {
          int write_bytes = state.pos.x - last_byte;
          FANSI_W_MCOPY(buff, state.string + last_byte, write_bytes);

          // consume tab and advance, temporarily suppressing warning
          state.settings &= ~WARN_MASK;
          FANSI_read_next(&state, i, arg);
          state.settings = settings;
          cur_chr = state.string[state.pos.x];
          state = FANSI_inc_width(state, extra_spaces, i);
          last_byte = state.pos.x;

          // actually write the extra spaces
          FANSI_W_FILL(buff, ' ', extra_spaces);
        } else {
          FANSI_read_next(&state, i, arg);
        }
        if(!cur_chr) break;
      }
      // Write the CHARSXP

      cetype_t chr_type = CE_NATIVE;
      if(state.utf8) chr_type = CE_UTF8;
      SEXP chr_sxp =
        PROTECT(FANSI_mkChar0(buff->buff0, buff->buff, chr_type, i));
      SET_STRING_ELT(res_sxp, i, chr_sxp);
      UNPROTECT(1);
    }
  }
  UNPROTECT(prt);
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

