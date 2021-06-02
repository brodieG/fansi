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
 * Writes out a String With Expanded SGR
 *
 * Or computes the size required
 *
 * @param state the start of the string.
 * @param sgr (by ref) final state
 * @param buff if NULL, computes the size required, if not writes it.
 */

static int expand(
  char * buff, struct FANSI_state state, R_xlen_t i
) {
  const char * string, * string_prev, * string_last;
  string_prev = string_last = string = state.string + state.pos_byte;
  int len = 0;        // output
  int any_to_exp = 0;
  char * buff_track = buff;

  const char * err_msg = "Expanding SGR";

  // - Pass 1: Measure -------------------------------------------------------

  // Logic based on FANSI_esc_to_html

  // Find other ESCs
  while(1) {
    string = strchr(string_prev, 0x1b);
    if(!string) string = string_prev + strlen(string_prev);
    state.pos_byte = (string - state.string);

    // We encountered an ESC
    if(*string && *string == 0x1b) {
      state = FANSI_read_next(state, i);
      // Not all ESC sequences are SGR, and only non-expanded need re-writing
      if(state.is_sgr && state.non_expanded) {
        any_to_exp = 1;
        // stuff prior to SGR
        len += MCOPY_OR_MEASURE(&buff_track, string_last, string - string_last);

        // Any prior open styles not overriden by new one need to be closed
        struct FANSI_sgr to_close =
          FANSI_sgr_setdiff(state.sgr_prev, state.sgr);
        len += FANSI_sgr_close(buff_track, to_close, len, 1, i);
        if(buff_track) buff_track = buff + len;

        // Any newly open styles will need to be opened
        struct FANSI_sgr to_open = FANSI_sgr_setdiff(state.sgr, state.sgr_prev);
        len += FANSI_sgr_write(buff_track, to_open, len, 1, i);
        if(buff_track) buff_track = buff + len;

        // Keep track of the last point we copied
        string_last = state.string + state.pos_byte;
      }
      string = state.string + state.pos_byte;
    }
    else if (*string == 0) {
      if(any_to_exp) {
        len += MCOPY_OR_MEASURE(&buff_track, string_last, string - string_last);
      } else {
        len = -1;  // No need to write out.
      }
      break;
    }
    else error("Internal Error: logic error HDFAJJH."); // nocov
    string_prev = string;
  }
  if(buff && (buff_track - buff != len))
    error("Internal Error: buffer sync mismatch in expand SGR."); // nocov
  return len;
}

SEXP FANSI_expand_sgr_ext(SEXP x, SEXP warn, SEXP term_cap) {
  if(TYPEOF(x) != STRSXP)
    error("Internal Error: `x` must be a character vector");  // nocov

  R_xlen_t x_len = XLENGTH(x);
  SEXP res = x;
  // Reserve spot on protection stack
  PROTECT_INDEX ipx;
  PROTECT_WITH_INDEX(res, &ipx);
  struct FANSI_buff buff = {.buff=NULL, .len=0};
  struct FANSI_sgr sgr_end = {.color = -1, .bg_color = -1};

  for(R_xlen_t i = 0; i < x_len; ++i) {
    FANSI_interrupt(i);
    SEXP chrsxp = STRING_ELT(x, i);
    if(chrsxp == NA_STRING) continue;
    struct FANSI_state state = FANSI_state_init(x, warn, term_cap, i);
    int len = expand(NULL, state, i);
    if(len < 0) continue;

    // Write
    if(res == x) REPROTECT(res = duplicate(x), ipx);
    FANSI_size_buff(&buff, (size_t)len + 1);
    expand(buff.buff, state, i);
    cetype_t chr_type = getCharCE(chrsxp);
    SEXP reschr =
      PROTECT(FANSI_mkChar(buff.buff, buff.buff + len, chr_type, i));
    SET_STRING_ELT(res, i, reschr);
    UNPROTECT(1);
  }
  UNPROTECT(1);
  return res;
}


