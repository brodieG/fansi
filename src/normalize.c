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
 * Writes out a String With normalized SGR
 *
 * Or computes the size required
 *
 * @param buff if NULL, computes the size required, if not writes it.
 * @param *state state by reference so that we can recover the changed state
 *   info from reading for use in the `carry` case.
 */

static int normalize(
  struct FANSI_buff * buff, struct FANSI_state *state, R_xlen_t i
) {
  struct FANSI_state state_int = *state;
  const char * string, * string_prev, * string_last;
  string_prev = string_last = string = state_int.string + state_int.pos_byte;
  int any_to_exp = 0;

  const char * err_msg = "Normalizing SGR";

  // Logic based on FANSI_esc_to_html

  // Find other ESCs
  while(1) {
    string = strchr(string_prev, 0x1b);
    if(!string) string = string_prev + strlen(string_prev);
    state_int.pos_byte = (string - state_int.string);

    // We encountered an ESC
    if(*string && *string == 0x1b) {
      state_int = FANSI_read_next(state_int, i, 1);
      // Any special sequence will be re-written.  In some cases, we don't need
      // to do so, but even when things are already normalized, the order of the
      // elements may not be the same.
      if(state_int.last_special) {
        any_to_exp = 1;
        // stuff prior to SGR/URL
        FANSI_W_MCOPY(buff, string_last, string - string_last);

        // Any prior open styles not overriden by new one need to be closed
        struct FANSI_sgr to_close =
          FANSI_sgr_setdiff(state_int.sgr_prev, state_int.sgr);

        FANSI_W_sgr_close(buff, to_close, 1, i);

        // Any newly open styles will need to be opened
        struct FANSI_sgr to_open =
          FANSI_sgr_setdiff(state_int.sgr, state_int.sgr_prev);
        FANSI_W_sgr(buff, to_open, 1, i);

        // Any changed URLs will need to be written (empty URL acts as a closer
        // so simpler than with SGR).
        if(FANSI_url_comp(state_int.url, state_int.url_prev))
          FANSI_W_url(buff, state_int.url, 1, i);

        // Keep track of the last point we copied
        string_last = state_int.string + state_int.pos_byte;
      }
      string = state_int.string + state_int.pos_byte;
    }
    else if (*string == 0) {
      if(any_to_exp) {
        FANSI_W_MCOPY(buff, string_last, string - string_last);
      }
      break;
    }
    else error("Internal Error: logic error HDFAJJH."); // nocov
    string_prev = string;
  }
  *state = state_int;
  return any_to_exp ? buff->len : -1;
}

static SEXP normalize_state_int(
  SEXP x, SEXP warn, SEXP term_cap, SEXP carry,
  struct FANSI_buff *buff, R_xlen_t index0
) {
  if(TYPEOF(x) != STRSXP)
    error("Internal Error: `x` must be a character vector");  // nocov

  int prt = 0;
  R_xlen_t x_len = XLENGTH(x);
  SEXP res = x;
  // Reserve spot on protection stack
  PROTECT_INDEX ipx;
  PROTECT_WITH_INDEX(res, &ipx); ++prt;

  SEXP ctl = PROTECT(ScalarInteger(1)); ++prt;  // "all"
  int do_carry = STRING_ELT(carry, 0) != NA_STRING;
  struct FANSI_state state_carry = FANSI_carry_init(carry, warn, term_cap, ctl);

  for(R_xlen_t i = 0; i < x_len; ++i) {
    FANSI_interrupt(i + index0);
    SEXP chrsxp = STRING_ELT(x, i);
    if(chrsxp == NA_STRING) continue;

    // Measure
    struct FANSI_state state_start, state;
    state = FANSI_state_init(x, warn, term_cap, i);
    if(do_carry) {
      state.sgr = state_carry.sgr;
      state.url = state_carry.url;
    }
    state_start = state;

    FANSI_reset_buff(buff);
    int len = normalize(buff, &state, i);
    state_carry.sgr = state.sgr;
    state_carry.url = state.url;

    if(len < 0) continue;

    // Write
    if(res == x) REPROTECT(res = duplicate(x), ipx);
    FANSI_size_buff(buff);
    state = state_start;
    state.warn = 0;  // avoid double warnings
    normalize(buff, &state, i);

    cetype_t chr_type = getCharCE(chrsxp);
    SEXP reschr = PROTECT(FANSI_mkChar(*buff, chr_type, i));
    SET_STRING_ELT(res, i, reschr);
    UNPROTECT(1);
  }
  UNPROTECT(prt);
  return res;
}

SEXP FANSI_normalize_state_ext(SEXP x, SEXP warn, SEXP term_cap, SEXP carry) {
  if(TYPEOF(x) != STRSXP)
    error("Internal Error: `x` must be a character vector");  // nocov

  struct FANSI_buff buff;
  FANSI_INIT_BUFF(&buff);
  SEXP res = PROTECT(normalize_state_int(x, warn, term_cap, carry, &buff, 0));
  FANSI_release_buff(&buff, 1);
  UNPROTECT(1);
  return res;
}
// List version to use with result of `strwrap_ctl(..., unlist=FALSE)`
// Just a lower overhead version.

SEXP FANSI_normalize_state_list_ext(
  SEXP x, SEXP warn, SEXP term_cap, SEXP carry
) {
  if(TYPEOF(x) != VECSXP)
    error("Internal Error: `x` must be a list vector");  // nocov

  SEXP res = x;
  // Reserve spot on protection stack
  PROTECT_INDEX ipx;
  PROTECT_WITH_INDEX(res, &ipx);
  struct FANSI_buff buff;
  FANSI_INIT_BUFF(&buff);

  R_xlen_t i0 = 0;  // for interrupt across vector elements
  R_xlen_t llen = XLENGTH(x);
  for(R_xlen_t i = 0; i < llen; ++i) {
    SEXP elt0 = VECTOR_ELT(x, i);
    if(i0 > FANSI_lim.lim_R_xlen_t.max - XLENGTH(elt0)) i0 = 0;
    SEXP elt1 = PROTECT(
      normalize_state_int(elt0, warn, term_cap, carry, &buff, i0)
    );
    // If unequal, normalization occurred
    if(elt0 != elt1) {
      if(res == x) REPROTECT(res = duplicate(x), ipx);
      SET_VECTOR_ELT(res, i, elt1);
    }
    UNPROTECT(1);
  }
  FANSI_release_buff(&buff, 1);
  UNPROTECT(1);
  return res;
}