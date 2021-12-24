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
 * Go to <https://www.r-project.org/Licenses> for a copies of the licenses.
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
 * @param stop pos.x corresponding to the position to stop normalizing.
 */

int FANSI_W_normalize(
  struct FANSI_buff * buff, struct FANSI_state *state,
  int stop, R_xlen_t i, const char * err_msg, const char * arg
) {
  struct FANSI_state state_int, state_prev;
  state_int = state_prev  = *state;
  const char * string, * string_prev, * string_last;
  string_prev = string_last = string = state_int.string + state_int.pos.x;
  int any_to_exp = 0;

  // Logic originally based on FANSI_esc_to_html, adapted for explicit stop

  // Find other ESCs
  while(1) {
    string = strchr(string_prev, 0x1b);
    if(!string) string = string_prev + strlen(string_prev);
    state_int.pos.x = (string - state_int.string);
    if(state_int.pos.x >= stop) {
      // Overshot target length
      if(any_to_exp) {
        FANSI_W_MCOPY(buff, string_last, stop - (string_last - state->string));
      }
      break;
    } else if (*string && *string == 0x1b) {
      // We encountered an ESC
      state_prev = state_int;
      FANSI_read_next(&state_int, i, arg);
      // Any special sequence will be re-written.  In some cases, we don't need
      // to do so, but even when things are already normalized, the order of the
      // elements may not be the same.
      if(state_int.status & STAT_SPECIAL) {
        any_to_exp = 1;
        // stuff prior to SGR/URL
        FANSI_W_MCOPY(buff, string_last, string - string_last);
        // Actual SGR
        FANSI_W_bridge(buff, state_prev, state_int, 1, i, err_msg);

        // Keep track of the last point we copied
        string_last = state_int.string + state_int.pos.x;
      }
      string = state_int.string + state_int.pos.x;
    } else if (*string == 0) {
      // We ran out of string (should be impossible if `stop` used correctly)
      error("Internal Error: unexpected `stop` value for normalize."); // nocov
      // if(any_to_exp) {
      //   FANSI_W_MCOPY(buff, string_last, string - string_last);
      // }
      break;
    }
    else error("Internal Error: normalize logic error."); // nocov
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
  int any_na = 0;
  struct FANSI_state state_carry = FANSI_carry_init(carry, warn, term_cap, ctl);
  struct FANSI_state state_start, state;
  const char * err_msg = "Normalizing state";

  for(R_xlen_t i = 0; i < x_len; ++i) {
    FANSI_interrupt(i + index0);
    if(!i) {
      state = FANSI_state_init(x, warn, term_cap, i);
    } else FANSI_state_reinit(&state, x, i);

    SEXP chrsxp = STRING_ELT(x, i);
    if(chrsxp == NA_STRING || (any_na && do_carry)) {
      // duplicate input vector if needed
      if(res == x) REPROTECT(res = duplicate(x), ipx);
      any_na = 1;
      SET_STRING_ELT(res, i, NA_STRING);
      continue;
    }
    // Measure
    if(do_carry) {
      state.fmt.sgr = state_carry.fmt.sgr;
      state.fmt.url = state_carry.fmt.url;
    }
    state_start = state;

    FANSI_reset_buff(buff);
    int len = FANSI_W_normalize(
      buff, &state, (int)LENGTH(chrsxp), i, err_msg, "x"
    );
    state_carry.fmt.sgr = state.fmt.sgr;
    state_carry.fmt.url = state.fmt.url;

    if(len < 0) continue;

    // Write
    if(res == x) REPROTECT(res = duplicate(x), ipx);
    FANSI_size_buff(buff);
    state = state_start;
    state.status |= STAT_WARNED;  // avoid double warnings
    FANSI_W_normalize(
      buff, &state, (int)LENGTH(chrsxp), i, err_msg, "x"
    );

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
// Just a lower overhead version.  Needed b/c `strwrap_ctl` calls normalize from
// R level instead of doing it internally.

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
