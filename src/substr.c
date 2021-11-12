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

SEXP FANSI_substr(
  SEXP x,
  SEXP start, SEXP stop,
  SEXP type, SEXP rnd,
  SEXP warn, SEXP term_cap,
  SEXP ctl, SEXP norm,
  SEXP carry, SEXP terminate
) {
  if(TYPEOF(start) != INTSXP) error("Internal Error: invalid `start`.");// nocov
  if(TYPEOF(stop) != INTSXP) error("Internal Error: invalid `stop`.");  // nocov
  if(TYPEOF(rnd) != INTSXP || XLENGTH(rnd) != 1)
    error("Internal Error: invalid `rnd`."); // nocov

  if(!FANSI_is_tf(terminate))
    error("Internal Error: invalid `terminate`."); // nocov

  FANSI_val_args(x, norm, carry);

  int rnd_i = asInteger(rnd);
  int norm_i = asLogical(norm);
  int term_i = asLogical(terminate);

  R_xlen_t len = XLENGTH(x);
  R_xlen_t start_l = XLENGTH(start);
  R_xlen_t stop_l = XLENGTH(stop);

  // Unclear whether recycling explicitly is better / worse than taking the
  // modulo.  Presumably it is worse, but since modulo is often division maybe
  // not.  But then maybe ALTREP helps (probably not, INTEGER()) might defeat it.
  // Need to confirm modulo on 1 and on a number greater are fast.  Right now we
  // recycle explicitly in VAL_IN_ENV
  if(len != start_l || len != stop_l)
    error("Internal Error: start/stop not same length as x."); // nocov

  int prt = 0;
  SEXP R_false = PROTECT(ScalarLogical(0)); ++prt;

  // Prep for carry
  int do_carry = STRING_ELT(carry, 0) != NA_STRING;
  struct FANSI_state state_carry = FANSI_carry_init(carry, warn, term_cap, ctl);

  SEXP res = PROTECT(allocVector(STRSXP, len)); ++prt;

  int * start_i = INTEGER(start);
  int * stop_i = INTEGER(stop);

  struct FANSI_state state, state_prev, state_start, state_stop;
  struct FANSI_buff buff;
  FANSI_INIT_BUFF(&buff);

  for(R_xlen_t i = 0; i < len; ++i) {
    FANSI_interrupt(i);
    if(!i) {
      // Handle interaction with carry
      SEXP allowNA, keepNA;
      allowNA = keepNA = R_false;
      state = FANSI_state_init_full(
        x, warn, term_cap, allowNA, keepNA, type, ctl, i, "x"
      );
      switch(state.width_mode) {  // init does broader validation
        case FANSI_COUNT_CHARS:
        case FANSI_COUNT_WIDTH:
        case FANSI_COUNT_GRAPH:
          break;
        default: error("Internal Error: invalid type for `substr`."); // nocov
      }
    } else {
      state = FANSI_state_reinit(state, x, i);
    }
    if(do_carry) {
      state.sgr = state_carry.sgr;
      state.url = state_carry.url;
    }
    state_prev = state;

    // - Corner Cases ----------------------------------------------------------
    int start_ii = start_i[i];
    int stop_ii = stop_i[i];
    if(
      STRING_ELT(x, i) == NA_STRING ||
      start_ii == NA_INTEGER || stop_ii == NA_INTEGER
    ) {
      SET_STRING_ELT(res, i, NA_STRING);
      continue;
    }
    if(start_ii < 1) start_ii = 1;
    if(stop_ii < start_ii) continue;

    // - Start Point -----------------------------------------------------------

    // Recall `start` and `stop` are in 1-index, here we're greedy eating zero
    // width things.
    while(state.string[state.pos_byte] && state.pos_width < start_ii) {
      state_prev = state;
      state = FANSI_read_next(state, i, 1);
    }
    /*
    Rprintf(
      "startii %d stopii %d, w %d %d b %d %d c %d %d\n",
      start_ii,
      stop_ii,
      state.pos_width,
      state_prev.pos_width,
      state.pos_byte,
      state_prev.pos_byte,
      state.string[state.pos_byte],
      state_prev.string[state_prev.pos_byte]
    );
    */
    if(
      state_prev.pos_width == start_ii - 1 &&    // match target point
      state_prev.string[state_prev.pos_byte] &&  // not past end
      state.pos_width > state_prev.pos_width     // confirm there is more width
    ) {
      // Prev read matches exactly the target point, and not past end
      state_start = state_prev;
    } else if (
      state.pos_width > start_ii - 1 &&
      state_prev.pos_width < start_ii - 1
    ) {
      // Overshot, chose prev or current based on round
      switch(rnd_i) {
        case FANSI_RND_START:
        case FANSI_RND_BOTH:
          state_start = state_prev;
          break;
        case FANSI_RND_STOP:
        case FANSI_RND_NEITHER:
          state_start = state;
          break;
        default:
          error("Internal Error: invalid `rnd` value.");  // nocov
      }
    } else if (!state.string[state.pos_byte]) {
      // String finished before finding start, so move on to next string.
      // It wouldn't be crazy to write out this state even on an otherwise empty
      // string, but we've chosen not to do it mostly b/c that's what the R code
      // did.
      if(do_carry) {
        while(state.string[state.pos_byte]) {
          state = FANSI_read_next(state, i, 1);
        }
        state_carry = state;
      }
      continue;
    } else error("Internal Error: unexpected branch finding start."); // nocov

    // - End Point -------------------------------------------------------------

    state_prev.warned = state.warned; // double warnings.
    state = state_prev;
    while(state.string[state.pos_byte] && state.pos_width <= stop_ii) {
      // Only move the end point forward if we add non-CTL elements, this is so
      // that trailing controls are not consumed, but zero width chars are
      if(state.pos_raw > state_prev.pos_raw) state_prev = state;
      state = FANSI_read_next(state, i, 1);
    }
    if(
      state_prev.pos_width == stop_ii ||
      (
        // In terminate mode, don't include escapes we're about to close anyway
        term_i && state_prev.pos_width == state.pos_width &&
        (FANSI_sgr_active(state_prev.sgr) || FANSI_url_active(state_prev.url))
      )
    ) {
      // Favor state_prev to avoid including trailing controls
      state_stop = state_prev;
    } else if (
      state.pos_width > stop_ii &&
      state_prev.pos_width < stop_ii
    ) {
      // Overshot, chose prev or current based on round
      switch(rnd_i) {
        case FANSI_RND_START:
        case FANSI_RND_BOTH:
          state_stop = state;
          break;
        case FANSI_RND_STOP:
        case FANSI_RND_NEITHER:
          state_stop = state_prev;
          break;
        default:
          error("Internal Error: invalid `rnd` value.");  // nocov
      }
    } else if (!state.string[state.pos_byte]) {
      // String finished, use as stop
      state_stop = state;
    } else error("Internal Error: unexpected branch finding start."); // nocov

    if(do_carry) {
      while(state.string[state.pos_byte]) state = FANSI_read_next(state, i, 1);
      state_carry = state;
    }
    // - Extract ---------------------------------------------------------------

    // Do we need to open/close tags?
    int needs_st_sgr = FANSI_sgr_active(state_start.sgr);
    int needs_st_url = FANSI_url_active(state_start.url);
    int needs_cl_sgr = term_i && FANSI_sgr_active(state_stop.sgr);
    int needs_cl_url = term_i && FANSI_url_active(state_stop.url);

    /*
    Rprintf(
      "  pos %d %d - %d %d - %d %d, %d %d term %d\n",
      state_start.pos_byte,
      state_stop.pos_byte,
      state_start.pos_width,
      state_stop.pos_width,
      needs_st_sgr, needs_st_url, needs_cl_sgr, needs_cl_url,
      term_i
    );
    */
    // Measure/Write loop (see src/write.c), this is adapted from wrap.c
    const char * err_msg = "Writing line";

    for(int k = 0; k < 2; ++k) {
      if(!k) FANSI_reset_buff(&buff);
      else   FANSI_size_buff(&buff);
      if(needs_st_sgr) FANSI_W_sgr(&buff, state_start.sgr, norm_i, i);
      if(needs_st_url) FANSI_W_url(&buff, state_start.url, norm_i, i);

      // Actual string, remember state_stop.pos_byte is one past what we need
      const char * string = state_start.string + state_start.pos_byte;
      int bytes = state_stop.pos_byte - state_start.pos_byte;
      FANSI_W_MCOPY(&buff, string, bytes);

      // And turn off CSI styles if needed
      if(needs_cl_sgr) FANSI_W_sgr_close(&buff, state_stop.sgr, norm_i, i);
      if(needs_cl_url) FANSI_W_url_close(&buff, state_stop.url, i);
    }
    // Now create the charsxp, start by determining
    // what encoding to use.
    cetype_t chr_type = CE_NATIVE;
    if(state_stop.has_utf8 > state_start.pos_byte) chr_type = CE_UTF8;
    SET_STRING_ELT(res, i, FANSI_mkChar(buff, chr_type, i));
  }
  FANSI_release_buff(&buff, 1);
  UNPROTECT(prt);
  return res;
}
