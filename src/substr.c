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
 * @param state modified by reference.
 */

static SEXP substr_one(
  struct FANSI_state * state, struct FANSI_buff * buff, R_xlen_t i,
  int start, int stop, int rnd_i, int norm_i, int term_i, int keep_i,
  int x_len
) {
  // - Start Point -----------------------------------------------------------

  struct FANSI_state state_prev, state_anchor, state_start, state_stop;
  state_anchor = state_prev = *state;

  // Recall `start` and `stop` are in 1-index, here we're greedy eating zero
  // width things. `state_prev` tracks the last valid break point.
  while(state->string[state->pos_byte] && state->pos_width < start) {
    if(
      state->pos_raw == state_prev.pos_raw ||   // read an escape
      state->pos_width > state_prev.pos_width   // moved column in string
    )
      state_prev = *state;

    // Read all zero width following next char in addition to next char
    struct FANSI_state state_tmp;
    *state = state_tmp = FANSI_read_next(*state, i, 1);
    while(
      state_tmp.pos_width == state->pos_width &&
      state_tmp.string[state_tmp.pos_byte]
    ) {
      *state = state_tmp;
      state_tmp = FANSI_read_next(state_tmp, i, 1);
    }
    if(
      !state_tmp.string[state_tmp.pos_byte] &&
      state_tmp.pos_width == state->pos_width
    )
      *state = state_tmp;

    state->warned = state_tmp.warned;
  }
  /*
  Rprintf(
    "startii %d stopii %d, w %d %d b %d %d c %d %d\n",
    start,
    stop,
    state->pos_width,
    state_prev.pos_width,
    state->pos_byte,
    state_prev.pos_byte,
    state->string[state->pos_byte],
    state_prev.string[state_prev.pos_byte]
  );
  */
  if (state_prev.pos_width == start - 1) {
    state_start = state_prev;
  } else if (state->pos_width >= start) {
    // Overshot
    if(!(rnd_i == FANSI_RND_START || rnd_i == FANSI_RND_BOTH)) {
      // Not okay, collect trail zero width
      do{
        if(state->pos_raw > state_prev.pos_raw) state_prev = *state;
        *state = FANSI_read_next(*state, i, 1);
      } while(
        state->string[state->pos_byte] &&
        state->pos_width == state_prev.pos_width
      );
    }
    state_start = state_prev;
  } else if (!state->string[state->pos_byte]) {
    return mkChar("");
  } else if (rnd_i == FANSI_RND_START || rnd_i == FANSI_RND_BOTH) {
    state_start = state_prev;
  } else state_start = *state;

  // Rprintf("start %d %d\n", state_start.pos_byte, state_start.pos_width);

  // - End Point -------------------------------------------------------------

  state_prev.warned = state->warned; // double warnings.
  *state = state_prev;

  // Keep moving the end point forward until we're clearly over the limit, or
  // there are no non-zero width characters to consume.  Drop any trailing
  // controls.  Some terminals (e.g. MacOS term, iterm2) treat control
  // sequences as being out-of-band, i.e. they don't interfere with combining
  // glyphs, etc.
  while(state->string[state->pos_byte] && state->pos_width <= stop) {
    if(state->pos_raw > state_prev.pos_raw) state_prev = *state;
    *state = FANSI_read_next(*state, i, 1);
  }
  state_prev.warned = state->warned;
  // If we are allowed to overshoot, keep consuming zero-width non-CTL
  /*
  Rprintf(
    "rnd %d p_w %d %d\n", rnd_i, state->pos_width, state_prev.pos_width
  );
  */
  if(
    state_prev.pos_width == stop &&
    (state->pos_width > stop || state->pos_raw == state_prev.pos_raw)
  ) {
    // Finished exactly, did not add any interesting chars
    state_stop = state_prev;
  } else if(
    state_prev.pos_width < stop &&
    (rnd_i == FANSI_RND_STOP || rnd_i == FANSI_RND_BOTH) &&
    state->string[state->pos_byte]
  ) {
    // Overshot, collect trail zero width.  A mess b/c we need to handle
    // separately the case where the string ends
    state_prev = *state;
    while(state->pos_width == state_prev.pos_width) {
      if(state->pos_raw > state_prev.pos_raw) state_prev = *state;
      if(!state->string[state->pos_byte]) break;
      *state = FANSI_read_next(*state, i, 1);
    }
    state_stop = state_prev;
  } else if(!state->string[state->pos_byte]) {
    // Ran out of string, want to include trailing controls b/c we selected
    // past end of string, but not SGR/OSC if terminating
    if(term_i) {
      struct FANSI_state state_tmp = state_prev;
      while(state_tmp.string[state_tmp.pos_byte]) {
        state_tmp = FANSI_read_next(state_tmp, i, 1);
        if(!state_tmp.last_special) state_prev = state_tmp;
      }
      state_stop = state_prev;
      state_stop.warned = state_tmp.warned;
    } else {
      state_stop = *state;
    }
  } else if(
    state_prev.pos_width < stop && state->pos_width > stop &&
    !(rnd_i == FANSI_RND_STOP || rnd_i == FANSI_RND_BOTH)
  ) {
    state_stop = state_prev;
  } else error("Internal Error: bad `stop` state.");
  /*
  Rprintf(
    "w %d %d b %d %d c %d %d\n",
    state->pos_width,
    state_prev.pos_width,
    state->pos_byte,
    state_prev.pos_byte,
    state->string[state->pos_byte],
    state_prev.string[state_prev.pos_byte]
  );
  */
  // - Extract ---------------------------------------------------------------

  // Do we need to open/close tags?  Never open / close tags for lead / trail
  // substrings in replacement mode.
  int needs_cl_sgr = keep_i != 2 && term_i && FANSI_sgr_active(state_stop.sgr);
  int needs_cl_url = keep_i != 2 && term_i && FANSI_url_active(state_stop.url);

  /*
  int needs_st_sgr = keep_i != 1 && FANSI_sgr_active(state_start.sgr);
  int needs_st_url = keep_i != 1 && FANSI_url_active(state_start.url);
  // Corner case exit, nothing to write
  if(
    state_start.pos_byte == state_stop.pos_byte &&
    (term_i || !(needs_st_sgr || needs_st_url))
  )
    continue;
  */

  // Adjust start/end points for keep mode for replacement substrings
  int start_byte, stop_byte;
  start_byte = keep_i == 1 ? 0 : state_start.pos_byte;
  stop_byte = keep_i == 2 ? x_len : state_stop.pos_byte;

  /**
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
  const char * err_msg = "Writing substring";

  for(int k = 0; k < 2; ++k) {
    if(!k) FANSI_reset_buff(buff);
    else   FANSI_size_buff(buff);

    // Use bridge do write opening styles
    if(keep_i != 1)
      FANSI_W_bridge(buff, state_anchor, state_start, norm_i, i);

    // Actual string, remember state_stop.pos_byte is one past what we need
    const char * string = state_start.string + start_byte;
    int bytes = stop_byte - start_byte;
    FANSI_W_MCOPY(buff, string, bytes);

    // And turn off CSI styles if needed
    if(needs_cl_sgr) FANSI_W_sgr_close(buff, state_stop.sgr, norm_i, i);
    if(needs_cl_url) FANSI_W_url_close(buff, state_stop.url, i);
  }
  *state = state_stop;

  // Now create the charsxp, start by determining what encoding to use.
  cetype_t chr_type = CE_NATIVE;
  if(state_stop.has_utf8 > state_start.pos_byte) chr_type = CE_UTF8;

  return FANSI_mkChar(*buff, chr_type, i);
}
// Extract Substring (`substr_ctl`)

static SEXP substr_extract(
  SEXP x, SEXP start, SEXP stop, SEXP carry,
  struct FANSI_state state, struct FANSI_buff * buff,
  int rnd_i, int norm_i, int term_i, int keep_i
) {
  R_xlen_t len = XLENGTH(x);
  if(len < 1) error("Internal Error: must have at least one value.");
  int prt = 0;
  SEXP res = PROTECT(allocVector(STRSXP, len)); ++prt;

  // Prep for carry
  int carry_i = STRING_ELT(carry, 0) != NA_STRING;
  struct FANSI_state state_carry = state;
  if(carry_i) {
    state_carry.string = CHAR(STRING_ELT(carry, 0));
    state_carry.arg = "carry";
    while(state_carry.string[state_carry.pos_byte]) {
      state_carry = FANSI_read_next(state_carry, 0, 1);
    }
  }
  int * start_i = INTEGER(start);
  int * stop_i = INTEGER(stop);

  for(R_xlen_t i = 0; i < len; ++i) {
    FANSI_interrupt(i);
    state = FANSI_state_reinit(state, x, i);
    int start_ii = start_i[i];
    int stop_ii = stop_i[i];
    if(
      STRING_ELT(x, i) == NA_STRING ||
      start_ii == NA_INTEGER || stop_ii == NA_INTEGER
    ) {
      SET_STRING_ELT(res, i, NA_STRING);
    } else {
      if(start_ii < 1) start_ii = 1;
      if(stop_ii < start_ii) continue;
      if(carry_i) {
        state.sgr = state_carry.sgr;
        state.url = state_carry.url;
      }
      int x_len = (int) LENGTH(STRING_ELT(x, i));
      SET_STRING_ELT(
        res, i,
        substr_one(
          &state, buff, i, start_ii, stop_ii, rnd_i, norm_i, term_i, keep_i,
          x_len
      ) );
      if(carry_i) {
        while(state.string[state.pos_byte]) state = FANSI_read_next(state, i, 1);
        state_carry.sgr = state.sgr;
        state_carry.url = state.url;
  } } }
  UNPROTECT(prt);
  return res;
}
// Replace Substring (`substr_ctl<-`)

/*
static SEXP substr_rep(
  SEXP x, SEXP start, SEXP stop, SEXP value, SEXP carry,
  struct FANSI_state state, struct FANSI_buff * buff,
  int rnd_i, int norm_i, int term_i, int keep_i
) {
  R_xlen_t len = XLENGTH(x);
  if(len < 1) error("Internal Error: must have at least one value.");

  int prt = 0;
  SEXP res = PROTECT(allocVector(STRSXP, len)); ++prt;

  // Compute `start` for `value`
  // Compute `stop` for `value`
  // Determine if `stop` for `value` can be honored
  // If yes, done?
  UNPROTECT(prt);
  return res;
}
*/

// @param keep 0 (normal substring), 1 (replace lead), 2 (replace trail), used
//   to help maintain illusion of replacements happening in place (e.g. don't
//   modify start/end, but also keep track of carry status).

SEXP FANSI_substr(
  SEXP x,
  SEXP start, SEXP stop,
  SEXP value,
  SEXP type, SEXP rnd,
  SEXP warn, SEXP term_cap,
  SEXP ctl, SEXP norm,
  SEXP carry, SEXP terminate,
  SEXP keep
) {
  if(TYPEOF(start) != INTSXP) error("Internal Error: invalid `start`.");// nocov
  if(TYPEOF(stop) != INTSXP) error("Internal Error: invalid `stop`.");  // nocov
  if(TYPEOF(rnd) != INTSXP || XLENGTH(rnd) != 1)
    error("Internal Error: invalid `rnd`."); // nocov
  if(TYPEOF(keep) != INTSXP || XLENGTH(keep) != 1)
    error("Internal Error: invalid `keep` (1)."); // nocov
  if(!FANSI_is_tf(terminate))
    error("Internal Error: invalid `terminate`."); // nocov
  if(TYPEOF(type) == INTSXP && XLENGTH(type) == 1) {
    switch(asInteger(type)) {
      case FANSI_COUNT_CHARS:
      case FANSI_COUNT_WIDTH:
      case FANSI_COUNT_GRAPH:
        break;
      default: error("Internal Error: invalid `type` for `substr`."); // nocov
  } }
  if(
    (TYPEOF(value) != NILSXP && TYPEOF(value) != STRSXP) ||
    (TYPEOF(value) == STRSXP && XLENGTH(value) != XLENGTH(x))
  )
    error("Internal Error: invalid `value`."); // nocov

  FANSI_val_args(x, norm, carry);

  int rnd_i = asInteger(rnd);
  int norm_i = asLogical(norm);
  int term_i = asLogical(terminate);
  int keep_i = asInteger(keep);  // see param doc

  // To support replacement mode treatement of trailing and leading strings
  if(keep_i < 0 || keep_i > 2)
    error("Internal Error: invalid `keep` (2)."); // nocov
  if(keep_i == 2 && term_i)
    error("Internal Error: invalid `keep` - `term` combo."); // nocov

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
  struct FANSI_buff buff;
  FANSI_INIT_BUFF(&buff);
  struct FANSI_state state;

  SEXP res;
  if(len) {
    SEXP allowNA, keepNA;
    allowNA = keepNA = PROTECT(ScalarLogical(0)); ++prt;
    state = FANSI_state_init_full(
      x, warn, term_cap, allowNA, keepNA, type, ctl, (R_xlen_t) 0, "x"
    );
    // Note, UNPROTECT'ed SEXPs returned below
    if(value == R_NilValue) {
      res = substr_extract(
        x, start, stop, carry, state, &buff, rnd_i, norm_i, term_i, keep_i
      );
    } else {
      res = R_NilValue;
      // res = substr_replace(
      //   x, start, stop, value, carry, state, &buff, rnd_i, term_i, norm_i,
      //   keep_i
      // );
    }
  } else res = allocVector(STRSXP, len);

  FANSI_release_buff(&buff, 1);
  UNPROTECT(prt);
  return res;
}
