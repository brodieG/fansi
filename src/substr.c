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

static int has_8bit(const char * x) {
  while((unsigned char)(*x) > 0 && (unsigned char)(*x) < 0xff) ++x;
  return !(*x);
}

/*
 * Compute start and end states for a single substring.  Results are "returned"
 * by modifying the by-ref structs.
 *
 * Calling script needs to decided what to do when stop < start; this code will
 * assume state_stop >= state_start.
 *
 * @param state_start starting state, will be overwritten.
 * @param state_stop write only.
 * @return width of substring, in whatever width units have been selected.
 */

static int substr_calc_points(
  struct FANSI_state * state_start, struct FANSI_state * state_stop,
  R_xlen_t i, int start, int stop, int rnd_i, int term_i
) {
  *state_stop = *state_start;

  // - Start Point -----------------------------------------------------------

  struct FANSI_state state, state_prev;
  state_prev = state = *state_start;

  // Recall `start` and `stop` are in 1-index, here we're greedy eating zero
  // width things. `state_prev` tracks the last valid break point.
  while(state.string[state.pos_byte] && state.pos_width < start) {
    if(
      state.pos_raw == state_prev.pos_raw ||   // read an escape
      state.pos_width > state_prev.pos_width   // moved column in string
    )
      state_prev = state;

    // Read all zero width following next char in addition to next char
    struct FANSI_state state_tmp;
    FANSI_read_next(&state, i, 1);
    state_tmp = state;
    while(
      state_tmp.pos_width == state.pos_width &&
      state_tmp.string[state_tmp.pos_byte]
    ) {
      state = state_tmp;
      FANSI_read_next(&state_tmp, i, 1);
    }
    if(
      !state_tmp.string[state_tmp.pos_byte] &&
      state_tmp.pos_width == state.pos_width
    )
      state = state_tmp;

    state.warned = state_tmp.warned;
  }
  /*
  Rprintf(
    "startii %d stopii %d, w %d %d b %d %d c %d %d\n",
    start,
    stop,
    state.pos_width,
    state_prev.pos_width,
    state.pos_byte,
    state_prev.pos_byte,
    state.string[state.pos_byte],
    state_prev.string[state_prev.pos_byte]
  );
  */
  // Remember there are non SGR/URL controls!  These are not re-issued.
  if (state.pos_width == start - 1) {
    // Consume leading zero widths; controls will be re-issued on output since
    // they are recorded in state?
    *state_start = state;
  } else if (state_prev.pos_width == start - 1) {
    *state_start = state_prev;
  } else if (state.pos_width >= start) {
    // Overshot
    if(!(rnd_i == FANSI_RND_START || rnd_i == FANSI_RND_BOTH)) {
      // Not okay, collect trail zero width
      do{
        if(state.pos_raw > state_prev.pos_raw) state_prev = state;
        FANSI_read_next(&state, i, 1);
      } while(
        state.string[state.pos_byte] &&
        state.pos_width == state_prev.pos_width
      );
    }
    *state_start = state_prev;
  } else if (!state.string[state.pos_byte]) {
    *state_start = state;
  } else if (rnd_i == FANSI_RND_START || rnd_i == FANSI_RND_BOTH) {
    *state_start = state_prev;
  } else *state_start = state;

  //Rprintf("start %d %d\n", state_start->pos_byte, state_start->pos_width);

  // - End Point -------------------------------------------------------------

  state_start->warned = state.warned; // double warnings.
  state = state_prev = *state_start;

  // Keep moving the end point forward until we're clearly over the limit, or
  // there are no non-zero width characters to consume.  Drop any trailing
  // controls.  Some terminals (e.g. MacOS term, iterm2) treat control
  // sequences as being out-of-band, i.e. they don't interfere with combining
  // glyphs, etc.
  while(state.string[state.pos_byte] && state.pos_width <= stop) {
    if(state.pos_raw > state_prev.pos_raw) state_prev = state;
    FANSI_read_next(&state, i, 1);
  }
  state_prev.warned = state.warned;
  // If we are allowed to overshoot, keep consuming zero-width non-CTL
  /*
  Rprintf(
    "rnd %d p_w %d %d\n", rnd_i, state.pos_width, state_prev.pos_width
  );
  */
  if(
    state_prev.pos_width == stop &&
    (state.pos_width > stop || state.pos_raw == state_prev.pos_raw)
  ) {
    // Finished exactly, did not add any interesting chars
    *state_stop = state_prev;
  } else if(
    state_prev.pos_width < stop &&
    (rnd_i == FANSI_RND_STOP || rnd_i == FANSI_RND_BOTH) &&
    state.string[state.pos_byte]
  ) {
    // Overshot, collect trail zero width.  A mess b/c we need to handle
    // separately the case where the string ends
    state_prev = state;
    while(state.pos_width == state_prev.pos_width) {
      if(state.pos_raw > state_prev.pos_raw) state_prev = state;
      if(!state.string[state.pos_byte]) break;
      FANSI_read_next(&state, i, 1);
    }
    *state_stop = state_prev;
  } else if(!state.string[state.pos_byte]) {
    // Ran out of string, want to include trailing controls b/c we selected
    // past end of string, but not SGR/OSC if terminating
    if(term_i) {
      struct FANSI_state state_tmp = state_prev;
      while(state_tmp.string[state_tmp.pos_byte]) {
        FANSI_read_next(&state_tmp, i, 1);
        if(!state_tmp.last_special) state_prev = state_tmp;
      }
      *state_stop = state_prev;
      state_stop->warned = state_tmp.warned;
    } else {
      *state_stop = state;
    }
  } else if(
    state_prev.pos_width < stop && state.pos_width > stop &&
    !(rnd_i == FANSI_RND_STOP || rnd_i == FANSI_RND_BOTH)
  ) {
    *state_stop = state_prev;
  } else if (
    stop < state_prev.pos_width
  ) {
    // Stop was already less than the starting point, so don't read anything
    *state_stop = state_prev;
  } else error("Internal Error: bad `stop` state."); // nocov
  if(state_start->pos_byte > state_stop->pos_byte) {
    error("Internal Error: bad `stop` state 2."); // nocov
  }
  /*
  Rprintf(
    "w %d %d b %d %d c %d %d startb %d stopb %d\n",
    state.pos_width,
    state_prev.pos_width,
    state.pos_byte,
    state_prev.pos_byte,
    state.string[state.pos_byte],
    state_prev.string[state_prev.pos_byte],
    state_start->pos_byte,
    state_stop->pos_byte
  );
  */
  return state_stop->pos_width - state_start->pos_width;
}

/*
 * @param state modified by reference.
 * @param state_ref state at end of previous extracted substring.
 * @param mode whether in measure (0) or write (1) mode
 * @return if in measure, an integer vector with the size of the substring,
 *   otherwise the substring.
 */

static SEXP substr_one(
  struct FANSI_state * state, struct FANSI_state ref, struct FANSI_buff * buff,
  R_xlen_t i, int start, int stop, int rnd_i, int norm_i, int term_i
) {
  struct FANSI_state state_start, state_stop;
  state_start = state_stop = *state;
  if(term_i) FANSI_reset_state(state);
  else *state = ref;

  substr_calc_points(&state_start, &state_stop, i, start, stop, rnd_i, term_i);

  // - Extract ---------------------------------------------------------------

  SEXP res;
  int empty_string = state_stop.pos_byte == state_start.pos_byte;
  if(!(empty_string && term_i) && stop >= start) {
    // Measure/Write loop (see src/write.c), this is adapted from wrap.c
    const char * err_msg = "Writing substring";
    for(int k = 0; k < 2; ++k) {
      if(!k) FANSI_reset_buff(buff);
      else   FANSI_size_buff(buff);

      // Use bridge do write opening styles to account for potential carry and
      // similar in the input state.
      FANSI_W_bridge(buff, *state, state_start, norm_i, i, err_msg);

      // Actual string, remember state_stop.pos_byte is one past what we need
      int stop = state_stop.pos_byte;
      FANSI_W_normalize_or_copy(buff, state_start, norm_i, stop, i, err_msg);

      // And turn off CSI styles if needed
      if(term_i) FANSI_W_sgr_close(buff, state_stop.sgr, norm_i, i);
      if(term_i) FANSI_W_url_close(buff, state_stop.url, i);
    }
    cetype_t chr_type = CE_NATIVE;
    if(state_stop.utf8 > state_start.pos_byte) chr_type = CE_UTF8;
    res = FANSI_mkChar(*buff, chr_type, i);
  } else {
    res = R_BlankString;
  }
  // Carry handled in `substr_extract`
  *state = state_stop;
  return res;
}
// Extract Substring (`substr_ctl`)

static SEXP substr_extract(
  SEXP x, SEXP start, SEXP stop, SEXP carry,
  struct FANSI_state state, struct FANSI_buff * buff,
  int rnd_i, int norm_i, int term_i
) {
  R_xlen_t len = XLENGTH(x);
  if(len < 1) error("Internal Error: must have at least one value.");
  int prt = 0;
  SEXP res = PROTECT(allocVector(STRSXP, len)); ++prt;

  // Prep for carry.  ref needed to account for state changes that occur outside
  // of the substring so the next element knows to apply them.
  int carry_i = STRING_ELT(carry, 0) != NA_STRING;
  struct FANSI_state state_carry, state_ref;
  state_carry = state;
  if(carry_i) {
    state_carry.string = CHAR(STRING_ELT(carry, 0));
    state_carry.arg = "carry";
    while(state_carry.string[state_carry.pos_byte]) {
      FANSI_read_next(&state_carry, 0, 1);
  } }
  // For first iteration, we assume carry is active.  It may not be the case in
  // subsequent iteratins where the end of the string may not be captured in the
  // substring.
  state_ref = state_carry;
  int * start_i = INTEGER(start);
  int * stop_i = INTEGER(stop);

  for(R_xlen_t i = 0; i < len; ++i) {
    FANSI_interrupt(i);
    FANSI_state_reinit(&state, x, i);
    int start_ii = start_i[i];
    int stop_ii = stop_i[i];
    if(
      STRING_ELT(x, i) == NA_STRING ||
      start_ii == NA_INTEGER || stop_ii == NA_INTEGER
    ) {
      SET_STRING_ELT(res, i, NA_STRING);
    } else {
      if(start_ii < 1) start_ii = 1;
      // We do the full process even if stop_ii < start_ii for consistency
      if(carry_i) {
        state.sgr = state_carry.sgr;
        state.url = state_carry.url;
      }
      SET_STRING_ELT(
        res, i,
        substr_one(
          &state, state_ref, buff, i,
          start_ii, stop_ii, rnd_i, norm_i, term_i
      ) );
      if(carry_i) {
        state_ref = state;
        while(state.string[state.pos_byte]) FANSI_read_next(&state, i, 1);
        state_carry.sgr = state.sgr;
        state_carry.url = state.url;
  } } }
  UNPROTECT(prt);
  return res;
}
// Replace Substring (`substr_ctl<-`)

static SEXP substr_replace(
  SEXP x, SEXP start, SEXP stop, SEXP value, SEXP carry,
  struct FANSI_state state, struct FANSI_buff * buff,
  int rnd_i, int norm_i, int term_i
) {
  R_xlen_t len = XLENGTH(x);
  if(len < 1)
    error("Internal Error: must have at least one value.");    // nocov
  if(XLENGTH(value) != len)
    error("Internal Error: `x` and `value` length mismatch."); // nocov

  int prt = 0;
  SEXP res = PROTECT(allocVector(STRSXP, len)); ++prt;
  int * start_i = INTEGER(start);
  int * stop_i = INTEGER(stop);
  int carry_i = STRING_ELT(carry, 0) != NA_STRING;
  int write_ld, write_tr, write_md;
  write_ld = write_tr = write_md = 0;

  struct FANSI_state st_x0, st_x1, st_x2, st_xlast, st_xref,
                     st_v0, st_v1, st_vlast, st_vref;
  st_x2 = st_x1 = st_xlast = st_xref =
    st_v0 = st_v1 = st_vlast = st_vref = state;
  st_v0.arg = st_v1.arg = st_vlast.arg = st_vref.arg = "value";

  for(R_xlen_t i = 0; i < len; ++i) {
    // - Setup -----------------------------------------------------------------

    FANSI_interrupt(i);
    // Reference state is the end of the last written substring.
    if(!term_i && write_md) st_vref = st_v1;
    // initial init done in caller as really all we're doing is setting all the
    // fixed parameters in the object.
    st_x0 = st_x2;
    FANSI_state_reinit(&st_x0, x, i);
    FANSI_state_reinit(&st_v0, value, i);
    if(carry_i == 1) {
      st_x0.sgr = st_xlast.sgr;
      st_x0.url = st_xlast.url;
      st_v0.sgr = st_vlast.sgr;
      st_v0.url = st_vlast.url;
    }
    st_x2 = st_x1 = st_x0;

    // Remember that start/stop are 1 indexed, but bounds are "zero" indexed.
    // (they are just a measure of width accrued prior to point).
    int start_ii, stop_ii, stop_ld, start_tr, start_v, stop_v;
    start_ii = start_i[i];
    stop_ii = stop_i[i];
    // ld = lead, tr = trail
    stop_ld = start_ii < 1 ? 1 : start_ii;
    start_tr = stop_ii + 1;
    start_v = 1;
    stop_v = start_tr - stop_ld;

    // - Compute Lengths -------------------------------------------------------

    // For `x` the start and end points we're computing are for the string we're
    // REMOVING, so logic is a bit weird.
    //
    // In: substr_ctl(x, 3, 5) <- v
    //   1 2 3 4 5 6
    //  .x.x.x.x.x.x.x.x      .v.v.v
    //     |       |           |   |
    //     x0      x1          v0  v1

    // We want the values in _x0 and _x1, using _x2 as dummy in the first call
    // as we don't care about preserving the very start point.  We don't need to
    // read the entire trail, we just need to check it has at leas one "char".
    substr_calc_points(&st_x2, &st_x0, i, 1, stop_ld - 1, rnd_i, term_i);
    // trail of x, we just need the first character position since
    // we're going to copy the entire string after that.
    substr_calc_points(&st_x1, &st_x2, i, start_tr, start_tr, rnd_i, term_i);
    // More straightforward for the `value`
    substr_calc_points(&st_v0, &st_v1, i, start_v, stop_v, rnd_i, term_i);

    int size_x = st_x1.pos_width - st_x0.pos_width;
    int size_v = st_v1.pos_width - st_v0.pos_width;

    // Adjustments if substring does not fit exactly
    if(size_v > size_x) {
      // Reduce the size of the replacement by 1 to see if it fits that way.
      // Implicit here is that the widths are all either 1 or 2, which may not
      // be the case when a \U code is rendered (but that should only be for
      // EncodeString, which we don't care about).
      stop_v = stop_v - 1;
      substr_calc_points(&st_v0, &st_v1, i, start_v, stop_v, rnd_i, term_i);
      size_v = st_v1.pos_width - st_v0.pos_width;
      if(size_v > size_x) {
        // Reduction didn't work, collapse size_v;
        st_v1 = st_v0;
        size_v = 0;
        stop_v = start_v - 1;
    } }
    if (size_v < size_x) {
      // Scooch forward trail by gap amount if replacement is too small
      struct FANSI_state st_x11, st_x21;
      st_x11 = st_x0;
      int start_tr2 = st_x1.pos_width - (size_x - size_v) + 1;
      substr_calc_points(
        &st_x11, &st_x21, i, start_tr2, start_tr, rnd_i, term_i
      );
      int size_x1 = st_x11.pos_width - st_x0.pos_width;
      /*
      Rprintf("x0 %d %d x1 %d %d x11 %d %d\n",
        st_x0.pos_byte,st_x0.pos_width,
        st_x1.pos_byte,st_x1.pos_width,
        st_x11.pos_byte,st_x11.pos_width
      );
      Rprintf("size x %d x1 %d v %d\n", size_x, size_x1, size_v);
      Rprintf("start_tr %d start_tr2 %d\n", start_tr, start_tr2);
      */
      if(size_v <= size_x1) {
        start_tr = start_tr2;
        // We explicilty do not reset _x2 as that doesn't move
        st_x1 = st_x11;
    } }
    // - Extract String --------------------------------------------------------

    // Which portions of the strings are we actually writing out?
    // tr = trail, ld = lead, md = mid (`value`).  The semantics are
    // "what you selected gets replaced", so that lead/trail sequences are left
    // in unless you clearly select past them on each side.
    write_ld = start_ii > 0 && (st_x0.pos_raw > 0 || !term_i);
    write_md =
      (stop_ii >= start_ii) && (st_v1.pos_byte > st_v0.pos_byte || !term_i);
    write_tr = (start_tr - 1) <= st_x2.pos_width; // stop_ii isn't scooched

    // We've done unncessary work for the cases where the things to write are
    // empty, but easier to keep things straight this way.
    const char * err_msg = "Replacing substring";
    const char * x1_string = "";

    if(write_md) {
      for(int k = 0; k < 2; ++k) {
        if(!k) FANSI_reset_buff(buff);
        else   FANSI_size_buff(buff);

        // Lead
        if(write_ld) {
          FANSI_W_MCOPY(buff, st_x0.string, st_x0.pos_byte);
          if(term_i) FANSI_W_sgr_close(buff, st_x0.sgr, norm_i, i);
          if(term_i) FANSI_W_url_close(buff, st_x0.url, i);
        }
        // Replacement
        if(write_md) {
          FANSI_W_bridge(buff, st_vref, st_v0, norm_i, i, err_msg);
          FANSI_W_normalize_or_copy(
            buff, st_v0, norm_i, st_v1.pos_byte, i, err_msg
          );
          if(term_i) FANSI_W_sgr_close(buff, st_v1.sgr, norm_i, i);
          if(term_i) FANSI_W_url_close(buff, st_v1.url, i);
        }
        // Trailing string
        if(write_tr) {
          if(write_ld && !term_i) st_xref = st_x0;
          FANSI_W_bridge(buff, st_xref, st_x1, norm_i, i, err_msg);
          int x_bytes = strlen(st_x1.string) - st_x1.pos_byte;
          x1_string = st_x1.string + st_x1.pos_byte;
          FANSI_W_MCOPY(buff, x1_string, x_bytes);
        }
      }
      int has_utf8 = st_x1.utf8 || st_v1.utf8;
      if(carry_i) {
        st_xlast = st_x1;
        while(st_xlast.string[st_xlast.pos_byte])
          FANSI_read_next(&st_xlast, i, 0);
        st_vlast = st_v1;
        while(st_vlast.string[st_vlast.pos_byte])
          FANSI_read_next(&st_vlast, i, 0);
        has_utf8 = has_utf8 || st_xlast.utf8 > st_x0.pos_byte;
      } else {
        has_utf8 = has_utf8 || has_8bit(x1_string);
      }
      cetype_t chr_type = CE_NATIVE;
      if(has_utf8) chr_type = CE_UTF8;
      SET_STRING_ELT(res, i, FANSI_mkChar(*buff, chr_type, i));
    } else {
      SET_STRING_ELT(res, i, STRING_ELT(x, i));
    }
  }
  UNPROTECT(prt);
  return res;
}

SEXP FANSI_substr(
  SEXP x,
  SEXP start, SEXP stop,
  SEXP value,
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
        x, start, stop, carry, state, &buff, rnd_i, norm_i, term_i
      );
    } else {
      res = substr_replace(
        x, start, stop, value, carry, state, &buff, rnd_i, norm_i, term_i
      );
    }
  } else res = allocVector(STRSXP, len);

  FANSI_release_buff(&buff, 1);
  UNPROTECT(prt);
  return res;
}
