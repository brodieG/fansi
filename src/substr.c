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

static int substr_range(
  struct FANSI_state * state_start, struct FANSI_state * state_stop,
  R_xlen_t i, int start, int stop, int rnd_i, int term_i,
  const char * arg
) {
  *state_stop = *state_start;
  int start0 = start - 1;     // start wants the beginning byte

  // - Start Point -----------------------------------------------------------

  struct FANSI_state state_tmp;
  int overshoot = !(rnd_i == RND_START || rnd_i == RND_BOTH);
  int mode = 0;               // start wants the beginning byte

  // Always consume leading controls, unless starting before string, in which
  // case don't consume them (except specials, which are re-output so
  // semantically equivalent to consume them).
  if(start0 < 0 && stop > 0) {
    state_tmp = *state_start;
    FANSI_read_next(&state_tmp, i, arg);
    if(state_tmp.status & STAT_SPECIAL) *state_start = state_tmp;
    state_start->status |= state_tmp.status & STAT_WARNED;
  } else {
    FANSI_read_until(state_start, start0, overshoot, term_i, mode, i, arg);
  }
  // - End Point -------------------------------------------------------------

  *state_stop = *state_start;
  overshoot = (rnd_i == RND_STOP || rnd_i == RND_BOTH);
  mode = 1;                   // stop wants the last byte
  FANSI_read_until(state_stop, stop, overshoot, term_i, mode, i, arg);

  if(state_start->pos.x > state_stop->pos.x) {
    error("Internal Error: bad `stop` state 2."); // nocov
  }
  return state_stop->pos.w - state_start->pos.w;
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
  else state->fmt = ref.fmt;
  const char * arg  = "x";
  substr_range(
    &state_start, &state_stop, i, start, stop, rnd_i, term_i, arg
  );
  // - Extract ---------------------------------------------------------------

  SEXP res;
  int empty_string = state_stop.pos.x == state_start.pos.x;
  if(!(empty_string && term_i) && stop > 0 && stop >= start) {
    // Measure/Write loop (see src/write.c), this is adapted from wrap.c
    const char * err_msg = "Writing substring";
    for(int k = 0; k < 2; ++k) {
      if(!k) FANSI_reset_buff(buff);
      else   FANSI_size_buff(buff);

      // Use bridge do write opening styles to account for potential carry and
      // similar in the input state.
      FANSI_W_bridge(buff, *state, state_start, norm_i, i, err_msg);

      // Actual string, remember state_stop.pos.x is one past what we need
      int stop = state_stop.pos.x;
      FANSI_W_normalize_or_copy(
        buff, state_start, norm_i, stop, i, err_msg, arg
      );
      // And turn off CSI styles if needed
      if(term_i) FANSI_W_close(buff, state_stop.fmt, norm_i, i);
    }
    cetype_t chr_type = CE_NATIVE;
    if(state_stop.utf8 > state_start.pos.x) chr_type = CE_UTF8;
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
    FANSI_read_all(&state_carry, 0, "carry");
  }
  // assume carry is active for 1st  iteration.  It may not be the case in later
  // iteratins where the end of the string may not be captured in the substring.
  state_ref = state_carry;
  int * start_i = INTEGER(start);
  int * stop_i = INTEGER(stop);
  int any_na = 0;
  const char * arg = "x";

  for(R_xlen_t i = 0; i < len; ++i) {
    FANSI_interrupt(i);
    FANSI_state_reinit(&state, x, i);
    int start_ii = start_i[i];
    int stop_ii = stop_i[i];
    if(
      STRING_ELT(x, i) == NA_STRING ||
      start_ii == NA_INTEGER || stop_ii == NA_INTEGER ||
      (any_na && carry_i)
    ) {
      any_na = any_na || STRING_ELT(x, i) == NA_STRING;
      SET_STRING_ELT(res, i, NA_STRING);
    } else {
      // We do the full process even if stop_ii < start_ii for consistency
      if(carry_i) state.fmt = state_carry.fmt;
      SET_STRING_ELT(
        res, i,
        substr_one(
          &state, state_ref, buff, i,
          start_ii, stop_ii, rnd_i, norm_i, term_i
    ) );}
    if(carry_i && STRING_ELT(x, i) != NA_STRING) {
      state_ref = state;
      FANSI_read_all(&state, i, arg);
      state_carry.fmt = state.fmt;
  } }
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
  int write_ld, write_tr, write_md, any_na;
  write_ld = write_tr = write_md = any_na = 0;

  struct FANSI_state st_x0, st_x1, st_x2, st_xlast, st_xref,
                     st_v0, st_v1, st_vlast, st_vref;
  st_x2 = st_x1 = st_xlast = st_xref =
    st_v0 = st_v1 = st_vlast = st_vref = state;

  for(R_xlen_t i = 0; i < len; ++i) {
    FANSI_interrupt(i);
    if(
      STRING_ELT(x, i) == NA_STRING || STRING_ELT(value, i) == NA_STRING ||
      (carry_i && any_na)
    ) {
      any_na = 1;
      SET_STRING_ELT(res, i, NA_STRING);
      continue;
    }
    // - Setup -----------------------------------------------------------------

    // Reference state is the end of the last written substring.
    if(!term_i && write_md) st_vref = st_v1;
    // initial init done in caller as really all we're doing is setting all the
    // fixed parameters in the object.
    st_x0 = st_x2;
    FANSI_state_reinit(&st_x0, x, i);
    FANSI_state_reinit(&st_v0, value, i);
    if(carry_i == 1) {
      st_x0.fmt = st_xlast.fmt;
      st_v0.fmt = st_vlast.fmt;
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
    substr_range(&st_x2, &st_x0, i, 1, stop_ld - 1, rnd_i, term_i, "x");
    // trail of x, we just need the first character position since
    // we're going to copy the entire string after that.
    substr_range(&st_x1, &st_x2, i, start_tr, start_tr, rnd_i, term_i, "x");
    // More straightforward for the `value`
    substr_range(&st_v0, &st_v1, i, start_v, stop_v, rnd_i, term_i, "value");

    int size_x = st_x1.pos.w - st_x0.pos.w;
    int size_v = st_v1.pos.w - st_v0.pos.w;

    // Adjustments if substring does not fit exactly
    if(size_v > size_x) {
      // Reduce the size of the replacement by 1 to see if it fits that way.
      // Implicit here is that the widths are all either 1 or 2, which may not
      // be the case when a \U code is rendered (but that should only be for
      // EncodeString, which we don't care about).
      stop_v = stop_v - 1;
      substr_range(&st_v0, &st_v1, i, start_v, stop_v, rnd_i, term_i, "value");
      size_v = st_v1.pos.w - st_v0.pos.w;
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
      int start_tr2 = st_x1.pos.w - (size_x - size_v) + 1;
      substr_range(
        &st_x11, &st_x21, i, start_tr2, start_tr, rnd_i, term_i, "x"
      );
      int size_x1 = st_x11.pos.w - st_x0.pos.w;
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
    write_ld = start_ii > 0 &&
      (st_x0.pos.w > 0 || !(st_x0.status & CTL_ALL) || !term_i);
    write_md =
      (stop_ii >= start_ii) && (st_v1.pos.x > st_v0.pos.x || !term_i);
    write_tr = (start_tr - 1) <= st_x2.pos.w; // stop_ii isn't scooched

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
          FANSI_W_MCOPY(buff, st_x0.string, st_x0.pos.x);
          if(term_i) FANSI_W_close(buff, st_x0.fmt, norm_i, i);
        }
        // Replacement
        if(write_md) {
          FANSI_W_bridge(buff, st_vref, st_v0, norm_i, i, err_msg);
          FANSI_W_normalize_or_copy(
            buff, st_v0, norm_i, st_v1.pos.x, i, err_msg, "value"
          );
          if(term_i) FANSI_W_close(buff, st_v1.fmt, norm_i, i);
        }
        // Trailing string
        if(write_tr) {
          if(write_ld && !term_i) st_xref = st_x0;
          FANSI_W_bridge(buff, st_xref, st_x1, norm_i, i, err_msg);
          int x_bytes = strlen(st_x1.string) - st_x1.pos.x;
          x1_string = st_x1.string + st_x1.pos.x;
          FANSI_W_MCOPY(buff, x1_string, x_bytes);
        }
      }
      int has_utf8 = st_x1.utf8 || st_v1.utf8;
      if(carry_i) {
        st_xlast = st_x1;
        FANSI_read_all(&st_xlast, i, "x");
        st_vlast = st_v1;
        FANSI_read_all(&st_vlast, i, "value");
        has_utf8 = has_utf8 || st_xlast.utf8 > st_x0.pos.x;
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
      case COUNT_CHARS:
      case COUNT_WIDTH:
      case COUNT_GRAPH:
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
      x, warn, term_cap, allowNA, keepNA, type, ctl, (R_xlen_t) 0
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
  PROTECT(res); ++prt;

  FANSI_release_buff(&buff, 1);
  UNPROTECT(prt);
  return res;
}
