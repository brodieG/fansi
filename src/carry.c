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

static struct FANSI_state state_at_end(
  struct FANSI_state state, R_xlen_t i
) {
  while(state.string[state.pos_byte]) {
    state = FANSI_read_next(state, i, 1);
  }
  state = FANSI_reset_pos(state);
  return state;
}

SEXP FANSI_state_at_end_ext(
  SEXP x, SEXP warn, SEXP term_cap, SEXP ctl, SEXP norm, SEXP carry
) {
  FANSI_val_args(x, norm, carry);

  int prt = 0;
  int normalize = asInteger(norm);

  // Read-in any pre-existing state to carry
  int do_carry = STRING_ELT(carry, 0) != NA_STRING;
  SEXP carry_string;
  if(do_carry) { carry_string = PROTECT(carry); ++prt; }
  else { carry_string = PROTECT(mkString("")); ++prt; }

  SEXP R_true = PROTECT(ScalarLogical(1)); ++prt;
  SEXP R_zero = PROTECT(ScalarInteger(0)); ++prt;
  SEXP allowNA, keepNA, width;
  allowNA = keepNA = R_true;
  width = R_zero; // character width mode

  struct FANSI_state state_prev = FANSI_state_init_full(
    carry_string, warn, term_cap, allowNA, keepNA, width,
    ctl, (R_xlen_t) 0
  );
  state_prev = state_at_end(state_prev, 0);

  R_xlen_t len = XLENGTH(x);
  struct FANSI_buff buff;
  FANSI_INIT_BUFF(&buff);

  SEXP res = PROTECT(allocVector(STRSXP, len)); ++prt;
  struct FANSI_state state;

  for(R_xlen_t i = 0; i < len; ++i) {
    FANSI_interrupt(i);
    if(!i) {
      state = FANSI_state_init_full(
        x, warn, term_cap, allowNA, keepNA, width, ctl, i
      );
    } else {
      state = FANSI_state_reinit(state, x, i);
    }
    if(do_carry) state.sgr = state_prev.sgr;

    state = state_at_end(state, i);
    FANSI_state_as_chr(&buff, state, normalize, i);

    SEXP reschr = PROTECT(FANSI_mkChar(buff, CE_NATIVE, i));
    SET_STRING_ELT(res, i, reschr);
    UNPROTECT(1);
    state_prev = state;
  }
  FANSI_release_buff(&buff, 1);
  UNPROTECT(prt);
  return res;
}

struct FANSI_state FANSI_carry_init(
  SEXP carry, SEXP warn, SEXP term_cap, SEXP ctl
) {
  int prt = 0;
  int do_carry = STRING_ELT(carry, 0) != NA_STRING;
  SEXP carry_string;
  if(do_carry) {
    carry_string = PROTECT(carry); ++prt;
  } else {
    carry_string = PROTECT(mkString("")); ++prt;
  }
  SEXP R_true = PROTECT(ScalarLogical(1)); ++prt;
  SEXP R_zero = PROTECT(ScalarInteger(0)); ++prt;
  SEXP allowNA, keepNA, width;
  allowNA = keepNA = R_true;
  width = R_zero; // character width mode

  // Read-in any pre-existing state to carry
  struct FANSI_state state_carry = FANSI_state_init_full(
    carry_string, warn, term_cap, allowNA, keepNA,
    width, ctl, (R_xlen_t) 0
  );
  state_carry = state_at_end(state_carry, (R_xlen_t) 0);
  UNPROTECT(prt);
  return state_carry;
}


/*
 * Compute Sequences to Transition from `end` to `restart`
 *
 * Very similar logic to used in `normalize`, intended to  handle the
 * `substr_ctl(..., carry=TRUE, terminate=FALSE)` case.
 */

static int bridge(
  struct FANSI_buff * buff,
  struct FANSI_state end,
  struct FANSI_state restart,
  int normalize,
  R_xlen_t i
) {
  struct FANSI_sgr to_close = FANSI_sgr_setdiff(end.sgr, restart.sgr);

  // Any prior open styles not overriden by new one need to be closed
  // One option is to always normalize the close, but ended up preferring to be
  // consistent with the use of `normalize` as we can't actually know how the
  // closed style was closed.
  FANSI_W_sgr_close(buff, to_close, normalize, i);

  // Open all new styles (an alternative would be to open only newly open ones)
  FANSI_W_sgr(buff, restart.sgr, normalize, i);

  // Any changed URLs will need to be written (empty URL acts as a closer
  // so simpler than with SGR).
  if(FANSI_url_comp(end.url, restart.url))
    FANSI_W_url(buff, restart.url, normalize, i);

  return buff->len;
}

SEXP FANSI_bridge_state_ext(SEXP end, SEXP restart, SEXP term_cap, SEXP norm) {
  if(TYPEOF(end) != STRSXP)
    error("Internal Error: `end` must be character vector");  // nocov
  if(TYPEOF(restart) != STRSXP)
    error("Internal Error: `restart` must be character vector");  // nocov
  if(XLENGTH(end) != XLENGTH(restart))
    error("Internal Error: `end` and `restart` unequal lengths");  // nocov
  if(TYPEOF(norm) != LGLSXP || XLENGTH(norm) != 1)
    error("Argument `normalize` should be TRUE or FALSE.");  // nocov

  int normalize = asInteger(norm);
  struct FANSI_buff buff;
  FANSI_INIT_BUFF(&buff);

  R_xlen_t x_len = XLENGTH(end);
  SEXP res = PROTECT(allocVector(STRSXP, x_len)); // WRE docs this is init'ed

  // We'll already have warned about these at some point
  SEXP warn =  PROTECT(ScalarInteger(0));
  struct FANSI_state st_end, st_rst;

  for(R_xlen_t i = 0; i < x_len; ++i) {
    FANSI_interrupt(i);
    if(STRING_ELT(end, i) == NA_STRING || STRING_ELT(restart, i) == NA_STRING)
      continue;
    if(
      getCharCE(STRING_ELT(end, i)) != CE_NATIVE ||
      getCharCE(STRING_ELT(restart, i)) != CE_NATIVE
    ) {
      // nocov start
      error(
        "Internal Error: non-native encoding at index[%jd].",
        FANSI_ind(i)
      );
      // nocov end
    }
    if(!i) {
      st_end = state_at_end(FANSI_state_init(end, warn, term_cap, i), i);
      st_rst = state_at_end(FANSI_state_init(restart, warn, term_cap, i), i);
    } else {
      st_end = state_at_end(FANSI_state_reinit(st_end, end, i), i);
      st_rst = state_at_end(FANSI_state_reinit(st_rst, restart, i), i);
    }
    FANSI_reset_buff(&buff);

    // Measure
    int len = bridge(&buff, st_end, st_rst, normalize, i);
    if(len < 0) continue;

    // Write
    FANSI_size_buff(&buff);
    bridge(&buff, st_end, st_rst, normalize, i);

    SEXP reschr = PROTECT(FANSI_mkChar(buff, CE_NATIVE, i));
    SET_STRING_ELT(res, i, reschr);
    UNPROTECT(1);
  }
  FANSI_release_buff(&buff, 1);
  UNPROTECT(2);
  return res;
}


