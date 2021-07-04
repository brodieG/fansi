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
    state = FANSI_read_next(state, i);
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

  struct FANSI_state state_prev = FANSI_state_init_full(
    carry_string, warn, term_cap, R_true, R_true,
    R_zero, // character width mode
    ctl, (R_xlen_t) 0
  );
  state_prev = state_at_end(state_prev, 0);

  R_xlen_t len = XLENGTH(x);
  struct FANSI_buff buff;
  FANSI_INIT_BUFF(&buff);

  SEXP res = PROTECT(allocVector(STRSXP, len)); ++prt;

  for(R_xlen_t i = 0; i < len; ++i) {
    FANSI_interrupt(i);

    struct FANSI_state state = FANSI_state_init_full(
      x, warn, term_cap, R_true, R_true, R_zero, ctl, i
    );
    if(do_carry) state.sgr = state_prev.sgr;

    state = state_at_end(state, i);
    char * state_chr = FANSI_state_as_chr(&buff, state, normalize, i);

    SEXP reschr = PROTECT(
      FANSI_mkChar(state_chr, state_chr + strlen(state_chr), CE_NATIVE, i)
    );
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

  // Read-in any pre-existing state to carry
  struct FANSI_state state_carry = FANSI_state_init_full(
    carry_string, warn, term_cap, R_true, R_true,
    R_zero, // normal char, we don't care about width
    ctl, (R_xlen_t) 0
  );
  state_carry = state_at_end(state_carry, (R_xlen_t) 0);
  UNPROTECT(prt);
  return state_carry;
}


