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

void state_at_end(
  struct FANSI_state *state, R_xlen_t i, const char * arg
) {
  FANSI_read_all(state, i, arg);
  FANSI_reset_pos(state);
}

SEXP FANSI_state_at_end_ext(
  SEXP x, SEXP warn, SEXP term_cap, SEXP ctl, SEXP norm, SEXP carry,
  SEXP arg, SEXP allowNA
) {
  FANSI_val_args(x, norm, carry);
  if(TYPEOF(arg) != STRSXP || XLENGTH(arg) != 1)
    error("Internal Error: bad `arg` arg."); // nocov

  const char * arg_chr;
  // NA case for testing warn generation with no arg.
  if(STRING_ELT(arg, 0) == NA_STRING) arg_chr = NULL;
  else arg_chr = CHAR(STRING_ELT(arg, 0));  // should be ASCII
  int prt = 0;
  int normalize = asInteger(norm);

  // Read-in any pre-existing state to carry
  int do_carry = STRING_ELT(carry, 0) != NA_STRING;
  SEXP carry_string;
  if(do_carry) { carry_string = PROTECT(carry); ++prt; }
  else { carry_string = PROTECT(mkString("")); ++prt; }

  SEXP R_true = PROTECT(ScalarLogical(1)); ++prt;
  SEXP R_zero = PROTECT(ScalarInteger(0)); ++prt;
  SEXP keepNA, width;
  keepNA = R_true;
  width = R_zero; // character width mode

  struct FANSI_state state_prev = FANSI_state_init_full(
    carry_string, warn, term_cap, allowNA, keepNA, width,
    ctl, (R_xlen_t) 0
  );
  state_at_end(&state_prev, 0, "carry");

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
    } else FANSI_state_reinit(&state, x, i);

    if(do_carry) state.fmt.sgr = state_prev.fmt.sgr;

    state_at_end(&state, i, arg_chr);
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
  state_at_end(&state_carry, (R_xlen_t) 0, "carry");
  UNPROTECT(prt);
  return state_carry;
}
/*
 * Compute Sequences to Transition from `end` to `restart`
 *
 * Very similar logic to used in `normalize`, intended to  handle the
 * `substr_ctl(..., carry=TRUE, terminate=FALSE)` case.
 */
int FANSI_W_bridge(
  struct FANSI_buff * buff,
  struct FANSI_state end,
  struct FANSI_state restart,
  int normalize,
  R_xlen_t i,
  const char * err_msg
) {
  // Fairly different logic for normalize vs not because in normalize we can
  // rely on an e.g. color change to change a pre-existing color, whereas in
  // non-normalize we close explicitly and need to re-open.
  struct FANSI_sgr to_close, to_open;
  to_close = FANSI_sgr_setdiff(end.fmt.sgr, restart.fmt.sgr, !normalize);
  to_open = FANSI_sgr_setdiff(restart.fmt.sgr, end.fmt.sgr, !normalize);

  if(!normalize) {
    // We need to combine all the style tokens, which means we need to write the
    // enclosing \033[ and m separately depending on what needs to change.
    // renew are all the things that exist both in old and new that need to be
    // re-opened after an all-close.
    struct FANSI_sgr renew;
    // If we close everything, we need to re-open the stuff that was active
    renew = FANSI_sgr_intersect(end.fmt.sgr, restart.fmt.sgr);

    int active_open = FANSI_sgr_active(to_open);
    int active_close = FANSI_sgr_active(to_close);

    if(active_open && active_close) {
      FANSI_W_COPY(buff, "\033[0;");
      FANSI_W_sgr(buff, renew, normalize, 0, i);
      FANSI_W_sgr(buff, to_open, normalize, 0, i);
      if(buff->buff) *((buff->buff) - 1) = 'm';
    } else if (active_close) {
      FANSI_W_COPY(buff, "\033[0;");
      FANSI_W_sgr(buff, renew, normalize, 0, i);
      if(buff->buff) *((buff->buff) - 1) = 'm';
    } else {
      FANSI_W_sgr(
        buff, FANSI_sgr_setdiff(restart.fmt.sgr, end.fmt.sgr, 0),
        normalize, 1, i
    );}
  } else {
    FANSI_W_sgr_close(buff, to_close, normalize, i);
    FANSI_W_sgr(buff, to_open, normalize, 1, i);
  }
  // Any changed URLs will need to be written (empty URL acts as a closer
  // so simpler than with SGR).
  if(FANSI_url_comp(end.fmt.url, restart.fmt.url)) {
    if(!FANSI_url_active(restart.fmt.url))
      FANSI_W_url_close(buff, end.fmt.url, i);
    FANSI_W_url(buff, restart.fmt.url, i);
  }
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
    // Do not warn here, so arg does not matter
    if(!i) {
      st_end = FANSI_state_init(end, warn, term_cap, i);
      state_at_end(&st_end, i, NULL);
      st_rst = FANSI_state_init(restart, warn, term_cap, i);
      state_at_end(&st_rst, i, NULL);
    } else {
      FANSI_state_reinit(&st_end, end, i);
      state_at_end(&st_end, i, NULL);
      FANSI_state_reinit(&st_rst, restart, i);
      state_at_end(&st_rst, i, NULL);
    }
    FANSI_reset_buff(&buff);

    // Measure
    const char * err_msg = "Bridging state";
    int len = FANSI_W_bridge(&buff, st_end, st_rst, normalize, i, err_msg);
    if(len < 0) continue;

    // Write
    FANSI_size_buff(&buff);
    FANSI_W_bridge(&buff, st_end, st_rst, normalize, i, err_msg);

    SEXP reschr = PROTECT(FANSI_mkChar(buff, CE_NATIVE, i));
    SET_STRING_ELT(res, i, reschr);
    UNPROTECT(1);
  }
  FANSI_release_buff(&buff, 1);
  UNPROTECT(2);
  return res;
}


