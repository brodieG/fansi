/*
 * Copyright (C) 2020  Brodie Gaslam
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
// nocov start
// this is a temporarily un-used implementation, but we may re-use it later

SEXP FANSI_strsplit(SEXP x, SEXP warn, SEXP term_cap) {
  error("This code is disabled currently.");
  warning("Handle UTF8");
  // UTF8 is a little tricky here, we're now using read_next, but we don't
  // really need it, so we could maybe have a version without read next that
  // doesn't need to translate to UTF8.

  if(
    TYPEOF(x) != VECSXP || TYPEOF(warn) != LGLSXP ||
    TYPEOF(term_cap) != INTSXP
  )
    error("Internal Error: invalid arguments; contact maintainer.");

  R_xlen_t x_len = xlength(x);

  // Save two spots on protect stack, one for the VECSXP, and then one for each
  // STRSXP we need to create

  PROTECT_INDEX ipx;
  PROTECT_WITH_INDEX(x, &ipx);

  SEXP res_vec = x;

  int sgr_in_vec, sgr_in_str;
  sgr_in_vec = sgr_in_str = 0;
  struct FANSI_buff buff = {.len=0};
  char * buff_track = buff.buff;

  for(R_xlen_t i = 0; i < x_len; ++i) {

    sgr_in_str = 0;

    FANSI_interrupt(i);
    SEXP string = VECTOR_ELT(x, i);
    if(TYPEOF(string) != STRSXP)
      // nocov start
      error(
        "%s%s%s",
        "Internal Error: result of `strsplit` contains ",
        type2char(TYPEOF(string)),
        " instead of expected STRSXP; contact maintainer."
      );
      // nocov end

    R_xlen_t chr_len = XLENGTH(string);
    struct FANSI_state state_prev = FANSI_state_init("", warn, term_cap);

    for(R_xlen_t j = 0; j < chr_len; ++j) {

      FANSI_interrupt(j);

      SEXP chrsxp = STRING_ELT(string, j);
      FANSI_check_chrsxp(chrsxp, j);

      if(chrsxp != NA_STRING) {
        const char * chr = CHAR(chrsxp);
        R_len_t chr_len = LENGTH(chrsxp);
        const char * chr_track = chr;

        struct FANSI_state state = FANSI_state_init(chr, warn, term_cap);

        // Read all escapes; note we only care about the state at the end of a
        // string

        while(*chr_track && (chr_track = strchr(chr_track, 0x1b))) {
          state.pos_byte = (chr_track - chr);
          state = FANSI_read_next(state);
          chr_track = chr + state.pos_byte;
        }
        int has = FANSI_state_has_style(state);
        int has_prev = FANSI_state_has_style(state_prev);
        int chr_size = 0;
        int chr_size_prev = 0;

        if(has_prev) {
          chr_size = chr_size_prev = FANSI_state_size(state_prev);
        }
        if(has_prev || has) {
          // really shouldn't overflow
          chr_size = FANSI_ADD_INT(chr_size, 4);

          if(chr_len > FANSI_int_max - chr_size) {
            error(
              "%s%s",
              "Overflow while attempting to reformat styles on split string; ",
              "this can happen if you have a sub-string close to INT_MAX long ",
              "and adding initial / and or ending SGRs would push it over ",
              "INT_MAX."
            );
          }
          chr_size += chr_len;
          FANSI_size_buff(&buff, (size_t) chr_size + 1); // + 1 for the NULL
          buff_track = buff.buff;

          if(!sgr_in_vec) {
            // re-allocate VECSXP
            sgr_in_vec = 1;
            REPROTECT(res_vec = duplicate(x), ipx);
            // Initiate buffer
          }
          if(!sgr_in_str) {
            // re-allocate STRSXP
            sgr_in_str = 1;
            SEXP string_cpy = PROTECT(duplicate(string));
            SET_VECTOR_ELT(res_vec, i, string_cpy);
            UNPROTECT(1);
            string = string_cpy;
          }
          if(has_prev) {
            FANSI_csi_write(buff_track, state_prev, chr_size_prev);
            buff_track += chr_size_prev;
          }
          memcpy(buff_track, chr, chr_len);
          buff_track += chr_len;
          memcpy(buff_track, "\033[0m", 4);
          buff_track += 4;
          *buff_track = 0;  // not strictly necessary

          SEXP chrsxp_new =
            PROTECT(mkCharLenCE(buff.buff, chr_size, getCharCE(chrsxp)));

          SET_STRING_ELT(string, j, chrsxp_new);
          UNPROTECT(1);

          if(has) state_prev = state;
  } } } }
  UNPROTECT(1);
  return res_vec;
}
// nocov end
