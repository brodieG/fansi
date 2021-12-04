/*
Copyright (C) 2021 Brodie Gaslam

This file is part of "fansi - ANSI Control Sequence Aware String Functions"

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 2 or 3 of the License.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Go to <https://www.r-project.org/Licenses/GPL-2> for a copy of the license.
*/

#ifndef _FANSI_H
#define _FANSI_H

#include <stdint.h>
#include <R.h>
#include <Rinternals.h>
#include <Rversion.h>
#include "fansi-cnst.h"
#include "fansi-struct.h"

// macros

#define FANSI_ADD_INT(x, y) FANSI_add_int((x), (y), __FILE__, __LINE__)

// Set or Get range of bits with offset `offset` and range `mask`.
//
// We could in theory have a table with all this info, but then would it be
// slower to look up than providing all the constants in line?  The advantage of
// the table is we can auto-fill it an be less likely to make mistakes in
// specifying the constants, and then we have fewer shifts.
//
// See fansi-cnst.h, FANSI_SET_*, FANSI_STAT_*.
//
// Needed by state.c, read.c, state.c
//
// @param offset how many bits to offset into the unsigned int.
// @param bits the mask right shifted by the offset.

#define FANSI_GET_RNG(x, offset, bits) (((x) >> (offset)) & (bits))
#define FANSI_SET_RNG(x, offset, bits, val)                         \
  (((x) & ~((bits) << (offset))) | ((val) << (offset)))

#define FANSI_GET_ERR(x)                                            \
  FANSI_GET_RNG((x), FANSI_STAT_ERR_START, FANSI_STAT_ERR_ALL)

// - Internal funs -----------------------------------------------------------

SEXP FANSI_process(
  SEXP input, SEXP term_cap, SEXP ctl, struct FANSI_buff *buff
);
SEXP FANSI_tabs_as_spaces(
  SEXP vec, SEXP tab_stops, struct FANSI_buff * buff, SEXP warn,
  SEXP term_cap, SEXP ctl
);
SEXP FANSI_sort_chr(SEXP x);

void FANSI_find_ctl(
  struct FANSI_state * state, R_xlen_t i, const char * arg
);
void FANSI_reset_pos(struct FANSI_state * state);
void FANSI_reset_width(struct FANSI_state * state);
void FANSI_reset_state(struct FANSI_state * state);

void FANSI_check_chrsxp(SEXP x, R_xlen_t i);

int FANSI_term_cap_as_int(SEXP term_cap);
unsigned int FANSI_ctl_as_int(SEXP ctl);

void FANSI_init_buff(struct FANSI_buff * buff, const char * fun);
#define FANSI_INIT_BUFF(A) FANSI_init_buff((A), __func__)
size_t FANSI_size_buff0(struct FANSI_buff * buff, int size);
size_t FANSI_size_buff(struct FANSI_buff * buff);
int FANSI_release_buff(struct FANSI_buff * buff, int warn);
void FANSI_check_buff(struct FANSI_buff buff, R_xlen_t i, int strict);
void FANSI_reset_buff(struct FANSI_buff * buff);

struct FANSI_state FANSI_state_init(
  SEXP strsxp, SEXP warn, SEXP term_cap, R_xlen_t i
);
void FANSI_state_reinit(
  struct FANSI_state * state, SEXP x, R_xlen_t i
);
struct FANSI_state FANSI_state_init_full(
  SEXP strsxp, SEXP warn, SEXP term_cap, SEXP allowNA, SEXP keepNA,
  SEXP width, SEXP ctl, R_xlen_t i
);
struct FANSI_state FANSI_state_init_ctl(
  SEXP strsxp, SEXP warn, SEXP ctl, R_xlen_t i
);
int FANSI_sgr_active(struct FANSI_sgr sgr);
int FANSI_url_active(struct FANSI_url url);
int FANSI_sgr_comp_color(struct FANSI_sgr target, struct FANSI_sgr current);
struct FANSI_sgr FANSI_sgr_setdiff(
  struct FANSI_sgr old, struct FANSI_sgr new, int mode
);
struct FANSI_sgr FANSI_sgr_intersect(
  struct FANSI_sgr old, struct FANSI_sgr new
);
int FANSI_url_comp(struct FANSI_url target, struct FANSI_url current);

void FANSI_read_next(
  struct FANSI_state * state, R_xlen_t i, const char * arg
);
int FANSI_add_int(int x, int y, const char * file, int line);

// "Writing" functions
void FANSI_W_sgr(
  struct FANSI_buff * buff, struct FANSI_sgr sgr, int normalize,
  int enclose, R_xlen_t i
);
void FANSI_W_url(
  struct FANSI_buff * buff, struct FANSI_url url, int normalize, R_xlen_t i
);
void FANSI_W_sgr_close(
  struct FANSI_buff * buff, struct FANSI_sgr sgr, int normalize, R_xlen_t i
);
void FANSI_W_url_close(
  struct FANSI_buff * buff, struct FANSI_url url, R_xlen_t i
);
void FANSI_W_close(
  struct FANSI_buff * buff, struct FANSI_format fmt, int normalize, R_xlen_t i
);
int FANSI_W_copy(
  struct FANSI_buff * buff, const char * tmp, R_xlen_t i, const char * err_msg
);
int FANSI_W_mcopy(
  struct FANSI_buff * buff, const char * tmp, int tmp_len, R_xlen_t i,
  const char * err_msg
);
void FANSI_W_fill(
  struct FANSI_buff * buff, const char tmp, int times,
  R_xlen_t i, const char * err_msg
);
int FANSI_W_bridge(
  struct FANSI_buff * buff, struct FANSI_state end,
  struct FANSI_state restart, int normalize, R_xlen_t i,
  const char * err_msg
);
int FANSI_W_normalize(
  struct FANSI_buff * buff, struct FANSI_state *state,
  int stop, R_xlen_t i, const char * err_msg, const char * arg
);
int FANSI_W_normalize_or_copy(
  struct FANSI_buff *buff, struct FANSI_state state, int norm_i,
  int stop, R_xlen_t i, const char * err_msg, const char * arg
);

// Macro versions require `len`, `i`, and `err_msg` defined in scope.
#define FANSI_W_COPY(A, B) FANSI_W_copy((A), (B), i, err_msg)
#define FANSI_W_MCOPY(A, B, C) FANSI_W_mcopy(\
  (A), (B), (C), i, err_msg)
#define FANSI_W_FILL(A, B, C) FANSI_W_fill(\
  (A), (B), (C), i, err_msg)

// Utilities
int FANSI_seek_ctl(const char * x);
void FANSI_print(char * x);
void FANSI_print_state(struct FANSI_state x);
void FANSI_print_sgr(struct FANSI_sgr s);
void FANSI_interrupt(R_xlen_t i);
intmax_t FANSI_ind(R_xlen_t i);
SEXP FANSI_mkChar0(char * start, char * end, cetype_t enc, R_xlen_t i);
SEXP FANSI_mkChar(struct FANSI_buff buff, cetype_t enc, R_xlen_t i);
void FANSI_check_limits();

int FANSI_check_append(int cur, int extra, const char * msg, R_xlen_t i);
void FANSI_check_append_err(const char * msg, R_xlen_t i);

void FANSI_val_args(SEXP x, SEXP norm, SEXP carry);
char * FANSI_state_as_chr(
  struct FANSI_buff *buff, struct FANSI_state state, int normalize, R_xlen_t i
);
struct FANSI_state FANSI_carry_init(
  SEXP carry, SEXP warn, SEXP term_cap, SEXP ctl
);
int FANSI_is_tf(SEXP x);

// - Compatibility -----------------------------------------------------------

// R_nchar does not exist prior to 3.2.2, so we sub in this dummy

#if defined(R_VERSION) && R_VERSION >= R_Version(3, 2, 2)
#else
typedef enum {Bytes, Chars, Width} nchar_type;
int R_nchar(SEXP string, nchar_type type_,
            Rboolean allowNA, Rboolean keepNA, const char* msg_name);
#endif

#endif  /* _FANSI_H */
